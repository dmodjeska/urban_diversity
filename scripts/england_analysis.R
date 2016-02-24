# ==============================================================================
# THIS FILE ANALYZES UK CENSUS DATA ON VISIBLE MINORITIES
# AND PRESENTS RESULTS
# ==============================================================================

do_plots <- TRUE

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(data.table)

pub_dir <- "presentation"
if (!file.exists(pub_dir)) {
    dir.create(pub_dir)
}

eng_dir <- "England Data"
if (!file.exists(eng_dir)) {
    dir.create(eng_dir)
}

eng_plots_dir <- "england_exploratory_plots"
if (!file.exists(eng_plots_dir)) {
    dir.create(eng_plots_dir)
}

# Load analytic data
if (!file.exists("england_processing.csv")) {
    stop("Analytic data file must be in the working directory.")
}
eng_anal_data <- read_csv("england_processing.csv")

#------------------------------------------------------------------------------
# GET AND LOAD BOUNDARY FILES FOR BRITISH METROS (BUA's)
#------------------------------------------------------------------------------

# This data file is only available via manual download from the following URL:
# http://www.ons.gov.uk/ons/external-links/social-media/g-m/2011-census-merged-ward-boundaries---generalised--20-metres---clipped-to-the-coastline.html
# The preceding URL can be reached by clicking on the link below:
# Link label: "Generalised (20 metres), clipped to the coastline"
# Link URL: http://www.ons.gov.uk/ons/guide-method/geography/products/census/spatial/2011/index.html

library(maptools)
library(ggmap)
library(rgeos)

ward_bound_prefix <- "Wards_(E+W)_2011_Boundaries_(Generalised_Clipped)"
ward_dir <- paste(eng_dir, ward_bound_prefix, sep = "/")
ward_bound_file <- paste(eng_dir, ward_bound_prefix, "WD_DEC_2011_EW_BGC.shp",
                       sep = "/")

# Get and uncompress boundary file for census wards
if (!file.exists(ward_dir)) {
    ward_bound_zip <- paste(ward_dir, ".zip", sep = "")
    unzip(ward_bound_zip, exdir = ward_dir)
}

# Load ward boundaries
ward_bound <- readShapePoly(ward_bound_file)

#------------------------------------------------------------------------------
# VISUALIZE DIVERSITY IN EACH BRITISH CITY (BUA)
#------------------------------------------------------------------------------

library(ggplot2)
library(scales)
library(grid)
bar_color <- "gray10"

# install.packages("gpclib")
# gpclibPermit()

# Helper function to open a PNG file
my_png <- function(dir, prefix) {
    imageFile <- paste(dir, "/", prefix, ".png", sep = "")
    png(imageFile)
}

eng_histogram_dir <- paste(eng_plots_dir, "histograms", sep = "/")
if (!file.exists(eng_histogram_dir)) {
    dir.create(eng_histogram_dir)
}

eng_boxplot_dir <- paste(eng_plots_dir, "boxplots", sep = "/")
if (!file.exists(eng_boxplot_dir)) {
    dir.create(eng_boxplot_dir)
}

eng_maps_dir <- "england_metro_maps"
if (!file.exists(eng_maps_dir)) {
    dir.create(eng_maps_dir)
}


plot_metro_descriptions <- function(metro_name, mean_index,
                                 bua_tracts_div_index) {
    plot_title <- sprintf("Neighborhood Diversity Indices in %s\n", metro_name)

    # Visualize metro tracts as histogram with probability density
    my_png(eng_histogram_dir, metro_name)
    g <- ggplot(bua_tracts_div_index, aes(Diversity_Index))
    p <- g + geom_histogram(aes(y = ..density..), binwidth = 0.035,
                            colour = "white", fill = bar_color) +
        geom_density(alpha = .2, fill = "#FF6666") +
        xlab("Diversity Index") + ylab("Density") + ggtitle(plot_title) +
        geom_vline(xintercept = mean_index, color = "red",
                   linetype = "dotted", size = 1) +
        theme_grey()
    print(p)
    dev.off()

    # Visualize metro tracts diversity indices as box plot
    my_png(eng_boxplot_dir, metro_name)
    g <- ggplot(bua_tracts_div_index,
                aes(x = factor(0), y = Diversity_Index))
    p <- g + geom_boxplot() + xlab("") +
        ylab("Diversity Index") +
        ggtitle(plot_title) + theme_grey()
    print(p)
    dev.off()
}

map_tracts_div <- function(bua, bua_div_data) {
    # Subset output area (tract) boundaries for this BUA
    # and combine with diversity data
    this_ward_bound <- ward_bound[ward_bound$WD11CD %in% bua_div_data$Ward,]
    this_ward_bound_df <- fortify(this_ward_bound, region = "WD11CD") %>%
        rename(Ward = id)
    this_ward_map_data <- this_ward_bound_df %>%
        inner_join(bua_div_data, by = "Ward")

    # Make plot (add margins around map itself)
    my_png(eng_maps_dir, bua)

    # get geographical coordinates to position annotation
    min_long <- min(this_ward_map_data$long)
    max_long <- max(this_ward_map_data$long)
    min_lat <- min(this_ward_map_data$lat)
    max_lat <- max(this_ward_map_data$lat)

    # shapefile uses UTM projection - no distortion at metro scale
    plot_title <- sprintf("Neighborhood Diversity in %s\n", bua)
    m1 <- ggplot(this_ward_map_data) + scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0))
    m2 <- m1 + geom_polygon(aes(x = long, y = lat, group = group,
                                fill = Diversity_Index), color = "gray50",
                            size = 0.1, data = this_ward_map_data) +
        scale_fill_distiller(palette = "Blues", labels = percent,
                             breaks = pretty_breaks(n = 10))
    m3 <- m2 + theme_nothing(legend = TRUE) +
        labs(title = plot_title, fill = "") +
        guides(fill = guide_legend(reverse = TRUE)) +
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
        theme(panel.background = element_rect(colour = "gray90")) +
        annotate(geom = "text", x = max_long - 28000, y = min_lat + 2000,
                 label = "Based on data from UK's Office for National Statistics",
                 color = "gray60", size = 3.5)
    print(m3)

    dev.off()
}

#------------------------------------------------------------------------------
# CALCULATE DIVERSITY FOR METROS (METROPOLITAN COUNTIES)
#------------------------------------------------------------------------------

# Helper function to calculate the metrowide diversity index for one metro
calc_metro_index <- function(this_bua_data) {

    index <- this_bua_data %>%
        # group data by visible minority
        group_by(Group) %>%
        #
        # sum up the population within each minority
        summarize(Population = sum(Population)) %>%
        #
        # summarize the minority populations into a metro total
        mutate(Metro_Population = sum(Population)) %>%
        #
        # calculate each minority's proportion within the metro total
        mutate(Group_Proportion = Population / Metro_Population) %>%
        #
        # calculate each minority's proportion of "met others"
        mutate(Other_Proportion = 1 - Group_Proportion) %>%
        #
        # calculate partial diversity indices as each minority's
        # proportion of "met others" times the minority's proportion
        mutate(Diversity_Index = Group_Proportion * Other_Proportion) %>%
        #
        # sum each minority's partial diversity index into a metro total
        summarize(Diversity_Index = sum(Diversity_Index))

    return(index)
}

# Helper function to calculate the wards diversity index for one metro
# This function also makes a boxplot, a histogram, and a map
# (while the wards data is already subset to one metro)
calcWardsIndex <- function(metro, this_bua_data) {

    # Subset, reshape, group, and summarize the wards data
    wards_data <- this_bua_data %>%
        group_by(Group, Ward) %>%
        summarize(Population = sum(Population))

    # Calculate wards diversity index
    bua_wards_div_indices <- wards_data %>%
        group_by(Ward) %>%
        mutate(Ward_Population = sum(Population)) %>%
        mutate(Group_Proportion = Population / Ward_Population) %>%
        mutate(Other_Proportion = 1 - Group_Proportion) %>%
        mutate(Diversity_Index = Group_Proportion * Other_Proportion) %>%
        summarize(Diversity_Index = sum(Diversity_Index))
    index <- mean(bua_wards_div_indices$Diversity_Index)

    if (do_plots) {
        plot_metro_descriptions(metro, index, bua_wards_div_indices)
    }
    map_tracts_div(metro, bua_wards_div_indices)
    return(index)
}

# Helper function to calculate the diversity statistics for one metro
my_calc_indices <- function(metro) {
    # Subset metro data and exward needed variables
    cleanBuaData <- eng_anal_data %>%
        filter(Metro == metro) %>%
        select(Ward, Group, Population)

    # Calculate indices
    buawide_div_Index <- calc_metro_index(cleanBuaData)
    bua_wards_div_Index <- calcWardsIndex(metro, cleanBuaData)
    Metro_Population = sum(cleanBuaData$Population)
    metroIndices <- data.table(Metro = metro,
                              Metrowide_Diversity_Index = buawide_div_Index[[1]],
                              Wards_Diversity_Index = bua_wards_div_Index,
                              Metro_Population = Metro_Population)
    return(metroIndices)
}

# Calculate diversity statistics for cities
metro_names = unique(eng_anal_data$Metro)
eng_div_indices <- lapply(metro_names, my_calc_indices) %>%
    bind_rows() %>%
    select(Metro, Metrowide_Diversity_Index, Wards_Diversity_Index,
           Metro_Population) %>%
    arrange(Metrowide_Diversity_Index)

#------------------------------------------------------------------------------
# VISUALIZE BRITISH cities (AS BUA'S)
#------------------------------------------------------------------------------

# Create lookup table for groups and one-letter abbreviations
group_names <- c("South Asian", "East Asian", "Black", "Latin American", "White",
                "West Asian", "Other")
groups_to_abbrevs <- c("SA", "EA", "B", "LA", "W", "WA", "O")
group_abbrevs <- data.table(Group = group_names, Group_Letter = groups_to_abbrevs)

if (do_plots) {
    # from http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
    ggplot_colours <- function(n = 6, h = c(0, 360) + 15) {
        if ((diff(h) %% 360) < 1) h[2] <- h[2] - (360 / n)
        hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
    }

    # Merge lookup tables and add metro populations
    eng_anal_data <- eng_anal_data %>%
        inner_join(group_abbrevs, by = "Group") %>%
        group_by(Metro) %>%
        mutate(Bua_Population = sum(Population)) %>%
        mutate(Group_Proportion = Population/Bua_Population)

    # Visualize group populations by metro
    my_png(eng_plots_dir, "England_Metro_Groups")
    g <- ggplot(eng_anal_data, aes(Group_Letter, Group_Proportion,
                                   fill = Group_Letter))
    p <- g + geom_bar(stat = "identity") +
        facet_wrap(~ Metro, ncol = 4) +
        xlab("Visible Minority") + ylab("Percentage of  Population") +
        ggtitle("Visible minority percentages in 10 most populous British cities\n") + theme_grey() +
        scale_fill_manual(values = ggplot_colours(5),
                          breaks = group_abbrevs$Group_Letter,
                          labels = group_abbrevs$Group,
                          name = "Legend") +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        theme(plot.title = element_text(size = 13)) +
        scale_y_continuous(labels = percent, limits = c(0, 1))
    print(p)
    dev.off()

    # Visualize group populations in London
    my_png(pub_dir, "London_Metro_Groups")
    london_anal_data <- eng_anal_data %>%
        filter(Metro == "London")
    g <- ggplot(london_anal_data, aes(Group_Letter, Group_Proportion,
                                   fill = Group_Letter))
    p <- g + geom_bar(stat = "identity") +
        xlab("Visible Minority") + ylab("Percentage of  Population") +
        ggtitle("Visible minority percentages in London\n") +
        theme_grey(base_size = 14) +
        annotate(geom = "text", x = 2.6, y = 0.85,
                 label = "Based on data from UK's Office for National Statistics",
                 color = "gray60", size = 3.5) +
        scale_fill_manual(values = ggplot_colours(5),
                          breaks = group_abbrevs$Group_Letter,
                          labels = group_abbrevs$Group,
                          name = "Legend") +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        theme(plot.title = element_text(size = 13)) +
        scale_y_continuous(labels = percent, limits = c(0, 1))
    print(p)
    dev.off()

    # Visualize metro diversity vs. neighborhood diversity
    my_png(pub_dir, "England_Metro_Diversity")
    g <- ggplot(eng_div_indices,
                aes(Metrowide_Diversity_Index, Wards_Diversity_Index))
    p1 <- g + geom_smooth(method = "loess", se = FALSE, size = 1, color = "red") +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted")
    p2 <- p1 + geom_point(size = 3) +
        geom_text(aes(label = Metro,
                      hjust = rep(c(1.25, -0.25),
                                  length.out = nrow(eng_div_indices)),
                      vjust = rep(0.5, length.out = nrow(eng_div_indices))),
                  size = 4)
    p3 <- p2 + xlab("Metrowide Diversity Index") +
        ylab("Neighborhood Diversity Index") +
        ggtitle("Metrowide vs. neighborhood diversity\nin 10 most populous British cities") +
        annotate(geom = "text", x = 0.48, y = 0.025,
                 label = "Based on data from UK's Office for National Statistics",
                 color = "gray60", size = 3.5) +
        theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        scale_x_continuous(labels = percent, limits = c(0, 0.7)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        scale_y_continuous(labels = percent, limits = c(0, 0.7))
    print(p3)
    dev.off()
}

group_names <- c("South Asian", "East Asian", "Black", "Latin American", "White",
                 "West Asian", "Other")
groups_to_abbrevs <- c("SA", "EA", "B", "LA", "W", "WA", "O")
group_abbrevs <- data.table(Group = group_names, Group_Letter = groups_to_abbrevs)
g <- ggplot(eng_anal_data, aes(Group_Letter, Group_Proportion,
                               fill = Group_Letter))

#------------------------------------------------------------------------------
# FLAG METROS WHERE BLACKS ARE A SIGNIFICANT SEGMENT OF THE POPULATION
#------------------------------------------------------------------------------

eng_black_prop = eng_anal_data %>%
    mutate(Group = gsub(" ", "_", Group)) %>%
    group_by(Metro, Group) %>%
    summarize(Total_Group_Proportion = sum(Group_Proportion)) %>%
    spread(key = Group, value = Total_Group_Proportion) %>%
    mutate(Black_Prop = (Black / (Black + White + East_Asian + South_Asian + Other))) %>%
    select(Metro, Black_Prop)
eng_div_indices2 = eng_div_indices %>%
    inner_join(eng_black_prop, by = "Metro")

#------------------------------------------------------------------------------
# SAVE DATA SUMMARIES
#------------------------------------------------------------------------------

# Save British metro diversity indices as a data file and as an HTML file

write.csv(eng_div_indices2, "England_Metro_Diversity.csv", row.names = FALSE)

library(xtable)
table <- xtable(eng_div_indices2, caption = "British Metro Diversity Indices")
out_file <- paste(pub_dir, "England_Metro_Diversity.html", sep = "/")
print.xtable(table, type = "html", file = out_file)
