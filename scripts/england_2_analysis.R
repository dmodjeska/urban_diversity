# This analysis extends one by Nate Silver at FiveThirtyEight
#http://fivethirtyeight.com/features/the-most-diverse-cities-are-often-the-most-segregated/

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
# GET AND LOAD BOUNDARY FILES FOR ENGLISH CITIES (BUA's)
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
# VISUALIZE DIVERSITY IN EACH ENGLISH CITY (BUA)
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

eng_maps_dir <- "england_city_maps"
if (!file.exists(eng_maps_dir)) {
    dir.create(eng_maps_dir)
}

plot_city_descriptions <- function(area_name, mean_index,
                                 bua_tracts_div_index) {
    plot_title <- sprintf("Neighborhood Diversity Indices in %s\n", area_name)

    # Visualize city tracts as histogram with probability density
    my_png(eng_histogram_dir, area_name)
    g <- ggplot(bua_tracts_div_index, aes(Diversity_Index))
    p <- g + geom_histogram(aes(y = ..density..), binwidth = 0.035,
                            colour = "white", fill = bar_color) +
        geom_density(alpha = .2, fill = "#FF6666") +
        xlab("Diversity Index") + ylab("Density") + ggtitle(plot_title) +
        geom_vline(xintercept = mean_index, color = "red",
                   linetype = "dotted", size = 1) +
        theme_bw()
    print(p)
    dev.off()

    # Visualize city tracts diversity indices as box plot
    my_png(eng_boxplot_dir, area_name)
    g <- ggplot(bua_tracts_div_index,
                aes(x = factor(0), y = Diversity_Index))
    p <- g + geom_boxplot(fill = "gray80") + xlab("") +
        ylab("Diversity Index") +
        ggtitle(plot_title) + theme_bw()
    print(p)
    dev.off()
}

map_tracts_div <- function(bua, bua_div_data) {
    # Subset output area (tract) boundaries for this bua
    # and combine with diversity data
    this_ward_bound <- ward_bound[ward_bound$WD11CD %in% bua_div_data$Ward,]
    this_ward_bound_df <- fortify(this_ward_bound, region = "WD11CD") %>%
        rename(Ward = id)
    this_ward_map_data <- this_ward_bound_df %>%
        inner_join(bua_div_data, by = "Ward")

    # Make plot (add margins around map itself)
    my_png(eng_maps_dir, bua)

    # shapefile uses UTM projection - no distortion at city scale
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
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
    print(m3)

    dev.off()
}

#------------------------------------------------------------------------------
# CALCULATE DIVERSITY FOR CITIES (METROPOLITAN COUNTIES
#------------------------------------------------------------------------------

# Helper function to calculate the citywide diversity index for one city
calc_city_index <- function(this_bua_data) {

    index <- this_bua_data %>%
        # group data by visible minority
        group_by(Group) %>%
        #
        # sum up the population within each minority
        summarize(Population = sum(Population)) %>%
        #
        # summarize the minority populations into a city total
        mutate(City_Population = sum(Population)) %>%
        #
        # calculate each minority's proportion within the city total
        mutate(Group_Proportion = Population / City_Population) %>%
        #
        # calculate each minority's proportion of "met others"
        mutate(Other_Proportion = 1 - Group_Proportion) %>%
        #
        # calculate partial diversity indices as each minority's
        # proportion of "met others" times the minority's proportion
        mutate(Diversity_Index = Group_Proportion * Other_Proportion) %>%
        #
        # sum each minority's partial diversity index into a city total
        summarize(Diversity_Index = sum(Diversity_Index))

    return(index)
}

# Helper function to calculate the wards diversity index for one city
# This function also makes a boxplot, a histogram, and a map
# (while the wards data is already subset to one city)
calcWardsIndex <- function(area, this_bua_data) {

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

    plot_city_descriptions(area, index, bua_wards_div_indices)
    map_tracts_div(area, bua_wards_div_indices)
    return(index)
}

# Helper function to calculate the diversity statistics for one city
my_calc_indices <- function(area) {
    # Subset city data and exward needed variables
    cleanBuaData <- eng_anal_data %>%
        filter(Area == area) %>%
        select(Ward, Group, Population)

    # Calculate indices
    buawide_div_Index <- calc_city_index(cleanBuaData)
    bua_wards_div_Index <- calcWardsIndex(area, cleanBuaData)
    cityIndices <- data.table(Area = area,
                              Areawide_Diversity_Index = buawide_div_Index[[1]],
                              Wards_Diversity_Index = bua_wards_div_Index)
    return(cityIndices)
}

# Calculate diversity statistics for cities
area_names = unique(eng_anal_data$Area)
eng_div_indices <- lapply(area_names, my_calc_indices) %>%
    bind_rows() %>%
    select(Area, Areawide_Diversity_Index, Wards_Diversity_Index) %>%
    arrange(Areawide_Diversity_Index)

#------------------------------------------------------------------------------
# VISUALIZE ENGLISH CITIES (AS BUA'S)
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

    # Merge lookup tables and add city populations
    eng_anal_data <- eng_anal_data %>%
        inner_join(group_abbrevs, by = "Group") %>%
        group_by(Area) %>%
        mutate(Bua_Population = sum(Population)) %>%
        mutate(Group_Proportion = Population/Bua_Population)

    # Visualize group populations by city
    my_png(pub_dir, "England_City_Groups")
    g <- ggplot(eng_anal_data, aes(Group_Letter, Group_Proportion,
                                   fill = Group_Letter))
    p <- g + geom_bar(stat = "identity") +
        facet_wrap(~ Area, ncol = 4) +
        xlab("Visible Minority") + ylab("Percentage of  Population") +
        ggtitle("Visible minority percentages in 10 largest English cities\n") + theme_bw() +
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

    # Visualize city diversity vs. neighborhood diversity
    my_png(pub_dir, "England_City_Diversity")
    g <- ggplot(eng_div_indices,
                aes(Areawide_Diversity_Index, Wards_Diversity_Index))
    p <- g + geom_point(size = 2) +
        geom_text(aes(label = Area,
                      hjust = rep(c(1.25,-0.25),
                                  length.out = nrow(eng_div_indices)),
                      vjust = rep(0.5, length.out = nrow(eng_div_indices))),
                  size = 4)
    p2 <- p + geom_smooth(method = "loess", se = FALSE) +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted")
    p3 <- p2 + xlab("Citywide Diversity Index") +
        ylab("Neighborhood Diversity Index") +
        ggtitle("Citywide vs. neighborhood diversity indices\nfor 10 largest English cities") +
        theme_bw() + theme(plot.title = element_text(size = 12, vjust = 1)) +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        scale_x_continuous(labels = percent, limits = c(0, 0.8)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        scale_y_continuous(labels = percent, limits = c(0, 0.8))
    print(p3)
    dev.off()
}

#------------------------------------------------------------------------------
# SAVE DATA SUMMARIES
#------------------------------------------------------------------------------

# Save English city diversity indices as a data file and as an HTML file

out_file <- paste(".", "England_City_Diversity.csv", sep = "/")
write.csv(eng_div_indices, out_file, row.names = FALSE)

library(xtable)
table <- xtable(eng_div_indices, caption = "English City Diversity Indices")
out_file <- paste(pub_dir, "England_City_Diversity.html", sep = "/")
print.xtable(table, type = "html", file = out_file)
