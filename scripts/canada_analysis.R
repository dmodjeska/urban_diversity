# ==============================================================================
# THIS FILE ANALYZES CANADA CENSUS DATA ON VISIBLE MINORITIES
# AND PRESENTS RESULTS
# ==============================================================================

do_plots <- TRUE

can_dir <- "Canada Data"
if (!file.exists(can_dir)) {
    dir.create(can_dir)
}

pub_dir <- "presentation"
if (!file.exists(pub_dir)) {
    dir.create(pub_dir)
}

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(data.table)

can_plots_dir <- "canada_exploratory_plots"
if (!file.exists(can_plots_dir)) {
    dir.create(can_plots_dir)
}

my_png <- function(dir, prefix) {
    image_file <- paste(dir, "/", prefix, ".png", sep = "")
    png(image_file)
}

#------------------------------------------------------------------------------
# GET AND LOAD BOUNDARY FILES FOR CANADIAN CITIES (CMA's)
#------------------------------------------------------------------------------

library(maptools)
library(ggmap)
library(rgeos)

# Data files are also available via manual download from the following URL:
#https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm

tract_bound_url <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gct_000b11a_e.zip"
tract_bound_prefix <- "gct_000b11a_e"
tract_dir <- paste(can_dir, tract_bound_prefix, sep = "/")
tract_bound_file <- paste(can_dir, tract_bound_prefix, "gct_000b11a_e.shp",
                          sep = "/")

# Get and uncompress boundary file for census tracts
if (!file.exists(tract_dir)) {
    tract_bound_zip <- paste(tract_dir, ".zip", sep = "")
    download.file(tract_bound_url, tract_bound_zip)
    unzip(tract_bound_zip, exdir = tract_dir)
}

# Load tract boundaries
tract_bound <- readShapePoly(tract_bound_file)

#--------------------------------

subdiv_bound_url <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gcsd000b11a_e.zip"
subdiv_bound_prefix <- "gcsd000b11a_e"
subdiv_dir <- paste(can_dir, subdiv_bound_prefix, sep = "/")
subdiv_bound_file <- paste(can_dir, subdiv_bound_prefix, "gcsd000b11a_e.shp",
                           sep = "/")

# Get and uncompress boundary file for census subdivisions
# (i.e., municipalities)
if (!file.exists(subdiv_dir)) {
    subdiv_bound_zip <- paste(subdiv_dir, ".zip", sep = "")
    download.file(subdiv_bound_url, subdiv_bound_zip)
    unzip(subdiv_bound_zip, exdir = subdiv_dir)
}

# Load subdivision boundaries
subdiv_bound <- readShapePoly(subdiv_bound_file)

#------------------------------------------------------------------------------
# VISUALIZE DIVERSITY IN EACH CANADIAN CITY (CMA)
#------------------------------------------------------------------------------

library(ggplot2)
library(scales)
library(grid)
bar_color <- "gray10"

# install.packages("gpclib")
# gpclibPermit()

can_histogram_dir <- paste(can_plots_dir, "histograms", sep = "/")
if (!file.exists(can_histogram_dir)) {
    dir.create(can_histogram_dir)
}

can_boxplot_dir <- paste(can_plots_dir, "boxplots", sep = "/")
if (!file.exists(can_boxplot_dir)) {
    dir.create(can_boxplot_dir)
}

can_maps_dir <- "canada_city_maps"
if (!file.exists(can_maps_dir)) {
    dir.create(can_maps_dir)
}

plot_city_descriptions <- function(cma, mean_index,
                                   cma_tracts_div_indices) {
    plot_title <- sprintf("Neighborhood Diversity Indices in %s\n", cma$Area_Name)

    # Visualize city tracts as histogram with probability density
    my_png(can_histogram_dir, cma$Area_Name)
    g <- ggplot(cma_tracts_div_indices, aes(x = Diversity_Index))
    p <- g + geom_histogram(aes(y = ..density..), binwidth = 0.035,
                            colour = "white", fill = bar_color) +
        geom_density(alpha = .2, fill = "#FF6666") +
        xlab("Diversity Index") + ylab("Density") + ggtitle(plot_title) +
        geom_vline(xintercept = mean_index, color = "red",
                   linetype = "dotted", size = 1) +
        theme_grey()
    print(p)
    dev.off()

    # Visualize city tracts diversity indices as box plot
    my_png(can_boxplot_dir, cma$Area_Name)
    g <- ggplot(cma_tracts_div_indices,
                aes(x = factor(0), y = Diversity_Index))
    p <- g + geom_boxplot() + xlab("") +
        ylab("Diversity Index") +
        ggtitle(plot_title) + theme_grey()
    print(p)
    dev.off()
}

map_tracts_div <- function(cma, city_div_data) {
    # Subset tract boundaries for this city and combine with diversity data
    city_bound <- tract_bound[substr(tract_bound$CMAUID, 1, 3) == cma$CMA,]
    city_bound_df <- fortify(city_bound, region = "CTUID") %>%
        rename(Tract = id) %>%
        mutate(Tract = as.numeric(Tract))
    city_map_data <- city_bound_df %>%
        inner_join(city_div_data, by = "Tract")
    missingMapData <- city_bound_df %>%
        anti_join(city_div_data, by = "Tract")

    # Subset municipality boundaries for this city
    city_subdiv_bound <- subdiv_bound[!is.na(subdiv_bound$CMAUID) &
                                          subdiv_bound$CMAUID == cma$CMA,]
    city_subdiv_boundDf <- fortify(city_subdiv_bound, region = "CSDUID")

    # get geographical coordinates to position annotation
    min_long <- min(city_map_data$long)
    max_long <- max(city_map_data$long)
    min_lat <- min(city_map_data$lat)
    max_lat <- max(city_map_data$lat)

    # Make plot
    my_png(can_maps_dir, cma$Area_Name)
    plot_title <- sprintf("Neighborhood Diversity in %s", cma$Area_Name)
    m1 <- ggplot(city_map_data)
    m2 <- m1 + geom_polygon(aes(x = long, y = lat, group = group,
                                fill = Diversity_Index),
                            data = city_map_data) +
        geom_polygon(aes(x = long, y = lat, group = group),
                     fill = "gray75", data = missingMapData) +
        coord_map() +
        scale_fill_distiller(palette = "Blues", labels = percent,
                             breaks = pretty_breaks(n = 10))
    m3 <- m2 + geom_path(aes(x = long, y = lat, group = group),
                         city_subdiv_boundDf, color = "gray", size = 0.5)
    m4 <- m3  + theme_nothing(legend = TRUE) +
        labs(title = plot_title, fill = "") +
        guides(fill = guide_legend(reverse = TRUE)) +
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
        theme(panel.background = element_rect(colour = "gray90")) +
        annotate(geom = "text", x = max_long - 0.3, y = min_lat,
                 label = "Based on data from Statistics Canada",
                 color = "gray60", size = 3.5)

    print(m4)

    dev.off()
}

#------------------------------------------------------------------------------
# CALCULATE DIVERSITY FOR CANADIAN CITIES (CMA's)
#------------------------------------------------------------------------------

# Helper function to calculate the citywide diversity index for one city
calc_city_index <- function(cma_data) {

    index <- cma_data %>%
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

# Helper function to calculate the tracts diversity index for one city
# This function also makes a boxplot, a histogram, and a map
# (while the tracts data is already subset to one city)
calc_tracts_index <- function(cma_data) {
    cma <- cma_data %>%
        slice(1) %>%
        select(CMA, Area_Name)

    # Subset, reshape, group, and summarize the tracts data
    tracts_data <- cma_data %>%
        group_by(Group, Tract) %>%
        summarize(Population = sum(Population))

    # Calculate tracts diversity index
    cma_tracts_div_indices <- tracts_data %>%
        group_by(Tract) %>%
        mutate(Tract_Population = sum(Population)) %>%
        mutate(Group_Proportion = Population / Tract_Population) %>%
        mutate(Other_Proportion = 1 - Group_Proportion) %>%
        mutate(Diversity_Index = Group_Proportion * Other_Proportion) %>%
        summarize(Diversity_Index = sum(Diversity_Index))
    index <- mean(cma_tracts_div_indices$Diversity_Index)

    plot_city_descriptions(cma, index, cma_tracts_div_indices)
    map_tracts_div(cma, cma_tracts_div_indices)
    return(index)
}

# Helper function to calculate the diversity statistics for one city
my_calc_indices <- function(cma) {
    # Subset city data, extract needed variables, and calculate diversity
    clean_city_data <- can_anal_data %>%
        filter(CMA == cma)
    citywide_div_index <- calc_city_index(clean_city_data)
    cma_tracts_div_index <- calc_tracts_index(clean_city_data)

    # Store CMA name (and work around encoding limitations with readers)
    cma_name <- clean_city_data %>%
        slice(1) %>%
        select(Area_Name)

    city_indices <- data.table(CMA = cma, Area_Name = as.character(cma_name),
                               Citywide_Diversity_Index = citywide_div_index[[1]],
                               Tracts_Diversity_Index = cma_tracts_div_index)

    return(city_indices)
}

# Load analytic data
if (!file.exists("canada_processing.csv") | !file.exists("canada_cma.csv")) {
    stop("Analytic data files must be in the working directory.")
}
can_anal_data <- read_csv("canada_processing.csv")
top_cma_codes <- read_csv("canada_cma.csv")

# Calculate diversity for each top CMA
can_div_indices <- lapply(top_cma_codes$CMA, my_calc_indices) %>%
    bind_rows() %>%
    arrange(Citywide_Diversity_Index) %>%
    select(-CMA)

#------------------------------------------------------------------------------
# VISUALIZE CANADIAN CITIES (as CMA's)
#------------------------------------------------------------------------------

if (do_plots) {
    # from http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
    ggplot_colors <- function(n = 6, h = c(0, 360) + 15) {
        if ((diff(h) %% 360) < 1) h[2] <- h[2] - (360 / n)
        hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
    }

    # Create lookup table for groups and one-letter abbreviations
    groups <- c("South Asian", "East Asian", "Black", "Latin American",
                "White", "West Asian", "Other")
    groups_to_abbrevs <- c("SA", "EA", "B", "LA", "W", "WA", "O")
    group_abbrevs <- data.table(Group = groups,
                                Group_Letter = groups_to_abbrevs)

    # Merge lookup tables and add city populations
    can_data_plot <- can_anal_data %>%
        inner_join(top_cma_codes, by = "CMA") %>%
        inner_join(group_abbrevs, by = "Group") %>%
        group_by(CMA) %>%
        mutate(Cma_Population = sum(Population)) %>%
        mutate(Group_Proportion = Population/Cma_Population)

    # Visualize group populations by city
    my_png(can_plots_dir, "Canada_City_Groups")
    g <- ggplot(can_data_plot, aes(Group_Letter, Group_Proportion,
                                   fill = Group_Letter))
    p <- g + geom_bar(stat = "identity") +
        facet_wrap(~ Area_Name, ncol = 4) +
        xlab("Visible Minority") + ylab("Percentage of Population") +
        ggtitle("Visible minority percentages in 10 most populous Canadian cities\n") + theme_grey() +
        annotate(geom = "text", x = 2, y = 0.04,
                 label = "Based on data from Statistics Canada",
                 color = "gray60", size = 3.5) +
        scale_fill_manual(values = ggplot_colors(5),
                          breaks = group_abbrevs$Group_Letter,
                          labels = group_abbrevs$Group,
                          name = "Legend") +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        theme(plot.title = element_text(size = 13)) +
        scale_y_continuous(labels = percent, limits = c(0, 1))
    print(p)
    dev.off()

    # Visualize group populations in Toronto
    my_png(pub_dir, "Toronto_City_Groups")
    toronto_data_plot <- can_data_plot %>%
        filter(Area_Name == "Toronto")
    g <- ggplot(toronto_data_plot, aes(Group_Letter, Group_Proportion,
                                   fill = Group_Letter))
    p <- g + geom_bar(stat = "identity") +
        xlab("Visible Minority") + ylab("Percentage of Population") +
        ggtitle("Visible minority percentages in Toronto\n") +
        theme_grey(base_size = 14) +
        annotate(geom = "text", x = 2.00, y = 0.85,
                 label = "Based on data from Statistics Canada",
                 color = "gray60", size = 3.5) +
        scale_fill_manual(values = ggplot_colors(5),
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
    my_png(pub_dir, "Canada_City_Diversity")
    g <- ggplot(can_div_indices,
                aes(Citywide_Diversity_Index, Tracts_Diversity_Index))
    p1 <- g +  geom_smooth(method = "loess", se = FALSE, size = 1, color = "red") +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted")
    p2 <- p1 + geom_point(size = 3) +
        geom_text(aes(label = Area_Name), size = 4,
                  hjust = c(-0.25, -0.25, 1.25, -0.25, 1.25, -0.25, 1.25, -0.25, 1.25, 1.25),
                  vjust = c(0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
    p3 <- p2 + xlab("Citywide Diversity Index") +
        ylab("Neighborhood Diversity Index") +
        ggtitle("Citywide vs. neighborhood diversity\nin 10 most populous Canadian cities") +
        annotate(geom = "text", x = 0.54, y = 0.025,
                 label = "Based on data from Statistics Canada",
                 color = "gray60", size = 3.5) +
        theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        scale_x_continuous(labels = percent, limits = c(0, 0.7)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        scale_y_continuous(labels = percent, limits = c(0, 0.7))
    print(p3)
    dev.off()
}

#------------------------------------------------------------------------------
# SAVE DATA SUMMARIES
#------------------------------------------------------------------------------

# Save Canadian city diversity indices as a data file and as an HTML file

out_file <- paste(".", "Canada_City_Diversity.csv", sep = "/")
write.csv(can_div_indices, out_file, row.names = FALSE)

library(xtable)
table <- xtable(can_div_indices, caption = "Canadian City Diversity Indices")
out_file <- paste(pub_dir, "Canada_City_Diversity.html", sep = "/")
print.xtable(table, type = "html", file = out_file)

