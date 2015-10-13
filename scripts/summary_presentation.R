# ==============================================================================
# THIS FILE PRESENTS RESULTS FROM ANALYZING US, CANADA, AND UK CENSUS DATA
# ON DIVERSITY
# ==============================================================================

do_plots <- TRUE

pub_dir <- "presentation"
if (!file.exists(pub_dir)) {
    dir.create(pub_dir)
}

library(dplyr)
library(readr)
library(tidyr)

# Load Canada and England result data ------------------------------------------

if (!file.exists("Canada_City_Diversity.csv") |
    !file.exists("England_City_Diversity.csv")) {
    stop("Results data files must be in the working directory.")
}
can_results_data <- read_csv("Canada_City_Diversity.csv") %>%
    mutate(Country = "Canada") %>%
    select(-Area_Name)
eng_results_data <- read_csv("England_City_Diversity.csv") %>%
    mutate(Country = "England") %>%
    rename(Citywide_Diversity_Index = Areawide_Diversity_Index,
           Tracts_Diversity_Index = Wards_Diversity_Index) %>%
    select(-Area)

# Get US result data -------------------------------------------------------------

# Note: USA data uses municipalities, rather than metropolitan areas
out_file <- "US_Results_Data.txt"
if (!file.exists(out_file)) {
    library(XML)
    us_city_url <-
        "http://projects.fivethirtyeight.com/mid-levels/segregation-cities/index.html"
    html <- htmlTreeParse(us_city_url, useInternalNodes = TRUE)
    us_results_data <- readHTMLTable(html)
    write.table(us_results_data, out_file, row.names = FALSE)
}

us_results_data <- read_delim(out_file, ' ') %>%
    mutate(Country = "USA") %>%
    rename(Citywide_Diversity_Index =
               fancytable.Citywide.Diversity.Index.hide,
           Tracts_Diversity_Index =
               fancytable.Neighborhood.Diversity.Index.hide) %>%
    select(-fancytable.Rank, -fancytable.City,
           -fancytable.Citywide.Diversity.Index,
           -fancytable.Neighborhood.Diversity.Index,
           -fancytable.Integration..Segregation.Index,
           -fancytable.Integration..Segregation.Index.hide)

# Combine US, Canada, and England result data ----------------------------------

all_results_data <- us_results_data %>%
    bind_rows(can_results_data) %>%
    bind_rows(eng_results_data)

# Visualize cities -------------------------------------------------------------

# Helper function to open a PNG file
my_png <- function(prefix) {
    image_file <- paste(pub_dir, "/", prefix, ".png", sep = "")
    png(image_file)
}

if (do_plots) {
    library(ggplot2)
    library(scales)

    my_png("USA_City_Diversity")
    g <- ggplot(us_results_data,
                aes(Citywide_Diversity_Index, Tracts_Diversity_Index))
    p <- g + geom_point(size = 2) +
        geom_smooth(method = "loess", se = FALSE, size = 1, color = "red") +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted")
    p2 <- p + xlab("Citywide Diversity Index") +
        ylab("Neighborhood Diversity Index") +
        ggtitle("Citywide vs. neighborhood diversity\nin 100 largest U.S. cities") +
        annotate(geom = "text", x = 0.67, y = 0.03,
                 label = "Data from FiveThirtyEight",
                 color = "gray60", size = 3.5) +
        theme_bw() + theme(plot.title = element_text(size = 12, vjust = 1)) +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        scale_x_continuous(labels = percent, limits = c(0, 0.8)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        scale_y_continuous(labels = percent, limits = c(0, 0.8))
    print(p2)
    dev.off()

    my_png("All_City_Diversity")
    g <- ggplot(all_results_data,
                aes(x = Citywide_Diversity_Index, y = Tracts_Diversity_Index,
                    color = Country))
    p <- g + geom_smooth(method = "loess", se = FALSE, size = 1) +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted")
    p1 <- p + geom_point(aes(color = Country), size = 1)
    p2 <- p1 + xlab("Citywide Diversity Index") +
        ylab("Neighborhood Diversity Index") +
        ggtitle("Citywide vs. neighborhood diversity\nin largest American, Canadian, and English cities") +
        annotate(geom = "text", x = 0.65, y = 0.05,
                 label = "Data from FiveThirtyEight,\nStatistics Canada,\nand UK's ONS",
                 color = "gray60", size = 3) +
        theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        scale_x_continuous(labels = percent, limits = c(0, 0.8)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        scale_y_continuous(labels = percent, limits = c(0, 0.8))
    print(p2)
    dev.off()
}
