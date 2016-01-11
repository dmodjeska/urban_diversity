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



# get and clean US population data(2010) ---------------------------------------------

# get data
us_population_file <- "SUB-EST2011-IP.csv"
if (!file.exists(us_population_file)) {
    us_population_url <-
        "http://www.census.gov/popest/data/cities/totals/2011/files/SUB-EST2011-IP.csv"
    download.file(us_population_url, us_population_file)
}

# clean data
us_population_data <- read.csv(us_population_file) %>%
    select(NAME, STNAME, CENSUS2010POP) %>%
    rename(City_Name = NAME, State_Name = STNAME,
           City_Population = CENSUS2010POP) %>%
    separate(col = City_Name, into = c("City", "Blank"),
             sep = " city| town| municipality| borough| village| unified|
             consolidated| CDP| urban| metro| [(]balance[)]") %>%
    mutate(State_Abb = state.abb[match(State_Name, state.name)],
           City_Population = as.numeric(as.character(City_Population))) %>%
    select(City, State_Abb, City_Population)

# Get and clean US result data -------------------------------------------------

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
               fancytable.Neighborhood.Diversity.Index.hide,
           City_State = fancytable.City) %>%
    select(-fancytable.Rank,
           -fancytable.Citywide.Diversity.Index,
           -fancytable.Neighborhood.Diversity.Index,
           -fancytable.Integration..Segregation.Index,
           -fancytable.Integration..Segregation.Index.hide) %>%
    separate(col = City_State, into = c("City", "State_Abb"), sep = ", ") %>%
    inner_join(us_population_data, by = c("City", "State_Abb")) %>%
    select(-City, -State_Abb)

# Combine US, Canada, and England result data ----------------------------------

all_results_data <- us_results_data %>%
    bind_rows(can_results_data) %>%
    bind_rows(eng_results_data) %>%
    mutate(Country = as.factor(Country)) %>%
    group_by(Country) %>%
    mutate(Country_Diversity_Index = mean(Citywide_Diversity_Index))

# country populations (2010) from Wikipedia
# TO DO - source these in reproducible fashion
all_results_data <- all_results_data %>%
    mutate(Country_Population = ifelse(Country == "USA", 309300000,
                                       ifelse(Country == "Canada", 34010000,
                                              ifelse(Country == "England", 62027000))))

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
        annotate(geom = "text", x = 0.63, y = 0.03,
                 label = "Based on data from FiveThirtyEight",
                 color = "gray60", size = 3.5) +
        theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
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
        geom_abline(slope = 1, linetype = "dotted")
    p1 <- p + geom_point(aes(color = Country), size = 1)
    p2 <- p1 + xlab("Citywide Diversity Index") +
        ylab("Neighborhood Diversity Index") +
        ggtitle("Citywide vs. neighborhood diversity\nin largest American, Canadian, and English cities") +
        annotate(geom = "text", x = 0.62, y = 0.05,
                 label = "Based on data from FiveThirtyEight,\nStatistics Canada, and UK's ONS",
                 color = "gray60", size = 3) +
        theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        scale_x_continuous(labels = percent, limits = c(0, 0.8)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        scale_y_continuous(labels = percent, limits = c(0, 0.8))
    print(p2)
    dev.off()
}

#------------------------------------------------------------------------------
# REGRESSION MODEL
#------------------------------------------------------------------------------


fit_citywide <- lm(log(Tracts_Diversity_Index) ~ log(Citywide_Diversity_Index),
          all_results_data)
Predict_Log <- predict(fit_citywide, se.fit = TRUE)
Predict_Fit <- data.frame(Predict_Index = exp(Predict_Log$fit))
all_results_data_2 <- all_results_data %>%
    bind_cols(Predict_Fit)

my_png("Combined_All_City_Diversity")
g <- ggplot(all_results_data_2,
            aes(x = Citywide_Diversity_Index, y = Tracts_Diversity_Index))
p <- g + geom_smooth(aes(x = Citywide_Diversity_Index, y = Predict_Index),
                col = "red", method = "loess", se = FALSE, size = 0.65) +
    geom_abline(slope = 1, linetype = "dotted")
p1 <- p + geom_point(size = 1.5)
p2 <- p1 + xlab("Citywide Diversity Index") +
    ylab("Neighborhood Diversity Index") +
    ggtitle("Combined citywide vs. neighborhood diversity\nin largest American, Canadian, and English cities") +
    annotate(geom = "text", x = 0.62, y = 0.05,
             label = "Based on data from FiveThirtyEight,\nStatistics Canada, and UK's ONS",
             color = "gray60", size = 3) +
    theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
    theme(axis.title.x = element_text(vjust= -0.25)) +
    scale_x_continuous(labels = percent, limits = c(0, 0.8)) +
    theme(axis.title.y = element_text(vjust= 0.75)) +
    scale_y_continuous(labels = percent, limits = c(0, 0.8))
print(p2)
dev.off()

#------------------------------------------------------------------------------
# SAVE SESSION INFO
#------------------------------------------------------------------------------

writeLines(capture.output(sessionInfo()), "session_info.txt")

