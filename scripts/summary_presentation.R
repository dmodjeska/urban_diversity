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

# Load USA, Canada and England result data -------------------------------------

if (!file.exists("Canada_Metro_Diversity.csv") |
    !file.exists("England_Metro_Diversity.csv") |
    !file.exists("USA_Metro_Diversity.csv")) {
    stop("USA, Canada, and England results files must be in the working directory.")
}
can_results_data <- read_csv("Canada_Metro_Diversity.csv") %>%
    mutate(Country = "Canada") %>%
    rename(Metro = Metro_Name) %>%
    mutate(State_Abb = NA_character_)
eng_results_data <- read_csv("England_Metro_Diversity.csv") %>%
    mutate(Country = "England") %>%
    rename(Tracts_Diversity_Index = Wards_Diversity_Index) %>%
    mutate(State_Abb = NA_character_)
us_results_data <- read_csv("USA_Metro_Diversity.csv") %>%
    mutate(Country = "USA")

# Combine US, Canada, and England result data ----------------------------------

all_results_data <- us_results_data %>%
    bind_rows(can_results_data) %>%
    bind_rows(eng_results_data) %>%
    mutate(Country = as.factor(Country)) %>%
    group_by(Country) %>%
    mutate(Country_Diversity_Index = mean(Metrowide_Diversity_Index))

# Visualize cities -------------------------------------------------------------

# Helper function to open a PNG file
my_png <- function(prefix) {
    image_file <- paste(pub_dir, "/", prefix, ".png", sep = "")
    png(image_file)
}

if (do_plots) {
    library(ggplot2)
    library(scales)

    my_png("USA_Metro_Diversity")
    g <- ggplot(us_results_data,
                aes(Metrowide_Diversity_Index, Tracts_Diversity_Index))
    p <- g + geom_point(size = 2) +
        geom_smooth(method = "loess", se = FALSE, size = 1, color = "red") +
        geom_abline(slope = 1, intercept = 0, linetype = "dotted")
    p2 <- p + xlab("Metropolitan Diversity Index") +
        ylab("Neighborhood Diversity Index") +
        ggtitle("Metropolitan vs. neighborhood diversity
in 100 largest U.S. metropolitan areas") +
        annotate(geom = "text", x = 0.63, y = 0.03,
                 label = "Based on data from USA's ACS",
                 color = "gray60", size = 3.5) +
        theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
        theme(axis.title.x = element_text(vjust= -0.25)) +
        scale_x_continuous(labels = percent, limits = c(0, 0.8)) +
        theme(axis.title.y = element_text(vjust= 0.75)) +
        scale_y_continuous(labels = percent, limits = c(0, 0.8))
    print(p2)
    dev.off()

    my_png("All_Metro_Diversity")
    g <- ggplot(all_results_data,
                aes(x = Metrowide_Diversity_Index, y = Tracts_Diversity_Index,
                    color = Country))
    p <- g + geom_smooth(method = "loess", se = FALSE, size = 1) +
        geom_abline(slope = 1, linetype = "dotted")
    p1 <- p + geom_point(aes(color = Country), size = 1)
    p2 <- p1 + xlab("Metropolitan Diversity Index") +
        ylab("Neighborhood Diversity Index") +
        ggtitle("Metropolitan vs. neighborhood diversity
in largest American, Canadian, and English metropolitan areas") +
        annotate(geom = "text", x = 0.62, y = 0.05,
                 label = "Based on data from USA's ACS,
                 Statistics Canada, and UK's ONS",
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

fit_metrowide <- lm(I(Tracts_Diversity_Index) ~ I(Metrowide_Diversity_Index)
                  + log(Metro_Population) + log(Black_Prop), all_results_data)
Predict_Multi <- predict(fit_metrowide, se.fit = TRUE)
Predict_Fit <- data.frame(Predict_Index = Predict_Multi$fit)
all_results_data_2 <- all_results_data %>%
    bind_cols(Predict_Fit)
coeff <- fit_metrowide$coeff

min_metro_pop = min(all_results_data$Metro_Population)
mean_metro_pop = mean(all_results_data$Metro_Population)
max_metro_pop = max(all_results_data$Metro_Population)

min_black_prop = min(all_results_data$Black_Prop)
mean_black_prop = mean(all_results_data$Black_Prop)
max_black_prop = max(all_results_data$Black_Prop)

# plot fitted model by metro diversity and population ------

f_div <- function(x) {
        (coeff[1] + coeff[2]*(x) + coeff[3]*log(mean_metro_pop)
         + coeff[4]*log(mean_black_prop))
}

my_png("Combined_All_Metro_Diversity_by_Diversity")
g <- ggplot(all_results_data_2,
            aes(x = Metrowide_Diversity_Index, y = Tracts_Diversity_Index))
p <- g +
    stat_function(fun = f_generator_pop(min_metro_pop), color = "red", size = 0.66,
                  na.rm = TRUE) +
    geom_abline(slope = 1, linetype = "dotted")
p1 <- p + geom_point(color = "gray10", size = 1.5)
p2 <- p1 + xlab("Metropolitan Diversity Index") +
    ylab("Neighborhood Diversity Index") +
    ggtitle("Metropolitan vs. neighborhood diversity
            in largest American, Canadian, and English metropolitan areas") +
    annotate(geom = "text", x = 0.62, y = 0.05,
             label = "Based on data from USA's ACS,
             Statistics Canada, and UK's ONS",
             color = "gray60", size = 3) +
    theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
    theme(axis.title.x = element_text(vjust= -0.25)) +
    scale_x_continuous(labels = percent, limits = c(0, 0.8)) +
    theme(axis.title.y = element_text(vjust= 0.75)) +
    scale_y_continuous(labels = percent, limits = c(0, 0.8))
print(p2)
dev.off()

# plot fitted model by metro diversity and population ------

f_generator_pop <- function(pop) {
    return(function(x)
        (coeff[1] + coeff[2]*(x) + coeff[3]*log(pop) + coeff[4]*log(mean_black_prop)))
}

my_png("Combined_All_Metro_Diversity_by_Diversity_and_Population")
g <- ggplot(all_results_data_2,
            aes(x = Metrowide_Diversity_Index, y = Tracts_Diversity_Index))
p <- g +
    stat_function(fun = f_generator_pop(min_metro_pop), color = "red", size = 0.66,
                  na.rm = TRUE) +
    stat_function(fun = f_generator_pop(mean_metro_pop), color = "red", size = 0.66,
                  na.rm = TRUE) +
    stat_function(fun = f_generator_pop(max_metro_pop), color = "red", size = 0.66,
                  na.rm = TRUE) +
    geom_abline(slope = 1, linetype = "dotted")
p1 <- p + geom_point(color = "gray10", size = 1.5)
p2 <- p1 + xlab("Metropolitan Diversity Index") +
    ylab("Neighborhood Diversity Index") +
    ggtitle("Metropolitan vs. neighborhood diversity (by metro population)
in largest American, Canadian, and English metropolitan areas") +
    annotate(geom = "text", x = 0.73, y = 0.69,
             label = sprintf("Min Popul = %.01f MM",
                             round(min_metro_pop/1000000, digits = 1)),
             color = "gray45", size = 3) +
    annotate(geom = "text", x = 0.73, y = 0.58,
             label = sprintf("Avg Popul = %.01f MM",
                             round(mean_metro_pop/1000000, digits = 1)),
             color = "gray45", size = 3) +
    annotate(geom = "text", x = 0.73, y = 0.45,
             label = sprintf("Max Popul = %.01f MM",
                             round(max_metro_pop/1000000, digits = 1)),
             color = "gray45", size = 3) +
    annotate(geom = "text", x = 0.62, y = 0.05,
             label = "Based on data from USA's ACS,
             Statistics Canada, and UK's ONS",
             color = "gray60", size = 3) +
    theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
    theme(axis.title.x = element_text(vjust= -0.25)) +
    scale_x_continuous(labels = percent, limits = c(0, 0.8)) +
    theme(axis.title.y = element_text(vjust= 0.75)) +
    scale_y_continuous(labels = percent, limits = c(0, 0.8))
print(p2)
dev.off()

# plot fitted model by metro diversity and black proportion ------

f_generator_black <- function(black) {
    return(function(x)
        (coeff[1] + coeff[2]*(x) + coeff[3]*log(mean_metro_pop) + coeff[4]*log(black)))
}

my_png("Combined_All_Metro_Diversity_by_Diversity_and_Black")
g <- ggplot(all_results_data_2,
            aes(x = Metrowide_Diversity_Index, y = Tracts_Diversity_Index))
p <- g +
    stat_function(fun = f_generator_black(min_black_prop), color = "red", size = 0.66,
                  na.rm = TRUE) +
    stat_function(fun = f_generator_black(mean_black_prop), color = "red", size = 0.66,
                  na.rm = TRUE) +
    stat_function(fun = f_generator_black(max_black_prop), color = "red", size = 0.66,
                  na.rm = TRUE) +
    geom_abline(slope = 1, linetype = "dotted")
p1 <- p + geom_point(size = 1.5)
p2 <- p1 + xlab("Metropolitan Diversity Index") +
    ylab("Neighborhood Diversity Index") +
    ggtitle("Metropolitan vs. neighborhood diversity (by Black proportion)
in largest American, Canadian, and English metropolitan areas") +
    annotate(geom = "text", x = 0.76, y = 0.73,
             label = sprintf("Min Black = %d%%",
                             round(min_black_prop*100, digits = 0)),
             color = "gray45", size = 3) +
     annotate(geom = "text", x = 0.76, y = 0.62,
              label = sprintf("Avg Black = %d%%",
                              round(mean_black_prop*100, digits = 0)),
              color = "gray45", size = 3) +
     annotate(geom = "text", x = 0.76, y = 0.47,
              label = sprintf("Max Black = %d%%",
                              round(max_black_prop*100, digits = 0)),
              color = "gray45", size = 3) +
    annotate(geom = "text", x = 0.62, y = 0.05,
             label = "Based on data from USA's ACS,
             Statistics Canada, and UK's ONS",
             color = "gray60", size = 3) +
    theme_grey() + theme(plot.title = element_text(size = 12, vjust = 1)) +
    theme(axis.title.x = element_text(vjust= -0.25)) +
    scale_x_continuous(labels = percent, limits = c(0, 0.8)) +
    theme(axis.title.y = element_text(vjust= 0.75)) +
    scale_y_continuous(labels = percent, limits = c(0, 0.8))
print(p2)
dev.off()

#------------------------------------------------------------------------------
# SAVE DATA AND SESSION INFO
#------------------------------------------------------------------------------

save_data <- all_results_data %>%
    select(Country, Metro, State_Abb, Country_Diversity_Index,
           Metrowide_Diversity_Index, Tracts_Diversity_Index, Metro_Population,
           Black_Prop)
write.csv(save_data, "summary_results_data.csv", row.names = FALSE)

library(xtable)
table <- xtable(save_data,
                caption = "USA, UK, and Canada Metro Diversity Indices")
out_file <- paste(pub_dir, "Summary_Results_Data.html", sep = "/")
print.xtable(table, type = "html", file = out_file)

writeLines(capture.output(sessionInfo()), "session_info.txt")

