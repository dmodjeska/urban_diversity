# This analysis extends one by Nate Silver at FiveThirtyEight
#http://fivethirtyeight.com/features/the-most-diverse-cities-are-often-the-most-segregated/

can_dir <- "Canada Data"
if (!file.exists(can_dir)) {
    dir.create(can_dir)
}

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(data.table)

#------------------------------------------------------------------------------
# MAP FROM STATISTICS CANADA VISIBLE MINORITIES TO 5-CLASS VISIBLE MINORITIES
#------------------------------------------------------------------------------

# Create lookup table for sub-groups and groups
# Combine East Asian and White/Arab groups for summary purposes
minorities <- c("South Asian", "Chinese", "Black", "Filipino",
                   "Latin American", "Arab", "Southeast Asian",
                   "West Asian", "Korean", "Japanese",
                   "Visible minority, n.i.e.", "Multiple visible minorities",
                   "Not a visible minority")
minorities_to_groups <- c("South Asian", "East Asian", "Black", "East Asian",
                          "Other", "White", "East Asian",
                          "Other", "East Asian", "East Asian",
                          "Other", "Other",
                          "White")
groups <- data.table(Description = minorities, Group = minorities_to_groups)

#------------------------------------------------------------------------------
# GET AND CLEAN CANADIAN DATA (n.b. Toronto is a CMA in this data set)
#------------------------------------------------------------------------------

# Data files are available only via manual download from the following URL:
# http://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/prof/details/download-telecharger/comprehensive/comp-csv-tab-nhs-enm.cfm?Lang=E
can_census_zip <- paste(can_dir, "99-004-XWE2011001-401_CSV.ZIP", sep = "/")
can_census_dir <- paste(can_dir, "99-004-XWE2011001-401_CSV", sep = "/")
census_file_2 <- paste(can_dir, "98-316-XWE2011001-401.CSV", sep = "/")

if (!file.exists(can_census_dir) | !file.exists(census_file_2)) {
    stop("Error: The directory with Canadian census data must be in the
             Canada Data directory in the working directory.")
}

# Helper function to load and clean the data for one province
province_classes <- rep("character", 14)
my_load_data <- function(filename) {
    province_data <- fread(filename, colClasses = province_classes) %>%
        filter(Topic == "Visible minority population") %>%
        select(Geo_Code, CMA_CA_Name, Characteristic, Total) %>%
        rename(Tract = Geo_Code, Area_Name = CMA_CA_Name,
               Description = Characteristic, Population = Total) %>%
        mutate(Population = as.numeric(Population)) %>%
        mutate(CMA = substr(Tract, 1, 3)) %>%
        mutate(Description = str_trim(Description)) %>%
        mutate(Description = factor(Description)) %>%
        mutate(CMA = as.numeric(CMA))

    return(province_data)
}

fileNames <- list.files(can_census_dir, pattern="*.csv", full.names=TRUE)
can_data <- lapply(fileNames, my_load_data) %>%
    bind_rows()

# Check for missing data values
if (all(colSums(is.na(can_data)) != 0)) {
    warning("Warning: Canada data has missing values.")
}

# Subset largest CMA's, down to a population of about 500K
top_cma_codes <- can_data %>%
    group_by(CMA) %>%
    summarize(Cma_Population = sum(Population)) %>%
    select(CMA, Cma_Population) %>%
    ungroup() %>%
    top_n(10, Cma_Population)
can_data_3 <- can_data %>%
    filter(CMA %in% top_cma_codes$CMA)

# Correct for encoding limitations in fread and read_csv
# Also trim hyphenated names for display purposes
montreal_data <- filter(can_data_3, CMA == 462) %>%
    mutate(Area_Name = "Montreal")
quebec_data <- filter(can_data_3, CMA == 421) %>%
    mutate(Area_Name = "Quebec")
other_data <- filter(can_data_3, (CMA != 462) & (CMA != 421)) %>%
    mutate(Area_Name = sub(" [[:punct:]] [[:alpha:]]* *[[:punct:]]* *[[:alpha:]]*$", "",
                          Area_Name))
can_data_4 <- bind_rows(montreal_data, quebec_data, other_data) %>%
    inner_join(groups, by = "Description") %>%
    select(-Description)

#------------------------------------------------------------------------------
# SAVE ANALYTIC DATA
#------------------------------------------------------------------------------

analytic_file <- "canada_processing.csv"
write.csv(can_data_4, analytic_file, row.names = FALSE)

cma_file <- "canada_cma.csv"
write.csv(top_cma_codes, cma_file, row.names = FALSE)