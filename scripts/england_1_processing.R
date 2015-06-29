# This analysis extends one by Nate Silver at FiveThirtyEight
#http://fivethirtyeight.com/features/the-most-diverse-cities-are-often-the-most-segregated/

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(readxl)
library(data.table)

eng_dir <- "England Data"
if (!file.exists(eng_dir)) {
    dir.create(eng_dir)
}

#------------------------------------------------------------------------------
# MAP FROM ONS ETHNICITIES TO 5-CLASS VISIBLE MINORITIES
#------------------------------------------------------------------------------

# Load my mapping from English ONS ethnicities to Canadian visible minorities
# Combine East Asian and White/Arab groups for summary purposes
#http://www.statcan.gc.ca/concepts/definitions/minority01-minorite01a-eng.htm
mapping_file <- "UK_Ethnicity_Mappings.csv"
group_mapping <- data.table(read_csv(mapping_file))

#------------------------------------------------------------------------------
# GET AND LOAD ENGLAND+WALES DATA
#------------------------------------------------------------------------------

# Download census data files if not already present
eng_zip_file <- "rft-ct0010-wards.zip"
eng_zip_path <- paste(eng_dir, eng_zip_file, sep = "/")
eng_file <- paste(eng_dir, "R2_2_EW__RT__Table_CT0010__Wards_v1.xls", sep = "/")
if (!file.exists(eng_file)) {
    eng_url <- "http://www.ons.gov.uk/ons/rel/census/2011-census/key-statistics-and-quick-statistics-for-wards-and-output-areas-in-england-and-wales/rft-ct0010-wards.zip"
    download.file(eng_url, eng_zip_file)
    unzip(eng_zip_path)
}

# Load England+Wales census data
data <- data.table(read_excel(eng_file, sheet = 6, skip = 9, col_names = FALSE))

# Clean the code/name columns
names_data <- select(data, X1:X8)
setnames(names_data, t(slice(names_data, 1)))
setnames(names_data, gsub(" ", "_", names(names_data)))
names_data_2 <- slice(names_data, 5:nrow(names_data)) %>%
    select(Ward_code) %>%
    rename(Ward = Ward_code)

# Clean the ethnicity columns
persons <- select(data, X10:X104)
setnames(persons, t(slice(persons, 2)))
setnames(persons, str_trim(names(persons)))
setnames(persons, gsub(" ", "_", names(persons)))
setnames(persons, gsub(":", "", names(persons)))
persons2 <- slice(persons, 5:nrow(persons))

# Re-combine code/name and ethnicity columns, reshape data, summarize data
data_2 <- bind_cols(names_data_2, persons2)

# 2011 built-up areas to 2011 LADs and OA's - file available via manual download only
# "www.ons.gov.uk/ons/external-links/social-media/g-m/2011-built-up-areas-to-2011-lads.html"
# http://www.ons.gov.uk/ons/external-links/social-media/g-m/2011-oas-to-2011-wards--with-best-fit-percentage-indicator-.html
oa_ward_file <- paste(eng_dir, "OA11_WD11_LAD11_EW_LU.csv", sep = "/")
oa_bua_file <- paste(eng_dir, "OA11_BUASD11_BUA11_LAD11_RGN11_EW_LU.csv", sep = "/")
if (!file.exists(oa_ward_file) | !file.exists(oa_bua_file)) {
    warning("Lookup tables for built-up areas must be in the UK directory.")
}

# Aggregate and subset top built-up areas in England+Wales (down to ca. 500K)
oa_ward_col_classes <- rep("character", 9)
oa_bua_col_classes <- rep("character", 11)
oa_ward_data <- fread(oa_ward_file, sep = ',', na = "", colClasses = oa_ward_col_classes) %>%
    select(OA11CD, WD11CD) %>%
    rename(Output_Area = OA11CD) %>%
    rename(Ward = WD11CD)
oa_bua_data <- fread(oa_bua_file, sep = ',', na = "", colClasses = oa_bua_col_classes) %>%
    select(OA11CD, BUA11NM) %>%
    rename(Output_Area = OA11CD) %>%
    rename(Area = BUA11NM) %>%
    mutate(Area = sub(" BUA", "", Area)) %>%
    mutate(Area = sub("Greater ", "", Area)) %>%
    filter(!is.na(Area))
bua_data <- inner_join(oa_ward_data, oa_bua_data, by = "Output_Area") %>%
    select(-Output_Area) %>%
    distinct()
data_3 <- data_2 %>%
    inner_join(bua_data, by = "Ward")

# Analyze names of largest built-up areas, down to a population of about 500K
big_bua_names <- data_3 %>%
    group_by(Area) %>%
    mutate(All_categories_Ethnic_group = as.numeric(All_categories_Ethnic_group)) %>%
    summarize(Area_Population = sum(All_categories_Ethnic_group)) %>%
    select(Area, Area_Population) %>%
    ungroup() %>%
    top_n(10, Area_Population)

# Clean, reshape, and summarize data
data_4 <- data_3 %>%
    select(-All_categories_Ethnic_group) %>%
    filter(Area %in% big_bua_names$Area) %>%
    gather(Ethnicity, Population, -Area, -Ward) %>%
    mutate(Population = as.numeric(Population)) %>%
    inner_join(group_mapping, by = "Ethnicity") %>%
    group_by(Group, Ward, Area) %>%
    summarize(Population = sum(Population))

# Check for missing data values
if (all(colSums(is.na(data_4)) != 0)) {
    warning("Warning: England+Wales data has missing values.")
}

#------------------------------------------------------------------------------
# SAVE ANALYTIC DATA
#------------------------------------------------------------------------------

analytic_file <- "england_processing.csv"
write.csv(data_4, analytic_file, row.names = FALSE)

