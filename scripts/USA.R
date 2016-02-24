# ==============================================================================
# THIS FILE GETS AND PROCESSES USA CENSUS DATA ON VISIBLE MINORITIES
# ==============================================================================

usa_dir <- "USA Data"
if (!file.exists(usa_dir)) {
    dir.create(usa_dir)
}

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(data.table)
library(xlsx)

#------------------------------------------------------------------------------
# GET AND CLEAN USA DATA 
#------------------------------------------------------------------------------

# Download census data files if not already present
# Note: formatting of this Excel file requires manual export as XLSX
usa_file_prefix = "msa10p"
usa_download_path <- paste(usa_dir, paste0(usa_file_prefix, ".xls"), sep = "/")
usa_exported_path <- paste(usa_dir, paste0(usa_file_prefix, ".xlsx"), sep = "/")
if (!file.exists(usa_download_path)) {
    usa_url <- paste0("www.s4.brown.edu/us2010/Data/WPdownload/",
                      paste0(usa_file_prefix, ".xls"))
    download.file(usa_url, usa_download_path)
}

# calculate metrowide diversity for one metro, using group exposures
calc_metrowide_diversity <- function(White, Black, Hispanic, Asian, Other) {
    white_exposure <- (1 - White) * White
    black_exposure <- (1 - Black) * Black
    hispanic_exposure <- (1 - Hispanic) * Hispanic
    asian_exposure <- (1 - Asian) * Asian
    other_exposure <- (1 - Other) * Other

    metrowide_diversity <- white_exposure + black_exposure + hispanic_exposure +
        asian_exposure + other_exposure
    return(metrowide_diversity)
}

# calculate other-other exposure for one metro, using named group exposures
calc_other_other_exposure <- function(ww, bb, hh, aa,
                                      wb, wh, wa, bw, bh, ba,
                                      hw, hb, ha, aw, ab, ah) {
    white_other_exposure <- (1 - ww) - (wb + wh + wa)
    black_other_exposure <- (1 - bb) - (bw + bh + ba)
    hispanic_other_exposure <- (1 - hh) - (hw + hb + ha)
    asian_other_exposure <- (1 - aa) - (aw + ab + ah)

    other_other_exposure = white_other_exposure + black_other_exposure +
        hispanic_other_exposure + asian_other_exposure
    return(other_other_exposure)
}

# calculate tract-level diversity for one metro, using group exposures
calc_tracts_diversity <- function(White, Black, Hispanic, Asian, Other,
                                  ww, bb, hh, aa, oo) {
    white_exposure <- (1 - ww) * White
    black_exposure <- (1 - bb) * Black
    hispanic_exposure <- (1 - hh) * Hispanic
    asian_exposure <- (1 - aa) * Asian
    other_exposure <- (1 - oo) * Other

    tracts_diversity <- white_exposure + black_exposure + hispanic_exposure +
        asian_exposure + other_exposure
    return(tracts_diversity)
}

# Load and clean the data, selecting the top 100 cities
colClasses = c(rep("character", 2), rep("integer", 6), rep("numeric", 21))
usa_read_data <- read.xlsx2(usa_exported_path, sheetIndex = 1, colClasses = colClasses)

# Check for missing data values
if (all(colSums(is.na(usa_read_data)) != 0)) {
    warning("Warning: USA data has missing values.")
}

usa_data = usa_read_data %>%
    rename(Metro = metroname, Metro_Population = m_t_a_10,
           White = m_w_a_10, Black = m_b_a_10,
           Hispanic = m_h_a_10, Asian = m_a_a_10, Other = m_o_a_10) %>%
    filter(!grepl("Metropolitan Division", Metro)) %>%
    top_n(100, Metro_Population) %>%
    mutate(Metro = gsub(" Metropolitan Statistical Area", "", Metro)) %>%
    mutate_each(funs(hundredth = . / 100), 9:29) %>%
    separate(col = Metro, into = c("Metro", "State_Abb"), sep = ",")

usa_diversity_indices <- usa_data %>%
    mutate(Metrowide_Diversity_Index
           = calc_metrowide_diversity(m_pw_a_10, m_pb_a_10, m_ph_a_10,
                                     m_pa_a_10, m_po_a_10)) %>%
    mutate(m_xoo_a_10
           = calc_other_other_exposure(m_xww_a_10, m_xbb_a_10, m_xhh_a_10, m_xaa_a_10,
                                         m_xwb_a_10, m_xwh_a_10, m_xwa_a_10,
                                         m_xbw_a_10, m_xbh_a_10, m_xba_a_10,
                                         m_xhw_a_10, m_xhb_a_10, m_xha_a_10,
                                         m_xaw_a_10, m_xab_a_10, m_xah_a_10)) %>%
    mutate(Tracts_Diversity_Index
           = calc_tracts_diversity(m_pw_a_10, m_pb_a_10, m_ph_a_10, m_pa_a_10,
                                    m_po_a_10,
                                    m_xww_a_10, m_xbb_a_10, m_xhh_a_10, m_xaa_a_10,
                                    m_xoo_a_10)) %>%
    rename(Black_Prop = m_pb_a_10) %>%
    select(Metro, State_Abb, Metrowide_Diversity_Index, Tracts_Diversity_Index,
           Metro_Population, Black_Prop)

#------------------------------------------------------------------------------
# SAVE ANALYTIC DATA
#------------------------------------------------------------------------------

write.csv(usa_diversity_indices, "USA_Metro_Diversity.csv", row.names = FALSE)
