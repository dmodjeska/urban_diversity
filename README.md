## Background
The data analysis performed by the scripts below replicates a study by Nata Silver of FiveThirtyEight.com on the subject of urban diversity and segregation. The original study explored ethnic diversity at citywide vs. neighborhood levels in the 100 most populous American cities. Building on this work, the current analysis explores diversity for the largest Canadian and UK cities. 

Given the smaller sizes of Canada and the UK relative to the USA, the 10 most populous cities were chosen in each country for analysis. This threshold resulted in the smallest cities analyzed having approximately the same population as the smallest cities analyzed in the American study, on the order of 500,000 people. Also, it's worth noting that comprehensive statistics on ethnicity in the UK are available only for the nations of England and Wales, so these nations were the focus of the present study.

For more information about the original analysis by FiveThirtyEight.com, please see this article:
http://fivethirtyeight.com/features/the-most-diverse-cities-are-often-the-most-segregated/

## Source Data

The Canadian data used in this analysis were obtained from government census and geographic repositories:
* http://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/prof/details/download-telecharger/comprehensive/comp-csv-tab-nhs-enm.cfm?Lang=E
  * 99-004-XWE2011001-401_CSV.ZIP (interactive download only)
* http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gct_000b11a_e.zip
* http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gcsd000b11a_e.zip

The UK data used in this analysis were obtained from government census and geographic repositories:
* http://www.ons.gov.uk/ons/rel/census/2011-census/key-statistics-and-quick-statistics-for-wards-and-output-areas-in-england-and-wales/rft-ct0010-wards.zip
* www.ons.gov.uk/ons/external-links/social-media/g-m/2011-built-up-areas-to-2011-lads.html
  * OA11_WD11_LAD11_EW_LU.csv (interactive download only)
* http://www.ons.gov.uk/ons/external-links/social-media/g-m/2011-oas-to-2011-wards--with-best-fit-percentage-indicator-.html
  * OA11_BUASD11_BUA11_LAD11_RGN11_EW_LU.csv (interactive download only)
* http://www.ons.gov.uk/ons/external-links/social-media/g-m/2011-census-merged-ward-boundaries---generalised--20-metres---clipped-to-the-coastline.html (interactive download only via http://www.ons.gov.uk/ons/guide-method/geography/products/census/spatial/2011/index.html)

The results of the American data analysis were obtained from the FiveThirtyEight.com article mentioned above.

## R Script Files

The following R scripts are used in this data analysis:
* **canada_processing.R**: get and process Canada census data
* **canada_analysis.R**: analyze Canada processing data, and output graphics and tables
* england_processing.R: get and process UK census data
* england_analysis.R: analyze UK processing data, and output graphics and tables
* summary_presentation.R: output summary graphics for USA, Canada, and UK

uk_ethnicity_mappings.csv


The script assumes that the experimental data set has already been downloaded 
and unzipped into the current working directory. The script also assumes that 
the data set retains its original name of "UCI HAR Dataset" and its original 
file structure. The data set can be downloaded from here:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

To run the script in an R environment, use the following command:

```
source("run_analysis.R")
```

The output of the script is a text file that contains the summarized data, per
the codebook described below. This text file is created in the current working
directory. This file can be read in a standard text editor or spreadsheet 
program. This file can also be read in R, using the following commands:

```
samsung_summary <- read.table("samsung_data_summary.txt", header=TRUE)
View(samsung_summary)
```

## Output Files
A codebook for the summary data can be found in the same directory as
run_analysis.R. This codebook is named codebook.md.

The codebook contains the following information:

* A list of the variables in the summary data, along with a description of each
variable, its units, and the values that it can hold.
* A detailed list of the steps taken to tidy and summarize the original data
set. In particular, steps are outlined to tidy the data, per Hadley Wickham's 
principles.
