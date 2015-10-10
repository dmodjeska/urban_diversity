## Background
The data analysis performed by the scripts below replicates a study by Nata Silver of FiveThirtyEight.com on the subject of urban diversity and segregation. The original study explored ethnic diversity at citywide vs. neighborhood levels in the 100 most populous American cities. Building on this work, the current analysis explores diversity for the largest Canadian and UK cities. 

Given the smaller sizes of Canada and the UK relative to the USA, the 10 most populous cities were chosen in each country for analysis. This threshold resulted in the smallest cities analyzed having approximately the same population as the smallest cities analyzed in the American study, on the order of 500,000 people. Also, it's worth noting that comprehensive statistics on ethnicity in the UK are available only for the nations of England and Wales, so these nations were the focus of the present study.

For more information about the original analysis by FiveThirtyEight.com, please see this article:
http://fivethirtyeight.com/features/the-most-diverse-cities-are-often-the-most-segregated/

## Source Data


## Running the Scripts
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
