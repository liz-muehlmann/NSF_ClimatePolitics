## run this chunk at the beginning of the R session

## databricks set up
.libPaths( c( getwd(), .libPaths() ) )

## load libraries
library(sparklyr)
library(data.table)
library(dplyr)
library(httr)
library(jsonlite)
library(fs)
library(arrow)
library(tidyr)

all_files <- list.files("/Workspace/weber_lab/parquet_files", pattern = "\\.parquet$")

# Read all enriched parquet files into a single dataframe
all_files <- list.files("/Workspace/weber_lab/parquet_files", pattern = "\\.parquet$")
file_paths <- file.path("../parquet_files/", all_files)
file_paths <- gsub("//", "/", file_paths)
all_data <- lapply(file_paths, read_parquet)

# Assuming all_data is a list of data frames or data tables
column_names_list <- lapply(all_data, colnames)

# Print the column names for the first data frame
print(column_names_list[[1]])

all_data <- do.call(rbind.data.frame, all_data)

# aggregate voter file data to the Census block level, voter turnout and other variables of interest
block_agg <- all_data %>%
  group_by(Block) %>%
  summarize(
    #voted = sum(General_2020_11_03, na.rm = TRUE),
    median_age = median(Voters_Age, na.rm = TRUE),
    male_prop = mean(Voters_Gender == "M", na.rm = TRUE) * 100,
    median_inc = median(CommercialData_EstimatedHHIncome, na.rm = TRUE)
  )

head(block_agg)
# generate equivalent block+blockgroup+censustract+county+state+country GEOID to merge misc data to L2 file


# append incorporation and census demographic variables to block-level aggregated voter file parquets via unique block number
head(block_agg)


# build voter turnout model and run analysis 