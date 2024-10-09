# md
# # Geocoding Texas L2 Data
# This notebook geocodes the individual parquet files that were previously saved in the [python_partition_L2](https://dbc-2ac542dc-2fdf.cloud.databricks.com/?o=2305557431820458#notebook/2804606605601366) notebook in /Workspace/Users/em8468@princeton.edu/ folder and resaved using R. Each partition has its own unique identifier and contains ~10,000 rows (the max number of rows the Census API allows to be processed at a given time). 
 
## run this chunk at the beginning of the R session

## databricks set up
.libPaths( c( getwd(), .libPaths() ) )

## load libraries
library(sparklyr)
library(dplyr)
library(httr)
library(jsonlite)
library(fs)
library(arrow)

## create driver & point it to default location
sc = spark_connect(method = 'databricks')
tbl_change_db(sc, 'main.weber_lab')

## create function to process geocoding in batches and update Spark table

process_geocode_batch <- function(df) {
  # Create address dataframe
  addresses <- df %>%
  select(row_index, Residence_Addresses_AddressLine, Residence_Addresses_City, Residence_Addresses_State, Residence_Addresses_Zip) 

file <- tempfile(fileext = ".csv")
write.table(addresses, file, sep = ",", col.names = FALSE, row.names = FALSE, quote = TRUE)

apiurl <- "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
  
req <- POST(apiurl, body=list(
  addressFile = upload_file(file), 
  benchmark = "Public_AR_Current",
  vintage = "Census2020_Current"
), encode="multipart")

## Process API output
api_output <- content(req, "text", encoding = "UTF-8")
 
## split into lines
lines <- strsplit(api_output, "\n")[[1]]

### create list of vectors
data_list <- lapply(lines, function(line) strsplit(line, "\",\"")[[1]])

### remove quotes from each element
data_list <- lapply(data_list, function(x) gsub("\"", "", x))
results_matrix <- do.call(rbind, data_list)
results_df <- as.data.frame(results_matrix, stringsAsFactors = FALSE)

colnames(results_df) <- c("row_index", "Input Address", "Match Flag", "Match Type",
                          "Standardized Address", "Coordinates", "Census Block ID",
                          "Side of Street", "State FIPS Code", "County FIPS Code",
                          "Census Tract", "Block")

  results_df$row_index <- as.numeric(results_df$row_index)
  df$row_index <- as.numeric(df$row_index)
  df <- left_join(df, results_df, by = "row_index")
 }

 ######## uncomment this chunk if you need to resave the partitions #####################################

## List all parquet files in the folder
all_files <- list.files("/Workspace/weber_lab/parquet_files/", pattern = "\\.parquet$", full.names = TRUE)

n_partitions <- length(all_files)

pattern <- "_geocoded\\.parquet$"

i <- 0
s <- 0
for (file in all_files){
  if (grepl(pattern, file)){
    cat("Skipping file: ", file, " \n")
    
    s <- s + 1
    
    next
  } else{
    df <- read_parquet(file)
    geocoded_df <- process_geocode_batch(df)
    new_path <- paste0(sub("\\.parquet$", "_geocoded.parquet", file))
    
    tryCatch({
      write_parquet(geocoded_df, new_path)
      file.remove(file)
      i <- i + 1
      print(paste0(file, " written. It is ", i + s, " out of ", n_partitions, " partitions. ", round((i+s)/n_partitions*100, 2), "% done." ))

  }, error = function(e){
    cat("Error processing file: ", file, ":", e$message, "\n")
  })
}
  }
