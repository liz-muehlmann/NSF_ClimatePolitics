## load libraries 
library(sparklyr)
library(dplyr)
library(DBI)
library(httr)
library(jsonlite)
library(fs)
library(stringr)

## create driver & point it to default location
sc = spark_connect(method = 'databricks')
tbl_change_db(sc, 'main.weber_lab')

## change tf to NA instead of "None" (python to R conversion hiccup)
geo_test_all <- tbl(sc, "geo_test") |>
  mutate(full_address = paste(Residence_Addresses_AddressLine, Residence_Addresses_City, Residence_Addresses_State, Residence_Addresses_Zip, sep = ", ")) 

geo_test_distinct <- geo_test_all |>
  distinct(full_address, .keep_all = TRUE)

## make sure all the columns and their data types are correct before processing
glimpse(geo_test)

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

## check if API is functional
if (req$status_code != 200) {
  stop("API request failed with status: ", req$status_code)
  }

## Process API output
api_output <- content(req, "text", encoding = "UTF-8")

## if the API returns nothing, stop and report 
if (nchar(api_output) == 0) {
  stop("No data returned from the API.")
  } 

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

## subset table - select n rows that relied on L2 geocoder
geo_sample <- geo_test |>
  filter(l2_census == "L2") |>
  select(row_index, everything()) |>
  sample_n(10000, replace = FALSE) |>
  collect() 

## geocode using the tidycensus geocoder
census_geocoded <- process_geocode_batch(geo_sample)

## copy census geocoded data back to spark to select rows on the backend
census_geocoded <- sdf_copy_to(sc, census_geocoded, name = "census_geocoded", overwrite = TRUE)


## pull the original data to compare if L2 geocoder returns the same information as the Census geocoder
l2_geocoded <- geo_test |>
    semi_join(census_geocoded, by = "row_index") |>
    select(starts_with("Residence"), row_index) |>
    mutate(full_address = paste(Residence_Addresses_AddressLine, Residence_Addresses_City, Residence_Addresses_State, Residence_Addresses_Zip, sep = ", ")) |>
    select(row_index, full_address) |>
    collect() 


## merge census geocoded addresses with l2 coded & select rows that do not match
check_geocodes <- census_geocoded |>
  select(row_index, Standardized_Address) |>
  rename(standardized_address = Standardized_Address) |>
  collect() |>
  right_join(l2_geocoded, by = "row_index") |>
  mutate(is_match = ifelse(standardized_address == full_address, " ", "not a match")) |>
  filter(is_match == "not a match")

  # Function to extract differing parts
address_diffs <- function(std_addr, full_addr) {
  std_parts <- unlist(str_split(std_addr, "\\s+"))
  full_parts <- unlist(str_split(full_addr, "\\s+"))
  
  # Find parts in full_address that are not in standardized_address
  differences <- setdiff(full_parts, std_parts)
  
  # Return as a single string
  return(paste(differences, collapse = " "))
}

# Apply the function to the data frame
check_diffs <- check_geocodes %>%
  mutate(differences = mapply(address_diffs, standardized_address, full_address))

# View the result

print(head(check_diffs |> select(standardized_address, full_address, differences) |> select(standardized_address), n = 5))

print(head(check_diffs |> select(standardized_address, full_address, differences) |> select(full_address), n = 5))

while( geo_test |> filter(is.na(tf)) |> tally() |> pull(1) > 0 ){
  
  ## subset table - select 10k rows that relied on L2 geocoder
  geo_sample <- geo_test |>
    filter(l2_census == "L2") |>
    select(row_index, everything()) |>
    sample_n(5, replace = FALSE) |>
    collect() 

  ## geocode using the tidycensus geocoder
  geo_sample <- process_geocode_batch(geo_sample)

  ## copy back to spark
  geo_sample_sdf <- sdf_copy_to(sc, geo_sample, name = "geoSample_matched", overwrite = TRUE)
  
  DBI::dbExecute(sc, 
  'MERGE INTO geo_test
   USING geoSample_matched 
   ON geo_test.row_index = geoSample_matched.row_index
   WHEN MATCHED THEN 
    UPDATE SET 
      geo_test.Residence_Addresses_AddressLine = geoSample_matched.Standardized_Address,
        geo_test.lat = geoSample_matched.lat,
        geo_test.long = geoSample_matched.long,
        geo_test.accuracy_lat_long = geoSample_matched.accuracy_lat_long,
        geo_test.tf = geoSample_matched.tf')
    }
  