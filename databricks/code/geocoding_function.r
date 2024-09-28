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
  df <- left_join(df, results_df, by = "row_index") |>
    mutate(tf = "true")
  return(df)
 }