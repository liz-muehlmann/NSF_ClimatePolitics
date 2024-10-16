# %md
# # Geocoding Texas Addresses
# The code in this notebook takes the addresses in Texas, geocodes them using the Census geocoder, and updates the ```geo_tx``` table with the new information.

## import libraries
from pyspark.sql.functions import *
from pyspark.sql import SparkSession
import tempfile, os, csv, io, time, random, math, logging, requests as r, pandas as pd

## start session
spark = SparkSession.builder.appName("DatabricksConnection").getOrCreate()
spark.sql("USE main.weber_lab")

## format logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

## define return columns
geocoded_columns = [
    "row_index", "address_in", "match_flag", "match_type",
    "address_out", "long_lat", "tiger_edge", "street_side",
    "state_fips", "county_fips", "tract_id", "block_id"
]

## define function to handle bad lines
bad_line_jail = []
def go_to_jail(line):
    n_badlines_before = len(bad_line_jail)
    bad_line_jail.append(line)
    n_badlines_after = len(bad_line_jail)
    logging.info(f"{n_badlines_after} - {n_badlines_before} badlines were added in this batch. New total: {n_badlines_after}")
    
## define api query function
def query_api(chunk):

    api_url = "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"

    try:
        ## create csv for census api
        api_columns = [
        "row_index",
        "Residence_Addresses_AddressLine",
        "Residence_Addresses_City",
        "Residence_Addresses_State",
        "Residence_Addresses_Zip"
        ]

        addresses = chunk.select(api_columns).toPandas()

        addresses_csv = addresses.to_csv(index=False, header=False)

        ## set query details
        upload_file = {
            "addressFile": ("addresses.csv", addresses_csv, "text/csv")
        }
        
        data_request = {
            "benchmark": "Public_AR_Current",
            "vintage": "Census2020_Current"
        }

        ## query api & save
        response = r.post(api_url, files=upload_file, data=data_request)
        response.raise_for_status()
        # logging.info(f"API response for chunk: {response.text}")

        ## convert response object to pandas df
        geocoded_results = pd.read_csv(
            io.StringIO(response.text),
            sep=",",
            header=None,
            quoting=csv.QUOTE_ALL,
            on_bad_lines=go_to_jail,
            engine="python"
        )
        
        ## add in columns
        geocoded_results.columns = geocoded_columns

        ## check if geocoder column exists, create it
        if "geocoder" not in geocoded_results.columns:
            geocoded_results["geocoder"] = None

        ## change geocoder to census for use in loop
        geocoded_results["geocoder"] = "Census"
        # print(f"The results of the query are: ", geocoded_results)
        return geocoded_results
    
    except Exception as e:
        logging.error(f"There was a problem querying the API: {str(e)}")
        return None
    
geo_copy = spark.table("geo")

## filter for texas addresses
geo_tx = geo_copy.where(geo_copy.Residence_Addresses_State == "TX")

## select all columns, add row index, create empty geocoding columns
geo_tx = geo_tx.select(
    "*",
    monotonically_increasing_id().alias("row_index"),
    lit(None).cast("string").alias("geocoder"),  
    *[lit(None).cast("string").alias(col) for col in geocoded_columns[1:]]  
)

## register table with spark
geo_tx.createOrReplaceTempView("geo_tx")
spark.sql("CREATE TABLE IF NOT EXISTS main.weber_lab.geo_tx AS SELECT * FROM geo_tx")

sample = geo_tx.limit(20)
sample.createOrReplaceTempView("sample")
spark.sql("CREATE TABLE IF NOT EXISTS main.weber_lab.sample AS SELECT * FROM sample")

## chunk preliminaries
chunk_size = 5
n_rows_original = spark.sql("SELECT COUNT(*) FROM main.weber_lab.sample").first()[0]  
n_chunks = math.ceil(n_rows_original / chunk_size)  
chunks_processed = 0

while True:

    ## create new chunk
    new_chunk = spark.sql(f"SELECT * FROM main.weber_lab.sample WHERE geocoder IS NULL LIMIT {chunk_size}".format(chunk_size))
    # print(f"New chunk looks like this: ", new_chunk.select(geocoded_columns).show())
    
    logging.info(f"New chunk size: {new_chunk.count()}")

    if new_chunk.count() == 0:
        logging.info("No more new chunks to process.")
        break

    ## process chunk
    query_results = query_api(new_chunk)

    if query_results is None:
        logging.warning("API failed to geocode addresses. Skipping this chunk.")
        continue

    logging.info(f"Processing chunk {chunks_processed + 1}/{n_chunks}...")

    ## register query results with spark
    geocoded_spark = spark.createDataFrame(query_results)
    geocoded_spark.createOrReplaceTempView("geocoded_spark")
    spark.sql("CREATE OR REPLACE TABLE main.weber_lab.geocoded_spark AS SELECT * FROM geocode_spark")
    
    ## count processed rows for progress
    rows_before = spark.sql("SELECT COUNT(*) FROM main.weber_lab.sample WHERE geocoder IS NULL").first()[0]
    
    ## update sample table
    spark.sql("""
        MERGE INTO main.weber_lab.sample AS original
        USING main.weber_lab.geocoded_spark AS updated
        ON original.row_index = updated.row_index
        WHEN MATCHED THEN
        UPDATE SET
            original.geocoder = updated.geocoder,
            original.address_in = updated.address_in,
            original.match_flag = updated.match_flag,
            original.match_type = updated.match_type,
            original.address_out = updated.address_out,
            original.long_lat = updated.long_lat,
            original.tiger_edge = updated.tiger_edge,
            original.street_side = updated.street_side,
            original.state_fips = updated.state_fips,
            original.county_fips = updated.county_fips,
            original.tract_id = updated.tract_id,
            original.block_id = updated.block_id
            """)
    
    ## clear cache and update table
    spark.catalog.uncacheTable("sample")
    sample = spark.table("main.weber_lab.sample")

    ## count how many rows are left
    rows_after = spark.sql("SELECT COUNT(*) FROM main.weber_lab.sample WHERE geocoder IS NULL").first()[0]
    logging.info(f"Rows with geocoder IS NULL after merge: {rows_after}")

    ## Check for successful updates
    if rows_before == rows_after:
        logging.warning("No rows were updated during this iteration.")
        
    
    ## iterate chunk counter
    chunks_processed += 1

    ## print processing progress
    percent_done = (chunks_processed / n_chunks) * 100
    percent_remaining = 100 - percent_done
    logging.info(f"Processed chunk {chunks_processed}/{n_chunks}. "
                 f"{percent_done:.2f}% done, {percent_remaining:.2f}% remaining.")
    
    ## drop extra tables
    spark.sql("DROP TABLE IF EXISTS main.weber_lab.geocoded_spark")
    spark.catalog.dropTempView("geocoded_spark")
    spark.catalog.dropTempView("geocoded_temp")

