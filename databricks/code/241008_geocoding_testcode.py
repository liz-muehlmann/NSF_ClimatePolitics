############################# PRELIMINARIES #############################
## import libraries
from pyspark.sql.functions import *
from pyspark.sql import SparkSession
import tempfile, os, csv, io, time, random, math, logging, requests as r, pandas as pd

## start session
spark = SparkSession.builder.appName("DatabricksConnection").getOrCreate()
spark.sql("USE main.weber_lab")

## format logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

############################# CREATE GEO TX TABLE #############################
## create copy of geo table
geo_copy = spark.table("geo")

## filter for texas addresses
geo_tx = geo_copy.where(geo_copy.Residence_Addresses_State == "TX")

## define return columns
geocoded_columns = [
    "row_index",
    "address_in",
    "match_flag",
    "match_type",
    "address_out",
    "long_lat",
    "tiger_edge",
    "street_side",
    "state_fips",
    "county_fips",
    "tract_id",
    "block_id"
]

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

############################# CREATE SAMPLE TABLE #############################
## create test for testing loop
test = geo_tx.limit(10)
test.createOrReplaceTempView("test")
spark.sql("CREATE TABLE IF NOT EXISTS main.weber_lab.test AS SELECT * FROM test")

############################# DEFINE FUNCTIONS #############################
## define function to handle bad lines
bad_line_jail = []
def go_to_jail(line):
    n_badlines_before = len(bad_line_jail)
    bad_line_jail.append(line)
    n_badlines_after = len(bad_line_jail)
    logging.info(f"{n_badlines_after} - {n_badlines_before} badlines were added in this batch. New total: {n_badlines_after}")

## define api query function
def query_api(chunk):
    api_columns = [
        "row_index",
        "Residence_Addresses_AddressLine",
        "Residence_Addresses_City",
        "Residence_Addresses_State",
        "Residence_Addresses_Zip"
    ]
    
    addresses = chunk.select(api_columns).toPandas()
    api_url = "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"

    try:
        ## create csv for census api
        addresses_csv = addresses.to_csv(index=False, header=False)
        upload_file = {
            "addressFile": ("addresses.csv", addresses_csv, "text/csv")
        }
        
        data_request = {
            "benchmark": "Public_AR_Current",
            "vintage": "Census2020_Current"
        }

        ## save api response
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

        ## change geocoder to Census for use in loop
        geocoded_results["geocoder"] = "Census"
        print(f"The results of the query are: ", geocoded_results)
        return geocoded_results
    
    except Exception as e:
        logging.error(f"There was a problem querying the API: {str(e)}")
        return None
    
geocoded_df = query_api(test)
type(geocoded_df)
geocoded_df.dtypes

# Create the DataFrame with casting
geocoded_spark = spark.createDataFrame(geocoded_df)
# type(geocoded_spark)

geocoded_spark.printSchema()

# geocoded_spark = geocoded_spark.select(
#     col("row_index").cast("bigint"),
#     col("address_in").cast("string"),
#     col("match_flag").cast("string"),
#     col("match_type").cast("string"),
#     col("address_out").cast("string"),
#     col("long_lat").cast("string"),
#     col("tiger_edge").cast("bigint"),  
#     col("street_side").cast("string"),
#     col("state_fips").cast("bigint"),  
#     col("county_fips").cast("bigint"), 
#     col("tract_id").cast("bigint"),    
#     col("block_id").cast("bigint"),    
#     col("geocoder").cast("string")
# )

# geocoded_spark.printSchema()
# # # Create or replace the temporary view
geocoded_spark.createOrReplaceTempView("geocode_temp")
# # # Create or replace the table
spark.sql("CREATE OR REPLACE TABLE main.weber_lab.geocoded_temp AS SELECT * FROM geocode_temp")

geocoded_spark.show(vertical=True)

spark.sql("""
    MERGE INTO main.weber_lab.test AS original
    USING main.weber_lab.geocoded_temp AS updated
    ON original.row_index = updated.row_index
    WHEN MATCHED THEN
    UPDATE SET
        original.geocoder = updated.geocoder
        """)

spark.catalog.uncacheTable("test")
test = spark.table("main.weber_lab.test")
test.show(vertical=True)

spark.sql("SELECT geocoder FROM main.weber_lab.test").show()
spark.sql("SELECT geocoder FROM main.weber_lab.geocoded_temp").show()
# spark.sql("CREATE OR REPLACE TABLE main.weber_lab.test AS SELECT * FROM test")


%sql
SELECT original.row_index, original.geocoder, updated.geocoder
FROM main.weber_lab.test AS original
LEFT JOIN main.weber_lab.geocoded_temp AS updated
ON original.row_index = updated.row_index
