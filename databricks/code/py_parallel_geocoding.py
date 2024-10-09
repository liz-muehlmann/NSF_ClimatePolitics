## import libraries
from pyspark.sql.functions import *
from pyspark.sql import SparkSession
import os, csv, io, logging, requests as r, pandas as pd

## format logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

## define return columns
geocoded_columns = [
    "row_index", "address_in", "match_flag", "match_type",
    "address_out", "long_lat", "tiger_edge", "street_side",
    "state_fips", "county_fips", "tract_id", "block_id"
]

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

        ## convert response object to pandas df
        geocoded_results = pd.read_csv(
            io.StringIO(response.text),
            sep=",",
            header=None,
            quoting=csv.QUOTE_ALL,
            on_bad_lines= "warn",
            engine="python"
        )
        
        ## add in columns
        geocoded_results.columns = geocoded_columns
        geocoded_results["geocoder"] = "Census"
        
        return geocoded_results
    
    except Exception as e:
        logging.error(f"There was a problem querying the API: {str(e)}")
        return None
    
## define batch processing function
def geocode(partition):

    ## create chunk
    chunk = spark.createDataFrame(partition, schema = sample.schema)

    ## process chunk
    query_results = query_api(chunk)

    if query_results is not None:
        # create data frame from results, register
        geocoded_temp.spark.createDataFrame(query_results)
        geocoded_temp.createOrReplaceTempView("geocode_spark")

        ## update table
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

sample = spark.table("sample")
sample.createOrReplaceTempView("sample")
spark.sql("CREATE TABLE IF NOT EXISTS main.weber_lab.sample AS SELECT * FROM sample")
sample.rdd.foreachPartition(geocode)