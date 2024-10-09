## import libraries
import pyspark.sql.functions as F

from pyspark.sql import SparkSession
from pyspark.sql.window import Window
import tempfile, os, csv, io, time, random, requests as r, pandas as pd, numpy as np

## start session
spark = SparkSession.builder.appName("DatabricksConnection").getOrCreate()
spark.sql("USE main.weber_lab")
geo_copy = spark.table("geo")

## filter for texas addresses
geo_tx = geo_copy.where(geo_copy.Residence_Addresses_State == "TX")

## select all columns, add row index
geo_tx = geo_tx.select(
    "*",
    F.monotonically_increasing_id().alias("row_index")
    )

geo_tx.createOrReplaceTempView("geo_tx")
spark.sql("CREATE TABLE IF NOT EXISTS main.weber_lab.geo_tx AS SELECT * FROM geo_tx")

temp_dir = "/Workspace/weber_lab/code/temp_dir"
temp_file_path = os.path.join(temp_dir, "temp_file.csv")

bad_line_jail = []
def go_to_jail(line):
    bad_line_jail.append(line)

geo_tx = geo_tx.withColumn('geocoded', F.lit(None))

n_chunks = geo_tx.count() / 500
i = 0

chunk = geo_tx.filter(geo_tx['geocoded'].isNull()).limit(600)

addresses = chunk.select([
        "row_index",
        "Residence_Addresses_AddressLine",
        "Residence_Addresses_City",
        "Residence_Addresses_State",
        "Residence_Addresses_Zip"]
        )

address_df = addresses.toPandas()
address_df.to_csv(temp_file_path, index=False, header=False, index_label=False)


apiurl = "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
        
# define upload file
upload_file = {
    "addressFile": (temp_file_path, open(temp_file_path, "rb"), "text/csv")
    }
 
# define data to request from census
data_request = {
    "benchmark": "Public_AR_Current", 
    "vintage": "Census2020_Current"
    }

# save census response data
sd = r.post(apiurl, files=upload_file, data=data_request)

# error if response != 200
sd.raise_for_status()

# convert response data to pandas df
return_df = pd.read_csv(
    io.StringIO(sd.text),
    sep=",",
    header = None,
    quoting=csv.QUOTE_ALL,
    on_bad_lines=go_to_jail,
    engine="python"
)

## add in column names
return_df.columns = [
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

return_df.head()
