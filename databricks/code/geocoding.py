## import libraries
import pyspark.sql.functions as F
from pyspark.sql import SparkSession
from pyspark.sql.window import Window
import tempfile, os, csv, io, requests as r, pandas as pd, numpy as np

################################################################################
##                                                                            ##
## the following section is only necessary when using the sample data hosted  ##
## on a local machine. Skip to the next section when transporting this        ##
## information into Databricks.                                               ##
##                                                                            ##
################################################################################

## change working directory
os.chdir("F:/GitHub/NSF_ClimatePolitics/databricks/")
# os.chdir("C:/Users/lizmo/Documents/GitHub/NSF_ClimatePolitics/databricks/")

## import csv
geo_tx = pd.read_csv("./data/l2sample_tx.csv", low_memory=False)

## create row index
geo_tx = geo_tx.assign(row_index=range(len(geo_tx)))


################################################################################
##                                                                            ##
## the following section is for processing the files on databricks.           ##
## Double check DF names                                                      ##
##                                                                            ##
################################################################################

########### .select is a pyspark command | need this for databricks ############

## select all columns, add row index
# geo_tx = geo_tx.select(
#     "*",
#     F.monotonically_increasing_id().alias("row_index")
#     )

################################################################################
##                                                                            ##
##                         geocoding prelims                                  ##
##                                                                            ##
################################################################################

## create indicator column to track geocoding
geo_tx['geocoded'] = np.NaN

## set temporary directory
temp_dir = "./data/temp/"

## define function to handle bad lines
bad_line_jail = []
def go_to_jail(line):
    bad_line_jail.append(line)

## define geocoding addresses function
def geocode_addresses(addresses):
    try:
        # create temporary file with subset of addresses
        with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as temp_file:
            addresses.to_csv(temp_file.name, index=False, header=False, index_label=False)
            print(f"Temporary CSV file created: {temp_file.name}")
        
        # define api url
        apiurl = "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
        
        # define upload file
        upload_file = {
            "addressFile": (temp_file.name, open(temp_file.name, "rb"), "text/csv")
            }
        
        # define data to request from census
        data_request = {
            "benchmark": "Public_AR_Current", 
            "vintage": "Census2020_Current"
            }
        
        
        # save census response data
        sd = r.post(apiurl, files=upload_file, data=data_request)

        # convert response data to pandas df
        return_df = pd.read_csv(
            io.StringIO(sd.text),
            sep=",",
            header = None,
            quoting=csv.QUOTE_ALL,
            on_bad_lines=go_to_jail,
            engine="python"
        )

        # add in column names
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
        
        return return_df
    
    except Exception as e:
        print(f"Oh no! Something went wrong: {str(e)}")
        return pd.DataFrame()
    
while geo_tx['geocoded'].isna().any():
    # subset n rows of geocoded data
    chunk = geo_tx[geo_tx['geocoded'].isna()].head(50)
    if chunk.empty:
        print("chunk is empty")
        break
    
    # subset addresses for geocoding
    addresses = chunk[
    [
        "row_index",
        "Residence_Addresses_AddressLine",
        "Residence_Addresses_City",
        "Residence_Addresses_State",
        "Residence_Addresses_Zip",
    ]
    ]

    # geocode the addresses
    geocoded_results = geocode_addresses(addresses)
    
    # add match data  the original df
    geo_tx = geo_tx.merge(geocoded_results, on='row_index', how='left')

    # update indicator columns
    geo_tx['geocoded'] = geo_tx['match_flag'].notna()

################################################################################
##                                                                            ##
##                          build up function                                 ##
##                                                                            ##
################################################################################

# # subset the row index and address columns from geo_tx
# addresses = geo_tx[
#     [
#         "row_index",
#         "Residence_Addresses_AddressLine",
#         "Residence_Addresses_City",
#         "Residence_Addresses_State",
#         "Residence_Addresses_Zip",
#     ]
# ]
# 
# # set temporary directory
# temp_dir = "./data/temp/"
# 
# try:
#     with tempfile.NamedTemporaryFile(delete=False, suffix=".csv") as temp_file:
#         addresses.to_csv(temp_file.name, index=False, header=False, index_label=False)
#         print(f"Temporary CSV file created: {temp_file.name}")
# 
# except Exception as e:
#     print(f"Danger, Will Robinson! {str(e)}")
# 
# ## view temporary file
# d = pd.read_csv(temp_file.name)
# # os.remove(temp_file)
# 
# # define bad lines function
# bad_line_jail = []
# def go_to_jail(line):
#     bad_line_jail.append(line)
# 
# try:
#     # define api url
#     apiurl = "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"
# 
#     # define upload file
#     upload_file = {"addressFile": (temp_file.name, open(temp_file.name, "rb"), "text/csv")}
# 
#     # define data to request from census
#     data_request = {"benchmark": "Public_AR_Current", "vintage": "Census2020_Current"}
# 
#     # save census response data
#     sd = r.post(apiurl, files=upload_file, data=data_request)
#     
#     # convert response option to pandas df
#     return_df = pd.read_csv(
#         io.StringIO(sd.text),
#         sep=",",
#         header = None,
#         quoting=csv.QUOTE_ALL,
#         on_bad_lines=go_to_jail,
#         engine="python"
#     )
# 
#     # make the data pretty
#     with pd.option_context(
#         "display.width", None,
#         "display.max_columns", None,
#         "display.max_colwidth", -1,
#         "display.colheader_justify", "left"
#     ):
#         print(return_df)
#     
# except ValueError as ve:
#     bad_line_jail.append(ve)
# except Exception as e:
#     print(f"Oh no! Something went wrong: {str(e)}")
# 
# # add in column names
# return_df.columns = [
#     "row_index",
#     "address_in",
#     "match_flag",
#     "match_type",
#     "address_out",
#     "long_lat",
#     "tiger_edge",
#     "street_side",
#     "state_fips",
#     "county_fips",
#     "tract_id",
#     "block_id"
# ]    
# 
# ## left join to original df
# geo_tx_updated = geo_tx.merge(return_df, how = 'left', on = 'row_index')
# 
# 
# ## remove temporary file
# temp_file.close()
# os.remove(temp_file.name)
# 
# 
# print(bad_line_jail)
# 