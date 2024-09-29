## import libraries
import pyspark.sql.functions as F
from pyspark.sql import SparkSession
from pyspark.sql.window import Window
import tempfile, os, csv, io, requests as r, pandas as pd


################################################################################
## the following section is only necessary when using the sample data hosted  ##
## on a local machine. Skip to the next section when transporting this        ##
## information into Databricks.                                               ##
################################################################################

## change working directory
os.chdir("F:/GitHub/NSF_ClimatePolitics/databricks/")

## import csv
geo_tx = pd.read_csv("./data/l2sample_tx.csv")

geo_tx = geo_tx.assign(row_index=range(len(geo_tx)))

################################################################################
## the following section is for processing the files on databricks.           ##
## Double check DF names                                                      ##
################################################################################

########### .select is a pyspark command | need this for databricks ############

## select all columns, add row index
# geo_tx = geo_tx.select(
#     "*",
#     F.monotonically_increasing_id().alias("row_index")
#     )

# ______________________________________________________________________________
# define geocoding function                                                 ####


def batch_geocode(df):
    # subset the row index and address columns from geo_tx
    addresses = df[
        [
            "row_index",
            "Residence_Addresses_AddressLine",
            "Residence_Addresses_City",
            "Residence_Addresses_State",
            "Residence_Addresses_Zip",
        ]
    ]

    # set temporary directory
    temp_dir = "./data/temp/"

    try:
        # set temporary file name
        temp_fn = "temp_addresses.csv"

        # set temporary file path
        temp_fp = os.path.join(temp_dir, temp_fn)

        temp_file = tempfile.NamedTemporaryFile(
            delete=False, dir=temp_dir, prefix=temp_fn
        )

        addresses.to_csv(temp_file, header=False, index=False)

        pd.read_csv(temp_file.name, nrows=5)

    except Exception as e:
        print(f"Danger, Will Robinson! {str(e)}")


################################################################################
##                                                                            ##
##                          build up function                                 ##
##                                                                            ##
################################################################################

# subset the row index and address columns from geo_tx
addresses = geo_tx[
    [
        "row_index",
        "Residence_Addresses_AddressLine",
        "Residence_Addresses_City",
        "Residence_Addresses_State",
        "Residence_Addresses_Zip",
    ]
]

# set temporary directory
temp_dir = "./data/temp/"

try:
    # set temporary file name
    temp_fn = "temp_addresses.csv"

    # set temporary file path
    temp_fp = os.path.join(temp_dir, temp_fn)

    # create temporary file
    temp_file = tempfile.NamedTemporaryFile(delete=False, dir=temp_dir, prefix=temp_fn)

    # write address df to temporary file
    addresses.to_csv(temp_file, header=False, index=False)

# error handling
except Exception as e:
    print(f"Danger, Will Robinson! {str(e)}")

# define api url
apiurl = "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"

# define upload file
upload_file = {'file': open(temp_file.name, 'rb')}

# define data to request from census
data_request = {'addressFile': upload_file,
           'benchmark': 'Public_AR_Current',
           'vintage': 'Census2020_Current'}

# create empty list for bad lines

bad_line_jail = []
# define bad lines function
def go_to_jail(line):
    bad_line_jail.append(line)


try:
    # save census response data
    sd = r.post(apiurl, data = data_request)
    
    # convert response option to pandas df
    df = pd.read_csv(io.StringIO(sd.text), sep = ",", header = None, quoting = csv.QUOTE_ALL, on_bad_lines = go_to_jail, engine = 'python')
    
    # make the data pretty
    with pd.option_context(
        'display.width', None,
        'display.max_columns', None,
        'display.max_colwidth', -1,
        'display.colheader_justify', 'left'
    ):
        print(df)
except ValueError as ve:
    bad_line_jail.append(ve)
except Exception as e:
    print(f"Oh no! Something went wrong: {str(e)}")

## remove temporary file
os.remove(temp_fp)


print(bad_line_jail)











