## define function to handle bad lines
bad_line_jail = []
def go_to_jail(line):
    bad_line_jail.append(line)

## define geocoding function:
def geocode(pyspark_df):
  
  # subset required columns for api
  api_columns = [
    "row_index",
    "Residence_Addresses_AddressLine",
    "Residence_Addresses_City",
    "Residence_Addresses_State",
    "Residence_Addresses_Zip"
    ]
  addresses = pyspark_df.select(api_columns).toPandas()
  #addresses = df[api_columns]

  temp_name = None

  try:

    # create temporary file for api
    with tempfile.NamedTemporaryFile(delete = False, mode = "w", suffix = ".csv", dir = temp_dir) as temp_file:
      temp_name = temp_file.name
      addresses.to_csv(
        temp_name, 
        index = False,
        header = False,
        index_label = False
        )
      
      # define api url
      api_url = "https://geocoding.geo.census.gov/geocoder/geographies/addressbatch"

      # define upload_file
      upload_file = {
        "addressFile": (os.path.basename(temp_name), open(temp_name, "rb"), "text/csv")
      }

      # define data to request from census
      data_request = {
        "benchmark": "Public_AR_Current",
        "vintage": "Census2020_Current"
        }
      
      # save census response data
      response = r.post(
        api_url, 
        files = upload_file, 
        data = data_request
        )
      
      # error if response !=200
      response.raise_for_status()

      # convert response data to pandas df
      return_df = pd.read_csv(
        io.StringIO(response.text),
        sep = ",",
        header = None,
        quoting = csv.QUOTE_ALL,
        on_bad_lines = go_to_jail,
        engine = "python"
      )

      # add in column names
      return_df.columns = geocoded_columns
      return_df_spark = spark.createDataFrame(return_df)
      return return_df_spark
    
  except Exception as e:
    print(f"error {str(e)}")
  
  finally:
    if temp_name and os.path.exists(temp_name):
      os.remove(temp_name)
      

   # merged = sample.alias("original").join(
    #     geocoded_spark.alias("updated"),
    #     on = "row_index",
    #     how = "left"
    # )
    
    # updated_columns = [
    #     when(col("original." + column).isNull(), col("updated." + column)).otherwise(col("original." + column)).alias(column)
    #     for column in sample.columns if column != "row_index"
    # ]

    # Create the updated DataFrame
    # sample = merged.select(*updated_columns, "original.row_index")



    # try:    
    #     query_results = query_api(chunk)

    #     geocoded_spark = spark.createDataFrame(query_results).withColumn("geocoder", lit("Census"))
    #     print("columns in geocoded_spark:", geocoded_spark.columns)

    #     if "row_index" not in geocoded_spark.columns:
    #         print("Warning: row_index is missing from query results.")
    #         continue
        
    #     merged = sample.join(
    #         geocoded_spark, 
    #         on="row_index", 
    #         how="left") \
    #         .select(
    #             sample["*"], 
    #             *[geocoded_spark[col] for col in geocoded_spark.columns if col != "row_index"]
    #         ) \
    #         .withColumn("geocoder", coalesce(sample.geocoder, geocoded_spark.geocoder)) 

    #     # Update sample to the newly updated DataFrame
    #     sample = merged

        
        # merged = sample.alias("original").join(
            # geocoded_spark.alias("updated"),
            # on = "row_index",
            # how = "left"
        # )
        # print("merged schema:", merged.schema)
        # print("merged_columns:", merged.columns)

        
        # geocoded_results = merged.select(
        #     col("original.row_index"),
        #     *[
        #         when(col(f"updated.{col_name}").isNotNull(), col(f"updated.{col_name}")).otherwise(col(f"original.{col_name}")).alias(col_name) for col_name in geocoded_columns
        #     ]
        # )

        # sample = geocoded_results

    #     print("updated sample schema: ", sample.schema)
    #     print("updated sample columns: ", sample.columns)
    # except Exception as e:
    #     print(f"There was a problem geocoding this chunk: {str(e)}")