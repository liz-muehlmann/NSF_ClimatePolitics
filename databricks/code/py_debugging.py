    ############################# DEBUGGING BEFORE MERGE #############################
    ## REPORT NUMBER OF COLUMNS WHERE GEOCODER = NULL
    # rows_before = spark.sql("SELECT COUNT(*) FROM main.weber_lab.sample WHERE geocoder IS NULL").first()[0]
    # logging.info(f"Rows with geocoder IS NULL before merge: {rows_before}")

    ## USE TEMPORARY TABLE
    # geocoded_spark.createOrReplaceTempView("geocoded_temp")
    # logging.info("Temporary view 'geocoded_temp' created.")
    # temp_view_exists = spark.sql("SHOW TABLES LIKE 'geocoded_temp'").count() > 0
    # logging.info(f"Temporary view exists: {temp_view_exists}")

    ## CHECK IF "GEOCODER" COLUMN IS BEING UPDATED
    # spark.sql("""
    # SELECT original.row_index 
    # FROM main.weber_lab.sample AS original
    # JOIN main.weber_lab.geocoded_spark AS updated
    # ON original.row_index = updated.row_index
    # """)  

    ## VIEW DIFFERENCES BETWEEN SAMPLE AND GEOCODED_SPARK
    # spark.sql("DESCRIBE main.weber_lab.sample").show()
    # spark.sql("DESCRIBE main.weber_lab.geocoded_spark").show()

    ## SHOW ROWS THAT HAVE DIFFERENT LENGTH ROW INDEXES
    # spark.sql("SELECT row_index FROM main.weber_lab.sample WHERE LENGTH(row_index) != LENGTH(TRIM(row_index))").show()
    # spark.sql("SELECT row_index FROM main.weber_lab.geocoded_spark WHERE LENGTH(row_index) != LENGTH(TRIM(row_index))").show()

    ## CHECK IF THERE ARE DUPLICATE ROW INDICES
    # spark.sql("SELECT DISTINCT row_index FROM main.weber_lab.sample").show()
    # spark.sql("SELECT DISTINCT row_index FROM main.weber_lab.geocoded_spark").show()
    
    ## CHECK WHETHER GEOCODER HAS BEEN UPDATED
    # rows_before = spark.sql("SELECT COUNT(*) FROM main.weber_lab.sample WHERE geocoder IS NULL").first()[0]
    # updated_count = spark.sql("SELECT COUNT(*) FROM main.weber_lab.geocoded_spark").first()[0]
    # logging.info(f"Count before merge - original: {rows_before}, updated: {updated_count}")



############################# DEBUGGING AFTER MERGE #############################
# spark.sql("SELECT row_index, geocoder FROM main.weber_lab.sample WHERE geocoder = 'Census' LIMIT 2").show()
    
