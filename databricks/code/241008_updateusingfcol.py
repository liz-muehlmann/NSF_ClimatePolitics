updated_sample = sample.alias("s").join(
        geocoded_spark.alias("g"),
        on="row_index",  # Use the unique identifier
        how="left"
    ).select(
        when(col("g.geocoder").isNotNull(), col("g.geocoder")).otherwise(col("s.geocoder")).alias("geocoder"),
        when(col("g.address_in").isNotNull(), col("g.address_in")).otherwise(col("s.address_in")).alias("address_in"),
        when(col("g.match_flag").isNotNull(), col("g.match_flag")).otherwise(col("s.match_flag")).alias("match_flag"),
        when(col("g.match_type").isNotNull(), col("g.match_type")).otherwise(col("s.match_type")).alias("match_type"),
        when(col("g.address_out").isNotNull(), col("g.address_out")).otherwise(col("s.address_out")).alias("address_out"),
        when(col("g.long_lat").isNotNull(), col("g.long_lat")).otherwise(col("s.long_lat")).alias("long_lat"),
        when(col("g.tiger_edge").isNotNull(), col("g.tiger_edge")).otherwise(col("s.tiger_edge")).alias("tiger_edge"),
        when(col("g.street_side").isNotNull(), col("g.street_side")).otherwise(col("s.street_side")).alias("street_side"),
        when(col("g.state_fips").isNotNull(), col("g.state_fips")).otherwise(col("s.state_fips")).alias("state_fips"),
        when(col("g.county_fips").isNotNull(), col("g.county_fips")).otherwise(col("s.county_fips")).alias("county_fips"),
        when(col("g.tract_id").isNotNull(), col("g.tract_id")).otherwise(col("s.tract_id")).alias("tract_id"),
        when(col("g.block_id").isNotNull(), col("g.block_id")).otherwise(col("s.block_id")).alias("block_id"),
    )

    # Overwrite the original DataFrame with the updated one
    updated_sample.createOrReplaceTempView("sample")

    # Drop the temporary view after the update
    spark.catalog.dropTempView("temp_geocoded")