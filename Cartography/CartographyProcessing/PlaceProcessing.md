# Place Processing  
This file includes detailed steps to complete each step to replicate the Place data.

1. [2020_CDPBoundaryIntersection](/Cartography/CartographyData/Place/2020_CDPBoundaryIntersection.csv)  
    * Download the 2020 national boundary file from the [US Census](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-geodatabase-file.2020.html#list-tab-1258746043).
    * Add the Census_Designated_Place and County layers to the map.     
        *note*: County boundaries in the 2020 national substate boundary file available from the U.S. Census merge Wisconsin and Michigan into one state (ignores the Great Lakes).
    * In the Geoprocessing tab, select *Tabulate Intersection.*  
        * Input Zone Features: CDP layer
            * Select the fields to keep (GEOID, NAMELSAD)
        * Input Class Features: County layer
            * Select the fields to keep (GEOID, NAMELSAD)
        * Sum Fields: Leave empty
        * Output Units: Unknown
        * Run. The output will be saved to the map.
    * Select the new data table under *Standalone Tables.* 
        * On the top bar select *Standalone Tables* and *Export Table.*
        * Change the file type to .csv under *Output Table.*