# [Cartography Code](/Cartography/CartographyCode/)

1. [Cartography.r](/Cartography/CartographyCode/Cartography.r)  
Downloads state, county, and place boundaries (2010 & 2020) using the tigris package in R. Reprojects the data to CRS WGS84. 

2. [TXLeaflet.r](/Cartography/CartographyCode/TXLeaflet.r)
Maps Texas county, precinct, and census block data. County and Census Block data is downloaded using the tigris package in R. Precinct data is available from the [Texas data portal](https://data.capitol.texas.gov/dataset/precincts). Map is created using leaflet for R.

3. [TXPopChoropleth.r](/Cartography/CartographyCode/TXPopChoropleth.r)  
Downloads and creates choropleth map of Texas population using U.S. Census data downloaded using the tidycensus package in R.

# [Cartography Data](/Cartography/CartographyData/)

1. Census_NationalSubstateBoundaries**   
    * Includes all national substate boundaries as defined by the United States Census for 2020, 2016, and 2014.  
    [Data](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-geodatabase-file.2020.html#list-tab-1258746043).  
2. Counties** 
    * Includes 2010 & 2020 county boundaries, downloaded using the tigris package in r, and reprojected into WGS 84.   
    [Code](/Cartography/CartographyCode/Cartography.r) | [Methodology](/Methodology/CountiesProcessing.md)
3. Place 
    * [2020_CDPBoundaryIntersection](/Cartography/CartographyData/Place/2020_CDPBoundaryIntersection.csv) includes a list of Census Designated Places across the United States by Place FIPS and name and calculates the percentage that overlaps with county boundaries. 
    * [2020_IncPlaceBoundaryIntersection](/Cartography/CartographyData/Place/2020_IncPlaceBoundaryIntersection.csv) includes a list of Incorporated Places across the United States by Place FIPS and name and calculates the percentage that overlaps with county boundaries. 
    * [read me](/Cartography/CartographyData/Place/PlaceReadMe.md) | [Methodology](/Methodology/PlaceProcessing.md)
4. Zip Code**
    * 2024 boundary file for United States Zip Codes for use with the [FEMA](/FEMA/) data.  
    [Data](https://hub.arcgis.com/datasets/esri::usa-zip-code-boundaries/about) | [Code](/FEMA/FEMACode/FEMAIndividualAssistance.r) | [Methodology](/Methodology/ZipCodeProcessing.md)

# [Cartography Maps](/Cartography/CartographyMaps/)  

1. [2020_TX_BlockGroup-Precincts](/Cartography/CartographyMaps/2020_TX_BlockGroup-Precincts.pdf)    
    * Census block groups are displayed in grey, precincts are displayed in blue.
2. [2020_TX_CountiesPrecincts](/Cartography/CartographyMaps/2020_TX_CountiesPrecincts.pdf)  
    * Counties are displayed in grey, precincts are displayed in red.
3. [2020_TX_PrecinctBlock](/Cartography/CartographyMaps/2020_TX_PrecinctBlock.pdf)  
    * Blocks are displayed in red, precincts are displayed in grey.
4. [2020_PrecinctVTD](/Cartography/CartographyMaps/2020_TX_PrecinctVTD.pdf)
    * Precincts and VTDs align and are displayed in green.




** these files or folders are tracked using [DVC](dvc.org) due to github's large file storage limits.