# Cartography code

1. [Cartography.r](/Cartography/CartographyCode/Cartography.r)  
Downloads state, county, and place boundaries (2010 & 2020) using the tigris package in R. Reprojects the data to CRS WGS84. 

2. [TXLeaflet.r](/Cartography/CartographyCode/TXLeaflet.r)
Maps Texas county, precinct, and census block data. County and Census Block data is downloaded using the tigris package in R. Precinct data is available from the [Texas data portal](https://data.capitol.texas.gov/dataset/precincts). Map is created using leaflet for R.

3. [TXPopChoropleth.r](/Cartography/CartographyCode/TXPopChoropleth.r)  
Downloads and creates choropleth map of Texas population using U.S. Census data downloaded using the tidycensus package in R.