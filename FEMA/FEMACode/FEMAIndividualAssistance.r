################################################################################
##                                                                            ##
##                                                                            ##      
## This file is used combine the FEMA individual assistance data for Texas    ##  
##    and the US with zip code level geography data.                          ##  
##                                                                            ##      
## Zip Code data is available here:                                           ##
## https://hub.arcgis.com/datasets/d6f7ee6129e241cc9b6f75978e47128b/explore   ##
##                                                                            ##
## FEMA data is available here:                                               ##     
## https://www.fema.gov/about/openfema/data-sets#individual                   ##
##                                                                            ##
################################################################################

## load library ################################################################
library(tidyverse)                 # data manipulation
library(sf)                        # working with shapefiles

## load data ###################################################################
fema <- read.csv("./Original_Data/Climate/2024 - FEMA - Housing Assistance/HousingAssistanceOwners.csv")
zip <- read_sf("./Cartography/CartographyData/Zip_Code/2024_ArcGIS_ZipCodes.geojson") 

fema_tx <- fema  %>% filter(state == "TX")

## merge data
fema_zip <- left_join(fema, zip, by = "zipCode", relationship = "many-to-many")
femazip_tx <- left_join(fema_tx, zip, by ="zipCode", relationship = "many-to-many")

## write data
# write_csv(fema_zip, "./Modified_Data/Cartographic/fema_zip.csv")
# write_csv(femazip_tx, "./Modified_Data/Cartographic/femazip_tx.csv")
