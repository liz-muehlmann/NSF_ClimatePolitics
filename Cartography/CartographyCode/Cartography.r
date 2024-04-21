################################################################################
##                                                                            ##
## This file downloads original cartographic files from the USCensus          ##
##      using the tigris() package                                            ##
##                                                                            ##
################################################################################

# suppress warnings & increase java heap space #################################
options(warn = -1)

## load libraries ##############################################################
library(tidyverse)                 # data manipulation
library(arrow)                     # open and work with parquet filepaths
library(readtext)                  # read filepaths
library(tigris)                    # geographic boundaries
library(sf)                        # work with geographic boundaries 

## download and save county maps ###############################################
counties2010 <- counties(year = 2010, cb=TRUE) %>% 
    select(STATE,COUNTY,NAME,LSAD,geometry)  %>% 
    filter(STATE < 57)  %>% 
    mutate(full_fips = paste(STATE, COUNTY, sep = ""),
    county_name = paste(NAME, LSAD, sep = " "))  %>% 
    select(full_fips, county_name, geometry)
counties2020 <- counties(year = 2010, cb=TRUE)   %>% 
    select(STATE,COUNTY,NAME,LSAD,geometry)  %>% 
    filter(STATE < 57)  %>% 
    mutate(full_fips = paste(STATE, COUNTY, sep = ""),
    county_name = paste(NAME, LSAD, sep = " "))  %>% 
    select(full_fips, county_name, geometry)
st_write(counties2010, "./Cartography/CartographyData/Counties/2010_Census_USCounties.gpkg")
st_write(counties2020, "./Cartography/CartographyData/Counties/2020_Census_UScounties.gpkg")

# download original 2011 place files ###################################################################################
# for(s in stateList){
#         place <- places(state = s, year = 2011)  %>% 
#                  select(STATEFP, PLACEFP, GEOID, NAME, NAMELSAD, INTPTLAT, INTPTLON, geometry)
#         ###### uncomment to save to variable
#        assign(paste0(s, "_place"), place)
#         ##### uncomment to save to gpkg
#         st_write(place, 
#                 paste0("./Cartography/Place/2011/",
#                 paste(s),"_2011Place.gpkg"))
# }

# download original 2020 place files ###################################################################################
# for(s in stateList){
#         place <- places(state = s, year = 2020)  %>% 
#                  select(STATEFP, PLACEFP, GEOID, NAME, NAMELSAD, INTPTLAT, INTPTLON, geometry)
#         ###### uncomment to save to variable
#        # assign(paste0("LVPlace_", s), place)
#         ##### uncomment to save to gpkg
#         st_write(place, 
#                 paste0("./Cartography/Place/2020/",
#                 paste(s),"_2020Place.gpkg"))
# }

## download and save state geometry ####################################################################################
states <- states()  %>% 
          filter(STATEFP < 57)
st_write(states, "./Cartography/States/states.gpkg")
