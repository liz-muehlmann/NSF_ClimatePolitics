## file description ################################################################################
##                                                                                                ##
## This file handles the geography data needed to for working with the local view data            ##
##          Incorporated/Census Designated Places (2023)                                          ## 
##              IP/CDP csvs were saved using the tabulate intersection function in ArcGIS         ##
##          County Subdivisions                                                                   ##
##              https://www.census.gov/library/reference/code-lists/ansi.html#cousub              ##
##          Counties (2010, 2015, 2020)                                                           ##
##              tigris                                                                            ##
##          States                                                                                ##
##              tigris                                                                            ## 
##                                                                                                ##
## Output:                                                                                        ##    
##          None                                                                                  ##
####################################################################################################

#   ________________________________________________________________________________________________
#   load libraries and custom functions                                                         ####
source("./LocalView/code/processing_scripts/preliminaries.r")
library(tigris)                                    # download geographic boundaries
library(sf)                                        # work with shapefiles

#   ________________________________________________________________________________________________
##  state boundaries                                                                            ####

statesGeo <- states()
st_crs(statesGeo) <- "EPSG:4326"
statesGeo <- statesGeo %>% 
    rename(state_fips = STATEFP,
           state_name = NAME,
           census_region = REGION,
           census_division = DIVISION) %>%
    select(state_fips, state_name, census_division, census_region, geometry) %>% 
    excludeStates()

states <- statesGeo %>% 
    st_drop_geometry()

#   ________________________________________________________________________________________________
##  county boundaries                                                                           ####

countiesGeo <- counties(year = 2020, cb = TRUE) %>% 
    rename(stcounty_fips = GEOID,
           county_name = NAMELSAD,
           state_fips = STATEFP,
           county_fips = COUNTYFP) %>% 
    fixCounties2020() %>% 
    excludeStates() %>% 
    left_join(states) %>% 
    group_by(state_name) %>% 
    mutate(n_countiesInState = n_distinct(stcounty_fips)) %>% 
    select(stcounty_fips, state_name, state_fips, county_fips, county_name, 
           census_division, census_region, n_countiesInState) %>% 
    ungroup()

counties <- countiesGeo %>% 
    st_drop_geometry()

#   ________________________________________________________________________________________________
##  county subdivisions                                                                         ####

countySub <- read.csv("./GIS/modified/2020_SubcountyDivisions.csv")  %>% 
    padFips() %>% 
    excludeStates() %>% 
    mutate(place_fips = paste(state_fips, countysub_fips, sep="")) %>% 
    createFips() %>% 
    select(stcounty_fips, place_fips, county_name)

#   ________________________________________________________________________________________________
##  incorporated and census designated places                                                   ####

ip <- majority(read.csv("./GIS/modified/2020_IPIntersections.csv")) %>% 
    padFips() %>% 
    createFips()
cdp <- majority(read.csv("./GIS/modified/2020_CDPIntersections.csv")) %>% 
    padFips() %>% 
    createFips()
ipcdp <- rbind(ip, cdp) %>% 
    select(-place_name, -coverage)

