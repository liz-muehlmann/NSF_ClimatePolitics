################################################################################
##                                                                            ##
## This file uses boundary intersection statistics from ArcGIS to aggregate   ##
##     places to the county level.                                            ##
##                                                                            ##
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##    
## Political data was downloaded from the Harvard Dataverse for Algara &      ##
##    Sharif (2021) "Replication Data for: Partisanship & Nationalization     ##
##    in American Elections: Evidence from Presidential, Senatorial, &        ##
##    Gubernatorial Elections in the U.S. Counties, 1872-2020",               ##
##      https://doi.org/10.7910/DVN/DGUMFI                                    ##
##                                                                            ##    
## Boundary changes were downloaded from the US Census:                       ##            
##   https://www.census.gov/library/reference/code-lists/ansi.2010.html#place ##
##                                                                            ##
## Virginia county equivalent data was downloaded from the US Census:         ##
##   https://www.census.gov/library/reference/code-lists/ansi.2020.html#list-tab-230474085
##                                                                            ##
## Election + ACS + County data were merged in                                ##        
##      Code/Political/AlgaraACSPlaces.r                                      ##
##                                                                            ##
## ** Majority counties were determined using ArcGIS' Tabulate Intersection   ##
##    function and processed using Google Docs.                               ##  
##    Methodology is available upon request.                                  ##               
##                                                                            ##
################################################################################

### prelims ####################################################################
##                                                                            ##
##                             load packages                                  ##
##                                                                            ##
################################################################################
library(tidyverse)
library(tigris)
library(sf)

### majority county function ###################################################
##                                                                            ##
##                calculates the counties with the highest overlap            ##
##                                                                            ##
################################################################################
majority <- function(df){
    df %>% mutate(coverage = round(coverage, 2)) %>% 
        filter(coverage != 0) %>% 
        group_by(place_fips) %>% 
        filter(coverage == max(coverage, na.rm = TRUE))
}

### data #######################################################################
##                                                                            ##
##  IP/CDP csvs were saved using the tabulate intersection function in ArcGIS ##
##  lvPlaces is saved in Code/LocalView/processing.r                          ##
##                                                                            ##
################################################################################
ip <- majority(read.csv("./Data/Cartography/CartographyModified/2020_IPIntersections.csv"))
cdp <- majority(read.csv("./Data/Cartography/CartographyModified/2020_CDPIntersections.csv"))

## merge majority IP / CDP
ipcdp <- rbind(ip, cdp) %>% 
    mutate(place_fips = str_pad(place_fips, 7, "left", "0"),
           stcounty_fips = str_pad(stcounty_fips, 5, "left", 0),
           coverage = as.numeric(coverage)) 

# write.csv(ipcdp, "./Data/Cartography/CartographyModified/2020_IPCDPMajorityCounties.csv")

## local view places
lvPlaceOnly <- read.csv("./Data/LocalView/LVModifiedData/LVPlaceOnly.csv") %>% 
    select(-state_name) %>% 
    mutate(coverage = 100,
           place_fips = str_pad(place_fips, 7, "left", 0))

## boundary changes for the US
changes <- read.csv("./Data/Cartography/CartographyOriginal/2020_PlaceBoundaryChanges.csv") %>% 
    mutate(state_fips = str_pad(state_fips, 2, "left", 0),
           placeOnly_fips = str_pad(place_fips, 5, "left", 0),
           place_fips = paste(state_fips, placeOnly_fips, sep=""),
           place_fips = str_pad(place_fips, 7, "left", 0),
           coverage = 100.00)%>% 
    select(state_fips, place_fips, place_name,  county_name, coverage)

## Virginia county equivalents for Hampton City (5151650), Petersburg City (5151730), & Norfolk City (5151710)
va <- read.csv("./Data/Cartography/CartographyOriginal/2020_VACountyEquivalents.csv") %>% 
    mutate(county_fips = str_pad(county_fips, 3, "left", 0),
           stcounty_fips = paste(state_fips, county_fips, sep=""),
           coverage = 100.00,
           place_name = NA,
           place_fips = NA) %>% 
    select(-state_abbr, -state_fips,-county_fips)

## 2020 county boundaries
counties20 <- counties(year = 2020) %>% 
    st_drop_geometry() %>% 
    rename(state_fips = STATEFP,
           county_fips = COUNTYFP,
           stcounty_fips = GEOID,
           county_name = NAMELSAD) %>% 
    select(state_fips, county_fips, stcounty_fips, county_name) 

### merge & save ###############################################################
##                                                                            ##
##                     merge and save aggregated data                         ##
##                                                                            ##
################################################################################

## merge county boundaries to 2020 boundary changes
countyChanges <- left_join(changes, counties20, by = c("state_fips", "county_name"))

## merge Virginia places with ipcdp
places <- rbind(ipcdp, va) %>% 
    left_join(counties20, by = c("stcounty_fips", "county_name"))

## places aggregated to the county level
places_full <- rbind(countyChanges, places) %>% distinct()

# write.csv(places_full, "./Data/Cartography/CartographyModified/2020_AllPlaces.csv")
