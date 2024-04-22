################################# FILE DESCRIPTION #############################
##                                                                            ##
##  This file handles the preliminary steps necessary for                     ##  
##    the political data                                                      ##
##                                                                            ##
##  Geography data was downloaded using tigiris                               ##
##                                                                            ##
##  Political data was downloaded from the Harvard Dataverse for Algara &     ##
##    Sharif (2021) "Replication Data for: Partisanship & Nationalization     ##
##    in American Elections: Evidence from Presidential, Senatorial, &        ##
##    Gubernatorial Elections in the U.S. Counties, 1872-2020",               ##
##      https://doi.org/10.7910/DVN/DGUMFI                                    ##
##                                                                            ##
################################################################################

## suppress warnings ###########################################################
options(warn = -1)

## load libraries ##############################################################
library(tidyverse)                 # data manipulation
library(tigris)                    # geographic boundaries
library(sf)                        # work with geographic boundaries 

## cartographic data ###########################################################
# load county data, filter out US territories, and Alaska
counties00 <- counties(year = 2000, cb=TRUE)  %>% 
    filter(STATEFP < 57 & STATEFP != "02")   %>% 
    rename(state_fips = STATEFP, 
           county_fips = COUNTYFP,
           county_name = NAME)  %>% 
    mutate(full_fips = paste(state_fips, county_fips, sep=""),
           county_name = paste(county_name, "County", sep = " "))  %>% 
    select(full_fips, state_fips, county_fips, county_name, geometry)

counties10 <- counties(year = 2020, cb=TRUE)  %>% 
    filter(STATEFP < 57 & STATEFP != "02")  %>% 
    rename(state_fips = STATEFP,
           county_fips = COUNTYFP,
           full_fips = GEOID,
           county_name = NAMELSAD,
           state_abbr = STUSPS,
           state_name = STATE_NAME)  %>% 
    select(full_fips, state_fips, county_fips, state_abbr, state_name, county_name, geometry)

states <- states()  %>% 
    filter(STATEFP < 57 & STATEFP != "02")  %>% 
    select(GEOID, STUSPS, NAME)  %>% 
    rename(state_fips = GEOID,
           state_abbr = STUSPS,
           state_name = NAME)  %>% 
    st_drop_geometry()

place_county <- read.csv("./Cartography/Place/2020_MajorityCounties.csv") %>% 
  select(place_fips, place_name, full_fips, coverage_percentage) %>% 
  mutate(full_fips = as.character(full_fips),
         full_fips = str_pad(full_fips, 5, side="left", 0))

## election data ###############################################################
load("./Political/PoliticalOriginalData/AlgaraSharif_all elections.Rdata")

algara <- pres_elections_release %>% 
  filter(election_year >=2008 & office == "PRES" & election_type == "G") %>% 
  select(election_year, fips, democratic_raw_votes, republican_raw_votes, raw_county_vote_totals) %>% 
  rename(year = election_year, 
         full_fips = fips,
         DEM = democratic_raw_votes,
         REP = republican_raw_votes,
         total_votes = raw_county_vote_totals) %>% 
  mutate(DVP = (DEM/total_votes)*100,
         RVP = (REP/total_votes)*100) 

## 2008 ########################################################################
algara08 <- algara %>% 
  filter(year == 2008)

e08geo <- left_join(algara08, counties00, by = "full_fips")

## 2016 ########################################################################
algara1216 <- algara %>% 
  filter(year == 2012 | year == 2016) 
algara1216$full_fips <- replace(algara1216$full_fips, algara1216$full_fips == 46113, 46102)

e1216geo <- left_join(algara1216, counties10, by = "full_fips")

## 2020 ########################################################################
algara20 <- algara %>% 
  filter(year == 2020)
algara20$full_fips <- replace(algara20$full_fips, algara20$full_fips == 46113, 46102)

e20geo <- left_join(algara20, counties10, by = "full_fips")  

e20placegeo <- left_join(place_county, counties10, by = "full_fips")
# ## write geopackages ###########################################################
# st_write(e08geo, "./Political/PoliticalModifiedData/Geopackages/us08_election.gpkg")
# st_write(e1216geo, "./Political/PoliticalModifiedData/Geopackages/us1216_election.gpkg")
# st_write(e20geo, "./Political/PoliticalModifiedData/Geopackages/us20_election.gpkg")
# 
