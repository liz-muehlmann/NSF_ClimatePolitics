########################### FILE DESCRIPTION ###################################
##                                                                            ##
## This file adds ACS, Presidential Election returns, and county geometry     ##
##      to the Local View data                                                ##  
##                                                                            ##  
## Local View data is available here:                                         ##
##      https://doi.org/10.7910/DVN/NJTBEM                                    ##
##                                                                            ##     
## County geographic boundaries are downloaded tigris                         ##
##                                                                            ##
## Places by county are downloaded for 2010 & 2020 from the US Census         ##
##      https://www.census.gov/library/reference/code-lists/ansi.html         ##
##                                                                            ##
## ACS data is downloaded using tidycensus                                    ##
##                                                                            ##
################################################################################

## suppress warnings and set source
setwd("F:/GitHub/NSF_Climate_LocalPolitics")
options(warn = -1)
source("./LocalView/LVCode/LVCartography/LVCartographyPrelim.r")

## load libraries ##################################################################################
library(tidyverse)                 # data manipulation
library(arrow)                     # open and work with parquet filepaths
library(readtext)                  # read filepaths
library(tigris)                    # geographic boundaries
library(sf)                        # work with geographic boundaries 
library(tidycensus)                # ACS data
library(readr)                     # wildcards


## election data ###################################################################################
e08 <- processElections(read_sf("./Political/PoliticalModifiedData/Geopackages/us08_election.gpkg")) %>% 
  select(full_fips, DVP_2008, RVP_2008, total_votes_2008) 

e1216 <- read_sf("./Political/PoliticalModifiedData/Geopackages/us1216_election.gpkg")

e12 <- e1216 %>% filter(year == 2012) %>% 
  processElections() %>% 
  select(full_fips, DVP_2012, RVP_2012, total_votes_2012)

e16 <- e1216 %>% filter(year == 2016) %>% 
  processElections() %>% 
  select(full_fips, DVP_2016, RVP_2016, total_votes_2016)

e20 <- processElections(read_sf("./Political/PoliticalModifiedData/Geopackages/us20_election.gpkg")) %>% 
  select(full_fips, DVP_2020, RVP_2020, total_votes_2020) %>% 
  rename(DVP20 = DVP_2020,
         RVP20 = RVP_2020,
         TV20 = total_votes_2020) 
 
e0812 <- left_join(e08, e12) 
e0816 <- left_join(e0812, e16)  %>%  
  rename(DVP08 = DVP_2008,
         RVP08 = RVP_2008,
         TV08 = total_votes_2008,
         DVP12 = DVP_2012,
         RVP12 = RVP_2012,
         TV12 = total_votes_2012,
         DVP16 = DVP_2016,
         RVP16 = RVP_2016,
         TV16 = total_votes_2016) 

## places ##########################################################################################
places2010 <- read.csv("./Cartography/Place/2010_PlacebyCounty.csv")  %>% 
  processPlaces() 
 
places2020 <- read.csv("./Cartography/Place/2020_PlacebyCounty.csv")  %>% 
  processPlaces()

one10 <- oneCounty(places2010) %>% 
  left_join(e0816, by = c("c1Full" = "full_fips")) %>% 
  rename(c1_DVP08 = DVP08,
         c1_RVP08 = RVP08,
         c1_TV08 = TV08,
         c1_DVP12 = DVP12,
         c1_RVP12 = RVP12,
         c1_TV12 = TV12,
         c1_DVP16 = DVP16,
         c1_RVP16 = RVP16,
         c1_TV16 = TV16)

two10 <- twoCounties(places2010) %>% 
  left_join(e0816, by = c("c2Full" = "full_fips")) %>% 
  rename(c2_DVP08 = DVP08,
         c2_RVP08 = RVP08,
         c2_TV08 = TV08,
         c2_DVP12 = DVP12,
         c2_RVP12 = RVP12,
         c2_TV12 = TV12,
         c2_DVP16 = DVP16,
         c2_RVP16 = RVP16,
         c2_TV16 = TV16)  

three10 <- threeCounties(places2010) %>% 
  left_join(e0816, by = c("c3Full" = "full_fips")) %>% 
  rename(c3_DVP08 = DVP08,
         c3_RVP08 = RVP08,
         c3_TV08 = TV08,
         c3_DVP12 = DVP12,
         c3_RVP12 = RVP12,
         c3_TV12 = TV12,
         c3_DVP16 = DVP16,
         c3_RVP16 = RVP16,
         c3_TV16 = TV16) 

four10 <- fourCounties(places2010) %>% 
  left_join(e0816, by = c("c4Full" = "full_fips")) %>% 
  rename(c4_DVP08 = DVP08,
         c4_RVP08 = RVP08,
         c4_TV08 = TV08,
         c4_DVP12 = DVP12,
         c4_RVP12 = RVP12,
         c4_TV12 = TV12,
         c4_DVP16 = DVP16,
         c4_RVP16 = RVP16,
         c4_TV16 = TV16) 

five10 <- fiveCounties(places2010) %>%
  left_join(e0816, by = c("c5Full" = "full_fips")) %>% 
  rename(c5_DVP08 = DVP08,
         c5_RVP08 = RVP08,
         c5_TV08 = TV08,
         c5_DVP12 = DVP12,
         c5_RVP12 = RVP12,
         c5_TV12 = TV12,
         c5_DVP16 = DVP16,
         c5_RVP16 = RVP16,
         c5_TV16 = TV16) 
  






















