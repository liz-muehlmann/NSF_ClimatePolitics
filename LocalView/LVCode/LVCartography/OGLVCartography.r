################################################################################
##                                                                            ##
## This file handles the preliminary steps necessary for adding               ##
##      geographic boundaries to the Local View data                          ##  
##                                                                            ##  
## Local View data is available here:                                         ##
##      https://doi.org/10.7910/DVN/NJTBEM                                    ##
##                                                                            ##     
## Geographic boundaries are downloaded tigris                                ##
##                                                                            ##
################################################################################

# suppress warnings & set source ###############################################
options(warn = -1)
source("./LocalView/LVCode/LVCartography/LVCartographyPrelim.r")

## load libraries ##############################################################
library(tidyverse)                 # data manipulation
library(arrow)                     # open and work with parquet filepaths
library(readtext)                  # read filepaths
library(tigris)                    # geographic boundaries
library(sf)                        # work with geographic boundaries 


## load county election data ###################################################
election <- read.csv("./Political/2000_2020_County_Presidential.csv")  %>% 
            select(year, State, state_po, county_fips, county_name, office, party, candidatevotes, totalvotes, mode)  %>% 
            filter(year >= 2008)

## download county boundaries ##################################################
counties2010 <- st_transform(tigris::counties(year = 2010), crs = "+proj=longlat +datum=WGS84")  %>% 
                processCounties()

counties2020 <- st_transform(tigris::counties(year = 2010), crs = "+proj=longlat +datum=WGS84")  %>% 
                processCounties()

limitedC2010 <- counties2010  %>% 
            select(STATEFP10, COUNTYFP10, NAMELSAD10)  %>% 
            st_drop_geometry()

limitedC2020 <- counties2020  %>% 
            select(STATEFP10, COUNTYFP10, NAMELSAD10)  %>% 
            st_drop_geometry()  

# load place data ##############################################################
places2010 <- read.csv("./Cartography/Place/2010_USPlaceByCounty.csv")  %>% 
              processPlaces()
             
places2020 <- read.csv("./Cartography/Place/2020_USPlaceByCounty.csv")  %>% 
              processPlaces()


# load local view data #########################################################
all_docs <- open_dataset("./LocalView/LVOriginalData/meetings")  
all_docs <- Scanner$create(all_docs)
all_docs <- all_docs$ToTable()
all_docs <- as.data.frame(all_docs)   %>% 
            filter(!(caption_text_clean == "<No caption available>")) %>% 
            mutate(year = str_sub(meeting_date, start = 1, end = 4))   %>% 
            rename(sp_fips = st_fips,
                   NAMELSAD10 = place_name)  %>% 
            mutate(STATEFP10 = str_sub(sp_fips, start = 1, end = 2))  %>%
            mutate(place_fips = str_sub(sp_fips, start = 3, end=7))  %>% 
            select(!(sp_fips)) 


# subset by year ###############################################################
allDocs2010 <- all_docs  %>% 
               filter(year >=2010 & year <= 2020)
allDocs2020 <- all_docs  %>% 
               filter(year >=2021 & year <=2023) 

# merge place and county boundaries ############################################
pc2010 <- mergePC(places2010, limitedC2010) 
pc2020 <- mergePC(places2020, limitedC2020)

# merge place, county, and local view ##########################################
pclv2010 <- left_join(allDocs2010, pc2010, by=c("STATEFP10", "NAMELSAD10" = "placename","place_fips"= "placefp"), keep=FALSE)
pclv2020 <- left_join(allDocs2020, pc2020, by=c("STATEFP10", "NAMELSAD10" = "placename","place_fips"= "placefp"), keep=FALSE)  

# separate by whether place splits over multiple counties ######################
pc1_2010 <- oneCounty(pc2010)  # was pclv2010
nrow(pc1_2010) # 57,491
pc2_2010 <- twoCounties(pc2010)
nrow(pc2_2010) # 6,012
pc3_2010 <- threeCounties(pc2010)
nrow(pc3_2010) # 965
pc4_2010 <- fourCounties(pc2010)
nrow(pc4_2010) # 340
pc5_2010 <- fiveCounties(pc2010)
nrow(pc5_2010) # 44

pc1_2020 <- oneCounty(pc2020)  #was pclv2020
nrow(pc1_2020) # 42,360
pc2_2020 <- twoCounties(pc2020)
nrow(pc2_2020) # 3,909
pc3_2020 <- threeCounties(pc2020)
nrow(pc3_2010) # 965
pc4_2020 <- fourCounties(pc2020)
nrow(pc4_2020) # 235
pc5_2020 <- fiveCounties(pc2020)
nrow(pc5_2020) # 4

pc1_geo2010 <-  left_join(pc1_2010, counties2010, by = c("STATEFP10", "county1fips" = "COUNTYFP10", "NAMELSAD10"))
pc2_geo2010 <-  left_join(pc2_2010, counties2010, by = c("STATEFP10", "county2fips" = "COUNTYFP10", "NAMELSAD10"))
pc3_geo2010 <-  left_join(pc3_2010, counties2010, by = c("STATEFP10", "county3fips" = "COUNTYFP10", "NAMELSAD10"))
pc4_geo2010 <-  left_join(pc4_2010, counties2010, by = c("STATEFP10", "county4fips" = "COUNTYFP10", "NAMELSAD10"))
pc5_geo2010 <-  left_join(pc5_2010, counties2010, by = c("STATEFP10", "county5fips" = "COUNTYFP10", "NAMELSAD10"))

pc1_geo2020 <-  left_join(pc1_2020, counties2020, by = c("STATEFP10", "county1fips" = "COUNTYFP10", "NAMELSAD10"))
pc2_geo2020 <-  left_join(pc2_2020, counties2020, by = c("STATEFP10", "county2fips" = "COUNTYFP10", "NAMELSAD10"))
pc3_geo2020 <-  left_join(pc3_2020, counties2020, by = c("STATEFP10", "county3fips" = "COUNTYFP10", "NAMELSAD10"))
pc4_geo2020 <-  left_join(pc4_2020, counties2020, by = c("STATEFP10", "county4fips" = "COUNTYFP10", "NAMELSAD10"))
pc5_geo2020 <-  left_join(pc5_2020, counties2020, by = c("STATEFP10", "county5fips" = "COUNTYFP10", "NAMELSAD10"))

pc1_geo2008 <- e2008(pc1_geo2010)
pc1_geo2012 <- e2012(pc1_geo2010)
pc1_geo2016 <- e2016(pc1_geo2010)
pc1_geo2020 <- e2020(pc1_geo2010)

pc2_geo2008 <- e2008(pc2_geo2010)
pc2_geo2012 <- e2012(pc2_geo2010)
pc2_geo2016 <- e2016(pc2_geo2010)
pc2_geo2020 <- e2020(pc2_geo2010)

pc3_geo2008 <- e2008(pc3_geo2010)
pc3_geo2012 <- e2012(pc3_geo2010)
pc3_geo2016 <- e2016(pc3_geo2010)
pc3_geo2020 <- e2020(pc3_geo2010)

pc4_geo2008 <- e2008(pc4_geo2010)
pc4_geo2012 <- e2012(pc4_geo2010)
pc4_geo2016 <- e2016(pc4_geo2010)
pc4_geo2020 <- e2020(pc4_geo2010)



# suppress warnings & set source ###############################################
options(warn = -1)
source("./LocalView/LVCode/LVCartography/LVCartographyPrelim.r")

## load data ###################################################################
# states
states <- states()  %>% 
       select(GEOID, STUSPS, NAME)  %>% 
       rename(state_fips = GEOID,
              state_abbr = STUSPS,
              state_name = NAME)  %>% 
       st_drop_geometry()

# counties
c10 <- counties(year = 2010)  %>% 
       select(STATEFP10, COUNTYFP10, GEOID10, NAMELSAD10, INTPTLAT10, INTPTLON10, geometry)  %>% 
       rename(state_fips = STATEFP10,
              county_fips = COUNTYFP10,
              full_fips = GEOID10,
              county_name = NAMELSAD10)  %>% 
       filter(state_fips < 57)

c20 <- counties(year = 2020)  %>% 
       select(STATEFP, COUNTYFP, GEOID, NAMELSAD, INTPTLAT, INTPTLON, geometry)  %>% 
       rename(state_fips = STATEFP,
              county_fips = COUNTYFP,
              full_fips = GEOID,
              county_name = NAMELSAD)  %>% 
       filter(state_fips < 57)

c10limited <- c10  %>% 
       select(-INTPTLAT10, -INTPTLON10, -full_fips)  %>% 
       st_drop_geometry()

c20limited <- c20  %>% 
       select(-INTPTLAT, -INTPTLON, -full_fips)  %>% 
       st_drop_geometry()

# places by county
pc10 <- processPlaces(read.csv("./Cartography/Place/2010_PlacebyCounty.csv")) 
pc20 <- processPlaces(read.csv("./Cartography/Place/2020_PlacebyCounty.csv")) 

# local view
all_docs <- open_dataset("./LocalView/LVOriginalData/meetings")  
all_docs <- Scanner$create(all_docs)
all_docs <- all_docs$ToTable()
all_docs <- as.data.frame(all_docs)   %>% 
            filter(!(caption_text_clean == "<No caption available>")) %>% 
            mutate(year = str_sub(meeting_date, start = 1, end = 4), 
                   state_fips = str_sub(st_fips, start = 1, end = 2),
                   place_fips = str_sub(st_fips, start = 3, end=7))  %>% 
            select(-st_fips, -acs_18_amind, -acs_18_asian, -acs_18_black, -acs_18_median_age, -acs_18_median_gross_rent, -acs_18_median_hh_inc, -acs_18_nhapi, -acs_18_pop, -acs_18_white, -acs_18_hispanic, -caption_text)              

ad10 <- all_docs  %>% 
       filter(year >=2010 & year <= 2014)
ad15 <- all_docs  %>% 
       filter(year >= 2015 & year <= 2020)
ad20 <- all_docs  %>% 
       filter(year >=2021 & year <=2023) 

# elections
e12 <- processElections(read_sf("./Political/PoliticalModifiedData/Geopackages/us1216_election.gpkg")) %>% 
       select(-state_abbr, -state_name, -DEM, -REP, -total_votes, -county_name, -state_fips) 
e20 <- processElections(read_sf("./Political/PoliticalModifiedData/Geopackages/us20_election.gpkg"))   %>% 
       select(-state_abbr, -state_name, -DEM, -REP, -total_votes, -county_name, -state_fips)

e1216 <- e12  %>% 
       pivot_wider(
       names_from = year,
       values_from = c(DVP, RVP))  %>% 
       select(-county_fips)

e2020 <- e20 %>% 
       rename(DVP_2020 = DVP,
              RVP_2020 = RVP)  %>% 
       select(-county_fips)


# acs
acsvars <- c("B02001_002E", "B02001_003E", "B02001_004E", "B02001_005E", "B02001_006E", "B03001_003E", "B01002_001E", "B25064_001E", "B19001_001E","B01003_001E")
acs10 <- processACS(get_acs(geography = "county", year = 2010, geometry = FALSE, variables = acsvars))
       
acs15 <- processACS(get_acs(geography = "county", year = 2015, geometry = FALSE, variables = acsvars))

acs20 <- processACS(get_acs(geography = "county", year = 2020, geometry = FALSE, variables = acsvars))


