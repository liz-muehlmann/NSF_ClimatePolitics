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
library(tidycensus)                # ACS data

## cartographic data ###########################################################
# load county data, filter out US territories, and Alaska
# counties00 <- counties(year = 2000, cb=TRUE)  %>% 
#     filter(STATEFP < 57 & STATEFP != "02")   %>% 
#     rename(state_fips = STATEFP, 
#            county_fips = COUNTYFP,
#            county_name = NAME)  %>% 
#     mutate(full_fips = paste(state_fips, county_fips, sep=""),
#            county_name = paste(county_name, "County", sep = " "))  %>% 
#     select(full_fips, state_fips, county_fips, county_name, geometry)
# 
# counties10 <- counties(year = 2020, cb=TRUE)  %>% 
#     filter(STATEFP < 57 & STATEFP != "02")  %>% 
#     rename(state_fips = STATEFP,
#            county_fips = COUNTYFP,
#            full_fips = GEOID,
#            county_name = NAMELSAD,
#            state_abbr = STUSPS,
#            state_name = STATE_NAME)  %>% 
#     select(full_fips, state_fips, county_fips, state_abbr, state_name, county_name, geometry)
# 
# states <- states()  %>% 
#     filter(STATEFP < 57 & STATEFP != "02")  %>% 
#     select(GEOID, STUSPS, NAME)  %>% 
#     rename(state_fips = GEOID,
#            state_abbr = STUSPS,
#            state_name = NAME)  %>% 
#     st_drop_geometry()
# 
# place_county <- read.csv("./Cartography/Place/2020_MajorityCounties.csv") %>% 
#   select(place_fips, place_name, full_fips, coverage_percentage) %>% 
#   mutate(full_fips = as.character(full_fips),
#          full_fips = str_pad(full_fips, 5, side="left", 0))

acsyears <- list(2010, 2015, 2020)
acsvars <- c("B02001_002E", "B02001_003E", "B02001_004E", "B02001_005E", "B02001_006E", "B03001_003E", "B01002_001E", "B25064_001E", "B19001_001E","B01003_001E")

for(year in acsyears) {
  df <- get_acs(geography = "county", year = year, geometry = FALSE, variables = acsvars) %>% 
    select(-moe)  %>% 
    pivot_wider(
      names_from = variable,
      values_from = estimate,
      values_fill = 0) %>%
    rename(!! sym(paste("acs", year, "_white", sep="")) := "B02001_002",
           !! sym(paste("acs", year, "_black", sep="")) := "B02001_003",
           !! sym(paste("acs", year, "_amind", sep="")) := "B02001_004",
           !! sym(paste("acs", year, "_asian", sep="")) := "B02001_005",
           !! sym(paste("acs", year, "_nhapi", sep="")) := "B02001_006",
           !! sym(paste("acs", year, "_hispanic", sep="")) := "B03001_003",
           !! sym(paste("acs", year, "_medage", sep="")) := "B01002_001",
           !! sym(paste("acs", year, "_medgrossrent", sep="")) := "B25064_001",
           !! sym(paste("acs", year, "_medhhic", sep="")) := "B19001_001",
           !! sym(paste("acs", year, "_totalpop", sep="")) := "B01003_001")  %>%
    rename(full_fips = GEOID) %>%
    select(-NAME)
  assign(paste("acs", year, sep=""), df)
}

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

# e08geo <- left_join(algara08, counties00, by = "full_fips")

write.csv(algara08, "./Political/PoliticalModifiedData/08_Algara_election.csv")
## 2016 ########################################################################
algara12 <- algara %>% 
  filter(year == 2012)  %>% 
  left_join(acs2010, by = "full_fips")
algara12$full_fips <- replace(algara12$full_fips, algara12$full_fips == 46113, 46102)

algara16 <- algara %>% 
  filter(year == 2016)  %>% 
  left_join(acs2015, by = "full_fips")
algara16$full_fips <- replace(algara16$full_fips, algara16$full_fips == 46113, 46102)


# e1216geo <- left_join(algara1216, counties10, by = "full_fips")
write.csv(algara12, "./Political/PoliticalModifiedData/12_Algara_electionACS.csv")
write.csv(algara16, "./Political/PoliticalModifiedData/16_Algara_electionACS.csv")
## 2020 ########################################################################
algara20 <- algara %>% 
  filter(year == 2020) %>% 
  left_join(acs2020, by = "full_fips")
algara20$full_fips <- replace(algara20$full_fips, algara20$full_fips == 46113, 46102)

# e20geo <- left_join(algara20, counties10, by = "full_fips")  

# e20placegeo <- left_join(place_county, counties10, by = "full_fips")
write.csv(algara20, "./Political/PoliticalModifiedData/20_Algara_electionACS.csv")
# ## write geopackages ###########################################################
# st_write(e08geo, "./Political/PoliticalModifiedData/Geopackages/us08_election.gpkg")
# st_write(e1216geo, "./Political/PoliticalModifiedData/Geopackages/us1216_election.gpkg")
# st_write(e20geo, "./Political/PoliticalModifiedData/Geopackages/us20_election.gpkg")
# 
