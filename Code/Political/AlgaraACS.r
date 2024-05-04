################################# FILE DESCRIPTION #############################
##                                                                            ##
##  This file handles the preliminary steps necessary for                     ##  
##    the political data                                                      ##
##                                                                            ##
##  Political data was downloaded from the Harvard Dataverse for Algara &     ##
##    Sharif (2021) "Replication Data for: Partisanship & Nationalization     ##
##    in American Elections: Evidence from Presidential, Senatorial, &        ##
##    Gubernatorial Elections in the U.S. Counties, 1872-2020",               ##
##      https://doi.org/10.7910/DVN/DGUMFI                                    ##
##                                                                            ##
##  ACS data was downloaded using the Tidy Census package                     ##
##      2006-2010 ACS > 2008                                                  ##
##      2011-2015 ACS > 2012 Election                                         ##
##      2016-2020 ACS > 2016 & 2020 Elections                                 ##
##                                                                            ##
## ** Majority counties were determined using ArcGIS' Tabulate Intersection   ##
##    function and processed using Google Docs.                               ##  
##    Methodology is available upon request.                                  ##                                    
##                                                                            ##
## county names are downloaded using the Tigris package                       ##
##                                                                            ##
################################################################################

## suppress warnings ###########################################################
options(warn = -1)
setwd("F:/GitHub/NSF_ClimatePolitics")

## load libraries ##############################################################
library(tidyverse)                 # data manipulation
library(tigris)                    # geographic boundaries
library(sf)                        # work with geographic boundaries 
library(tidycensus)                # ACS data

## load data ##################################################################
# counties
counties00 <- counties(year = 2000) %>% 
  filter(STATEFP00 < 57 & STATEFP00 != "02") %>% 
  rename(full_fips = CNTYIDFP00,
         county_name = NAMELSAD00) %>% 
  select(full_fips, county_name)

counties20 <- counties(year = 2020) %>% 
  filter(STATEFP < 57 & STATEFP != "02") %>% 
  rename(full_fips = GEOID,
         county_name = NAMELSAD) %>% 
  select(full_fips, county_name)

# acs
acsyears <- list(2009, 2015, 2020)
acsvars <- c("B02001_002E", "B02001_003E", "B02001_004E", "B02001_005E", "B02001_006E", "B03001_003E", "B01002_001E", "B25064_001E", "B19001_001E","B01003_001E")

for(year in acsyears) {
  df <- get_acs(geography = "county", year = year, geometry = FALSE, variables = acsvars) %>% 
    select(-moe)  %>% 
    pivot_wider(
      names_from = variable,
      values_from = estimate,
      values_fill = 0) %>%
    rename("white" = "B02001_002",
           "black" = "B02001_003",
           "amind" = "B02001_004",
           "asian" = "B02001_005",
           "nhapi" = "B02001_006",
           "hispanic" = "B03001_003",
           "medage" = "B01002_001",
           "medgrossrent" = "B25064_001",
           "medhhic" = "B19001_001",
           "totalpop" = "B01003_001")  %>%
    mutate(ACSyear = year) %>% 
    rename(full_fips = GEOID) %>% 
    select(-NAME) %>% 
    filter(full_fips < 56999)
  assign(paste("acs", year, sep=""), df)
}

acs <- rbind(acs2009, acs2015)
acs_wide <- rbind(acs, acs2020) %>% 
  distinct() %>% 
  pivot_wider(names_from = ACSyear,
              values_from = c(medage, totalpop, white, black, amind, asian, nhapi, hispanic, medhhic, medgrossrent))

# election data 
load("./Data/Political/AlgaraSharif_all elections.Rdata")

algara_wide <- pres_elections_release %>% 
  filter(election_year >=2008 & office == "PRES" & election_type == "G") %>% 
  select(election_year, fips, democratic_raw_votes, republican_raw_votes, pres_raw_county_vote_totals_two_party) %>%
  rename(full_fips = fips,
         DEM = democratic_raw_votes,
         REP = republican_raw_votes,
         total_votes = pres_raw_county_vote_totals_two_party) %>% 
  mutate(DVP = (DEM/total_votes)*100,                                   # calculate vote percentages
         RVP = (REP/total_votes)*100) %>% 
  pivot_wider(names_from = election_year,
              values_from = c(DEM, REP, total_votes, DVP, RVP))

algaraACSwide <- left_join(algara_wide, acs_wide, by = "full_fips")

# write.csv(algaraACSwide, "./Data/Political/PoliticalModified/algaraACSwide.csv", row.names = FALSE)

acs_long <- rbind(acs, acs2020) %>% 
  distinct()

algara_long <- pres_elections_release %>% 
  filter(election_year >=2008 & office == "PRES" & election_type == "G") %>% 
  select(election_year, fips, democratic_raw_votes, republican_raw_votes, pres_raw_county_vote_totals_two_party) %>%
  rename(full_fips = fips,
         DEM = democratic_raw_votes,
         REP = republican_raw_votes,
         total_votes = pres_raw_county_vote_totals_two_party) %>% 
  mutate(DVP = (DEM/total_votes)*100,                                   # calculate vote percentages
         RVP = (REP/total_votes)*100)

algaraACSlong <- left_join(algara_long, acs_long, by = "full_fips")

# write.csv(algaraACSlong, "./Data/Political/PoliticalModified/algaraACSlong.csv", row.names = FALSE)
