################################# FILE DESCRIPTION #############################
##                                                                            ##
##  This file handles the preliminary steps necessary for                     ##  
##    the political data.                                                     ##
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
################################################################################

## load libraries ##############################################################
library(tidyverse)                 # data manipulation
library(tigris)                    # geographic boundaries
library(sf)                        # work with geographic boundaries 
library(tidycensus)                # ACS data

### load data ##################################################################
## acs years
acsyears <- list(2010, 2015, 2020)
acsvars <- c("B02001_002E",               # white
             "B02001_003E",               # Black
             "B02001_004E",               # American Indian
             "B02001_005E",               # Asian
             "B02001_006E",               # Native Hawaiian, Pacific Islander
             "B03001_003E",               # Hispanic
             "B01002_001E",               # median age
             "B25064_001E",               # median gross rent
             "B19001_001E",               # median house hold income
             "B01003_001E",               # total population     
             "B15002_002E",               # over 25 male population, education total
             "B15002_006E",               # over 25 male, no high school
             "B15002_010E",               # over 25 male, high school
             "B15002_014E",               # over 25 male, associate's
             "B15002_015E",               # over 25 male, bachelor's
             "B15002_016E",               # over 25 male, master's
             "B15002_017E",               # over 25 male, professional degree
             "B15002_018E",               # over 25 male, doctorate
             "B15002_019E",               # over 25 female population, education total
             "B15002_023E",               # over 25 female, no high school
             "B15002_028E",               # over 25 female, high school
             "B15002_031E",               # over 25 female, associate's
             "B15002_032E",               # over 25 female, bachelor's
             "B15002_033E",               # over 25 female, master's
             "B15002_034E",               # over 25 female, professional degree
             "B15002_035E")               # over 25 female, doctorate
 
## for loop to download acs years 2010-2020 ####################################
for(year in acsyears) {
  df <- get_acs(geography = "county", year = year, geometry = FALSE, variables = acsvars) %>% 
    select(-moe) %>% 
    filter(GEOID < 56999) %>% 
    mutate(var_names = 
           case_when(variable == "B02001_002" ~ "white",
                     variable == "B02001_003" ~ "black",              
                     variable == "B02001_004" ~ "amind",              
                     variable == "B02001_005" ~ "asian",              
                     variable == "B02001_006" ~ "nhapi",              
                     variable == "B03001_003" ~ "hispanic",           
                     variable == "B01002_001" ~ "medage",             
                     variable == "B25064_001" ~ "medgrossrent",       
                     variable == "B19001_001" ~ "medhhic",            
                     variable == "B01003_001" ~ "totalpop",     
                     variable == "B15002_014" ~ "eduM_aa",      
                     variable == "B15002_015" ~ "eduM_ba",      
                     variable == "B15002_016" ~ "eduM_ma",      
                     variable == "B15002_017" ~ "eduM_prof",    
                     variable == "B15002_018" ~ "eduM_phd",     
                     variable == "B15002_031" ~ "eduF_aa",      
                     variable == "B15002_032" ~ "eduF_ba",      
                     variable == "B15002_033" ~ "eduF_ma",      
                     variable == "B15002_034" ~ "eduF_prof",    
                     variable == "B15002_035" ~ "eduF_phd",
                     TRUE ~ as.character(variable))) %>%
    select(-variable) %>% 
    pivot_wider(values_from = estimate,
                names_from = var_names) %>%
    mutate(edu_percentPop = ((eduM_aa + eduF_aa + eduM_ba + eduF_ba + eduM_ma + eduF_ma + eduM_prof + eduF_prof + eduM_phd + eduF_phd)/totalpop)*100,
           perc_white = (white/totalpop)*100,
           perc_black = (black/totalpop)*100,
           perc_hispanic = (hispanic/totalpop)*100,
           other = amind + asian + nhapi,
           perc_other = (other/totalpop)*100,
           ACSyear = year) %>%
    rename(stcounty_fips = GEOID,
           county_name = NAME) %>% 
    select(stcounty_fips, county_name, ACSyear, medage, totalpop, medgrossrent, medhhic, perc_white, perc_black, perc_hispanic, perc_other, edu_percentPop)
  assign(paste("acs", year, sep=""), df)
}

### election data ##############################################################
load("./Data/Political/PoliticalOriginal/AlgaraSharif_all elections.Rdata")

## select the presidential data for 2008-2020 elections
algara <- pres_elections_release %>% 
  filter(election_year >=2008 & office == "PRES" & election_type == "G") %>% 
  select(election_year, fips, democratic_raw_votes, republican_raw_votes, pres_raw_county_vote_totals_two_party) %>%
  rename(stcounty_fips = fips,
         DEM = democratic_raw_votes,
         REP = republican_raw_votes,
         total_votes = pres_raw_county_vote_totals_two_party) %>% 
  mutate(DVP = (DEM/total_votes)*100,                   # calculate vote percentages
         RVP = (REP/total_votes)*100) %>% 
  select(stcounty_fips, election_year, DVP, RVP) 

### merge election data with ACS data ##########################################
## merge 2008 and 2012 election data with 2010 ACS data
algara0812 <- algara %>% 
  filter(election_year == 2008 | election_year == 2012) %>% 
  left_join(acs2010, by = "stcounty_fips") %>% 
  select(stcounty_fips, county_name, election_year, DVP, RVP, medage, medgrossrent, medhhic, totalpop, perc_white, perc_black, perc_hispanic, perc_other, edu_percentPop)

## merge 2016 election data with 2015 ACS data
algara16 <- algara %>% 
  filter(election_year == 2016) %>% 
  left_join(acs2015, by = "stcounty_fips") %>% 
  select(stcounty_fips, county_name, election_year, DVP, RVP, medage, medgrossrent, medhhic, totalpop, perc_white, perc_black, perc_hispanic, perc_other, edu_percentPop)

## merge 2020 election data with 2025 ACS data
algara20 <- algara %>% 
  filter(election_year == 2020) %>% 
  left_join(acs2020, by = "stcounty_fips") %>% 
  select(stcounty_fips, county_name, election_year, DVP, RVP, medage, medgrossrent, medhhic, totalpop, perc_white, perc_black, perc_hispanic, perc_other, edu_percentPop)

## combine all algara ACS data for all years
algaraACS <- rbind(algara0812, algara16, algara20) %>% 
  separate_wider_delim(county_name, ",", names = c("county_name", "state_name")) %>% 
  mutate(state_name = str_trim(state_name, side = "left"))

## Shannon County (46113) was changed to Oglala Lakota County (46102) in 2015. 
## When this is later merged with the county boundaries 2016-2019 data becomes NA
## The following lines correct the problem. 
oglala <- algaraACS %>% filter(stcounty_fips == "46102" & election_year == 2016) %>% 
  mutate(stcounty_fips = "46113",
         county_name = "Shannon County")

algaraACS <- rbind(algaraACS, oglala)

# write.csv(algaraACS, "./Data/Political/PoliticalModified/algaraACS.csv", row.names = FALSE)































































