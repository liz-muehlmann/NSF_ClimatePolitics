### file description ###############################################################################
##                                                                                                ##
## This file handles the processing steps necessary for the Algara-Sharif election data           ##
##      Data included:                                                                            ##
##          Algara-Sharif (2008-2020)                                                             ##
##              Sharif (2021) "Replication Data for: Partisanship & Nationalization               ##
##              in American Elections: Evidence from Presidential, Senatorial, &                  ##
##              Gubernatorial Elections in the U.S. Counties, 1872-2020",                         ##      
##              https://doi.org/10.7910/DVN/DGUMFI                                                ##      
##                                                                                                ##    
#################################################################################################### 

#   ________________________________________________________________________________________________
#   load libraries and custom functions                                                         ####

source("./LocalView/code/processing_scripts/preliminaries.r")      

#   ________________________________________________________________________________________________
#   algara-sharif election                                                                      ####

load("./LocalView/data/original/AlgaraSharif.Rdata")

## select the presidential data for 2008-2020 elections
algara <- pres_elections_release %>% 
    filter(election_year >=2008 & office == "PRES" & election_type == "G") %>% 
    select(election_year, 
           fips, 
           democratic_raw_votes, 
           republican_raw_votes, 
           pres_raw_county_vote_totals_two_party) %>%
    rename(stcounty_fips = fips,
           DEM = democratic_raw_votes,
           REP = republican_raw_votes,
           total_votes = pres_raw_county_vote_totals_two_party) %>% 
    filter(stcounty_fips != 51515) %>% 
    createFips() %>% 
    excludeStates() %>% 
    fixCounties2020() %>% 
    mutate(DVP = DEM/total_votes,                   
           RVP = REP/total_votes) %>% 
    select(stcounty_fips, election_year, DVP, RVP, DEM, REP, total_votes)

# save(algara, file = "./LocalView/data/modified/algara.rdata")