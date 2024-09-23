### file description ###########################################################
##                                                                            ##
## This file uses the complete clean local view data without transcripts to   ##
##      calculate the number of days between a FEMA disaster declaration and  ##
##      the meeting date                                                      ##
##                                                                            ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##
##              ./processing_scripts/local_view.r                             ##
##          FEMA (2010-2023)                                                  ##
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2        
##                                                                            ##
## Output:                                                                    ##
##      /LocalView/results/summaries/LargeFiles/nearestDeclaration_beforeMeeting.csv
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

# library(lubridate)                                             # work with dates
library(data.table)
# library(magrittr)
library(fs)

source("./LocalView/code/processing_scripts/analysis_prelims.r")      
source("./LocalView/code/processing_scripts/geography.r")
load("./LocalView/data/modified/allData_transcriptLevel.rdata")
load("./LocalView/data/modified/fema_disasterYear.rdata")

#   ____________________________________________________________________________
#   remove unneeded data form geography.r file                              ####

counties <- left_join(counties, states_abbr)
rm(cdp, countiesGeo, countySub, ip, ipcdp, states, statesGeo, states_abbr)

#   ____________________________________________________________________________
#   noaa zones                                                              ####

noaa_zones <- read.csv("./LocalView/data/original/noaa_county_correlation.csv") %>% 
    select(stcounty_fips, state_abbr, zone_name, time_zone, zone) %>% 
    mutate(zone = str_pad(zone, 3, "left", 0)) %>% 
    padFips() %>% 
    excludeStates() %>% 
    left_join(counties)

#   ____________________________________________________________________________
#   load the details files                                                  ####

path <- "./LocalView/data/original/noaa/noaa_events/Details/"

files <- dir_ls(path)

## 2004-2024
all_details <- files %>% 
    map_dfr(read.csv) %>% 
    rename_with(tolower) %>%
    select(episode_id, event_id, begin_yearmonth, begin_day, end_yearmonth, 
           end_day, state, state_fips, event_type, cz_type, cz_fips, 
           cz_name, injuries_direct, injuries_indirect, deaths_direct, 
           deaths_indirect, damage_property, damage_crops, flood_cause) %>% 
    mutate(state = str_to_title(state),
           cz_name = str_to_title(cz_name),
           begin_year = str_sub(begin_yearmonth,0, 4),
           end_year = str_sub(end_yearmonth, 0, 4),
           begin_month = str_sub(begin_yearmonth, 5),
           end_month = str_sub(end_yearmonth, 5),
           begin_date = make_date(begin_year, begin_month, begin_day),
           end_date = make_date(end_year, end_month, end_day),
           cz_fips = str_pad(cz_fips, 3, "left", 0)) %>%
    select(-ends_with(c("_yearmonth", "_day", "_month", "year"))) %>% 
    rename(state_name = state) %>% 
    padFips() %>% 
    excludeStates() %>% 
    group_by(episode_id) %>% 
    mutate(all_events_in_episode = n_distinct(event_id)) %>% 
    ungroup() %>% 
    filter(event_type == "Blizzard" |
           event_type == "Coastal Flood" |
           event_type == "Drought" |
           event_type == "Excessive Heat" |
           event_type == "Flash Flood" |
           event_type == "Flood" |
           event_type == "Heat" |
           event_type == "High Wind" |
           event_type == "Hurricane (Typhoon)" |
           event_type == "Ice Storm" |
           event_type == "Lakeshore Flood" |
           event_type == "Storm Surge/Tide" |
           event_type == "Thunderstorm Wind" |
           event_type == "Tornado" |
           event_type == "Tropical Storm" |
           event_type == "Tsunami" |
           event_type == "Wildfire" |
           event_type == "Winter Storm" |
           event_type == "Winter Weather" |
           event_type == "Hail" |
           event_type == "Tornado Depression") %>% 
    group_by(episode_id) %>% 
    mutate(reduced_events_in_episode = n_distinct(event_id))
   
##  ............................................................................
##  separate zones and counties                                             ####

all_counties <- all_details %>% 
    filter(cz_type == "C") %>% 
    rename(county_fips = cz_fips) %>% 
    padFips() %>% 
    createFips() %>% 
    select(-county_fips, -cz_name) %>% 
    left_join(noaa_zones, by = c("state_name", "state_fips", "stcounty_fips"), 
              relationship = "many-to-many")

all_zones <- all_details %>% 
    filter(cz_type == "Z") %>% 
    rename(zone = cz_fips) %>% 
    select(-cz_name) %>% 
    mutate(zone = str_pad(zone, 3, "left", 0)) %>% 
    left_join(noaa_zones, by = c("state_fips", "state_name", "zone"), relationship = "many-to-many")











































