### file description ###########################################################
##                                                                            ##
## This file processes the National Oceanic and Atmospheric Association data  ##
##                                                                            ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##
##              ./processing_scripts/local_view.r                             ##
##          NOAA (2009-2023)                                                  ##
##              https://www.ncdc.noaa.gov/stormevents/ftp.jsp                 ##        
##                                                                            ##
## Output:                                                                    ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

library(data.table)             # load multiple files
library(fs)                     # load multiple files

source("./LocalView/code/processing_scripts/analysis_prelims.r")      
source("./LocalView/code/processing_scripts/geography.r")

#   ____________________________________________________________________________
#   function to convert property and crop units                             ####

# convert_units <- function(x) {
#     
#     # If x is a vector (more than one element), apply recursively
#     if (length(x) > 1) {
#         return(sapply(x, convert_units))
#     }
#     
#     # If the string is empty, return 0
#     if (x == "") return(0)
#     if (is.na(x)) return(0)
#     
#     # If the string is in scientific notation, return it as numeric
#     x_str <- as.character(x)
#     if (grepl("e", x_str, ignore.case = TRUE)) {
#         return(as.numeric(x_str))
#     }
#     
#     # If x is already numeric, just return it
#     if (class(x) == "numeric") return(x)
#     
#     # Named vector of scale for 'K' and 'M' units
#     unit_scale <- c("k" = 1e3, "m" = 1e6)
#     
#     # Clean up input: remove commas and whitespace, and make lowercase
#     x_str <- gsub(",", "", trimws(tolower(x_str)))
#     
#     # Extract the unit character (e.g., 'k' or 'm')
#     unit_char <- gsub("[^a-z]", "", x_str)
#     
#     # Extract the numeric part of the string
#     x_num <- as.numeric(gsub("[a-z]", "", x_str))
#     
#     # Look up the multiplier for the extracted unit character
#     multiplier <- unit_scale[match(unit_char, names(unit_scale))]
#     multiplier[is.na(multiplier)] <- 1  # If no unit, assume multiplier of 1
#     
#     # Return the numeric value multiplied by the appropriate unit scale
#     return(x_num * multiplier)
# }

#   ____________________________________________________________________________
#   remove unneeded data form geography.r file                              ####

counties <- left_join(counties, states_abbr)
rm(cdp, countiesGeo, countySub, ip, ipcdp, states, statesGeo, states_abbr, sample_ipcdp)

#   ____________________________________________________________________________
#   noaa zones                                                              ####

noaa_correlation <- read.csv("./LocalView/data/original/noaa_county_correlation.csv") %>% 
    select(stcounty_fips, state_abbr, zone_name, zone) %>% 
    mutate(zone = str_pad(zone, 3, "left", 0)) %>% 
    padFips() %>% 
    excludeStates() %>% 
    rename(noaa_zoneCode = zone) %>% 
    left_join(counties) 

#   ____________________________________________________________________________
#   load the details files                                                  ####

path <- "./LocalView/data/original/noaa/noaa_events/Details/"

files <- dir_ls(path)

## 2004-2024
# n = 804,109
noaa <- files %>% 
    map_dfr(read.csv) %>%                                                       # load all data
    rename_with(tolower) %>%
    select(episode_id, event_id, begin_yearmonth, begin_day, end_yearmonth,     # select columns of interest
           end_day, state, state_fips, event_type, cz_type, cz_fips, 
           cz_name, injuries_direct, injuries_indirect, deaths_direct, 
           deaths_indirect, damage_property, flood_cause) %>%  
    filter(event_type == "Blizzard" |                                           # select events of interest
           event_type == "Coastal Flood" |
           event_type == "Cold/Wind Chill" |
           event_type == "Drought" |
           event_type == "Excessive Heat" |
           event_type == "Extreme Cold/Wind Chill" |
           event_type == "Flash Flood" |
           event_type == "Flood" |
           event_type == "Heat" |
           event_type == "Heavy Rain" |
           event_type == "Heavy Snow" |
           event_type == "High Wind" |
           event_type == "Hurricane" |
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
    filter(state_fips <= 56) %>% 
    excludeStates() %>% 
    group_by(episode_id) %>%  
    mutate(damaged_property_converted = convert_units(damage_property),         # convert units 
           noaa_episode_injuriesDirect = sum(injuries_direct),                       # sum direct/indirect injuries/deaths - by episode
           noaa_episode_injuriesIndirect = sum(injuries_indirect),
           noaa_episode_deathsDirect = sum(deaths_direct),
           noaa_episode_deathsIndirect = sum(deaths_indirect),
           noaa_episode_damagedProperty = sum(damaged_property_converted),           # sum property damage - by episode
           state = str_to_title(state),
           cz_name = str_to_title(cz_name),
           noaa_episode_beginYear = str_sub(begin_yearmonth,0, 4),                   # create begin/end date
           noaa_episode_endYear = str_sub(end_yearmonth, 0, 4),
           noaa_episode_beginMonth = str_sub(begin_yearmonth, 5),
           noaa_episode_endMonth = str_sub(end_yearmonth, 5),
           noaa_episode_beginDate = make_date(noaa_episode_beginYear, noaa_episode_beginMonth, begin_day),
           noaa_episode_endDate = make_date(noaa_episode_endYear, noaa_episode_endMonth, end_day),
           cz_fips = str_pad(cz_fips, 3, "left", 0),
           noaa_year = str_sub(noaa_episode_beginDate, 1,4),
           noaa_nEventsInEpisode = n(),
           noaa_nEventTypesInEpisode = n_distinct(event_type)) %>%                    
    select(episode_id, event_id, state, state_fips, noaa_year, event_type,      # select variables of interest
           cz_type, cz_fips, cz_name, noaa_episode_injuriesDirect, 
           noaa_episode_injuriesIndirect, noaa_episode_deathsDirect, 
           noaa_episode_deathsIndirect, noaa_episode_damagedProperty,
           noaa_episode_beginDate, noaa_episode_endDate, noaa_nEventsInEpisode, noaa_nEventTypesInEpisode) %>% 
    ungroup() %>% 
    padFips() %>%                                                               # pad 
    createFips() %>% 
    rename(noaa_episodeID = episode_id,                                         # rename columns    
           noaa_eventID = event_id,
           state_name = state,
           noaa_eventType = event_type,
           noaa_czType = cz_type,
           noaa_czFips = cz_fips,
           noaa_czName = cz_name,
           noaa_episode_beginDate = noaa_episode_beginDate,
           noaa_episode_endDate = noaa_episode_endDate) %>% 
  filter(noaa_year >=2009 & noaa_year <= 2023)


##  ............................................................................
##  separate rows that use county vs zone fips                              ####

# n = 243,986
noaa_county <- noaa %>%                                                         # filter for rows that use county data
  filter(noaa_czType == "C") %>% 
  mutate(stcounty_fips = paste0(state_fips, noaa_czFips, "")) %>% 
  select(-noaa_czFips, -state_name, -state_fips) %>% 
  excludeStates() %>% 
  left_join(noaa_correlation, by = "stcounty_fips", relationship = "many-to-many") %>% 
  select(-noaa_zoneCode) %>% 
  distinct(noaa_episodeID, stcounty_fips, .keep_all = TRUE)

# n = 7,310,857
# zones are larger than counties
noaa_zone <- noaa %>% 
    filter(noaa_czType == "Z") %>%                                              # filter for rows that use zone data
    select(-state_name, -state_fips) %>%                                        
    left_join(noaa_correlation, by = c("noaa_czFips" = "noaa_zoneCode"),        # merge with zone to county correlation file
              relationship = "many-to-many") %>% 
    distinct(noaa_episodeID, stcounty_fips, .keep_all = TRUE) %>%               # select unique episodes per county
    select(-noaa_czFips) %>%   
    excludeStates() 

# n = 7,554,843
noaa_episodeLevel <- rbind(noaa_county, noaa_zone) %>%
  group_by(noaa_year, stcounty_fips) %>% 
  mutate(noaa_nEpisodeCountyYear = n(),
         noaa_nEventCountyYear = n_distinct(noaa_eventType)) %>% 
  distinct(stcounty_fips, noaa_year, .keep_all = TRUE) %>% 
  excludeStates()

# save(noaa_episodeLevel, file = "./LocalView/data/modified/noaa_episodeLevel.rdata")

##  ............................................................................
# create county-year level noaa data                                        ####

# some of the noaa years do not have events, this section adds the missing counties
n_merged <- list()
for (y in unique(noaa_episodeLevel$noaa_year)){
  n <- noaa_episodeLevel %>% 
    distinct(stcounty_fips, .keep_all = TRUE) %>% 
    filter(noaa_year == y)
  
  if (y == 2009){
    n_merged[[y]] <- n %>% 
      select(-state_fips, -county_fips, -state_abbr, -state_name, -county_name, -census_division, -census_region, -n_countiesInState) %>% 
      right_join(counties, by = "stcounty_fips") %>% 
      mutate(
             noaa_year = as.numeric(y),
             transcript_year = noaa_year + 1,
             noaa_nEpisodeBinary = ifelse(is.na(noaa_nEpisodeCountyYear), 0, 1)
             )
  } else if (y >= 2010 & y <= 2014) {
    n_merged[[y]] <- n %>% 
      select(-state_fips, -county_fips, -state_abbr, -state_name, -county_name, -census_division, -census_region, -n_countiesInState) %>% 
      right_join(counties, by = "stcounty_fips") %>% 
      mutate(
        noaa_year = as.numeric(y),
        transcript_year = noaa_year + 1,
        noaa_nEpisodeBinary = ifelse(is.na(noaa_nEpisodeCountyYear), 0, 1)
      )
  } else if (y >= 2015 & y <= 2019) {
    n_merged[[y]] <- n %>% 
      select(-state_fips, -county_fips, -state_abbr, -state_name, -county_name, -census_division, -census_region, -n_countiesInState) %>% 
      right_join(counties, by = "stcounty_fips") %>% 
      mutate(
        noaa_year = as.numeric(y),
        transcript_year = noaa_year + 1,
        noaa_nEpisodeBinary = ifelse(is.na(noaa_nEpisodeCountyYear), 0, 1)
      )
  } else {
    n_merged[[y]] <- n %>% 
      select(-state_fips, -county_fips, -state_abbr, -state_name, -county_name, -census_division, -census_region, -n_countiesInState) %>% 
      right_join(counties, by = "stcounty_fips") %>% 
      mutate(
        noaa_year = as.numeric(y),
        transcript_year = noaa_year + 1,
        noaa_nEpisodeBinary = ifelse(is.na(noaa_nEpisodeCountyYear), 0, 1)
      )
  }
}

noaa_countyLevel <- bind_rows(n_merged) %>% 
  mutate(transcript_year = as.character(transcript_year),
         noaa_nEpisodeCountyYear = ifelse(is.na(noaa_nEpisodeCountyYear), 0, noaa_nEpisodeCountyYear)) %>% 
  select(-noaa_eventID, -noaa_eventType, -noaa_czType, noaa_czName, -zone_name)
  


# save(noaa_countyLevel, file = "./LocalView/data/modified/noaa_countyLevel.rdata")






















