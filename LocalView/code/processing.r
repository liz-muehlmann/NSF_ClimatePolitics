### file description ###########################################################
##                                                                            ##
## This file handles the preliminary steps necessary for text analysis        ##          
##                                                                            ##  
## This file loads, processes, and combines the Local View data with state    ##
##      and county boundaries, the Algara-Sharif presidential election        ##
##      returns, and the American community survey.                           ##
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
## ACS data was downloaded using the Tidy Census package                      ##
##      2006-2010 ACS > 2008                                                  ##
##      2011-2015 ACS > 2012 Election                                         ##
##      2016-2020 ACS > 2016 & 2020 Elections                                 ##
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
library(arrow)                     # open and work with parquet filepaths
library(readtext)                  # read filepaths
library(tidyverse)                 # data manipulation
library(sf)                        # work with shapefiles
library(tigris)                    # download geographic boundaries
library(tidycensus)                # ACS data

### geography ##################################################################
##                                                                            ##
##     this section loads and processes the place, county and state data      ##
##                                                                            ##
################################################################################

## places
places <- read.csv("./Data/Cartography/CartographyModified/2020_AllPlaces.csv") %>% 
    mutate(place_fips = str_pad(place_fips, 7, "left", 0),
           county_fips = str_pad(county_fips, 3, "left", 0),
           state_fips = str_pad(state_fips, 2, "left", 0),
           stcounty_fips = str_pad(stcounty_fips, 5, "left", 0)) %>%
    group_by(stcounty_fips) %>% 
    mutate(n_places_incounty = n_distinct(place_fips)) %>% 
    select(-coverage) %>% 
    distinct(.keep_all = TRUE)

## subset state and county FIPS and number of places in each county
np <- places %>% select(stcounty_fips, n_places_incounty) %>%
    distinct(stcounty_fips, .keep_all = TRUE)

states_geo <- states() %>%
    filter(GEOID < 57 & GEOID != "02") %>%
    rename(state_fips = GEOID,
           state_name = NAME) %>%
    select(state_fips, state_name) 

states <- states_geo %>% st_drop_geometry()

# geometry is retained to merge in at the end for use in arcGIS
counties10 <- counties(year = 2010, cb=TRUE) %>%
    filter(STATEFP < 57 & STATEFP != "02") %>%
    mutate(county_name = paste(NAME, LSAD, sep=" "),
           stcounty_fips = paste(STATE, COUNTY, sep = "")) %>% 
    rename(state_fips = STATE,
           county_fips = COUNTY) %>%
    select(state_fips, county_fips, stcounty_fips, county_name) %>%
    left_join(states) %>% 
    left_join(np)

counties20 <- counties(year = 2020, cb=TRUE) %>%
    filter(STATEFP < 57 & STATEFP != "02") %>%
    rename(stcounty_fips = GEOID,
           county_name = NAMELSAD) %>%
    mutate(state_fips = str_sub(stcounty_fips, 1, 2),
           county_fips = str_sub(stcounty_fips, 3)) %>%
    select(state_fips, county_fips, stcounty_fips, county_name) %>%
    left_join(states) %>% 
    left_join(np)

### local view #################################################################
##                                                                            ##
##          This section loads and processes the local view data              ##
##                                                                            ##
################################################################################

### load parquet data and convert it to data frame (N = 153,452) 
LVRaw <- open_dataset("./Data/LocalView/LVOriginalData/meetings/")  
LVRaw <- Scanner$create(LVRaw)
LVRaw <- LVRaw$ToTable()
LVRaw <- as.data.frame(LVRaw) 

### uncomment if need to know the number of transcripts with No Caption Available (n = 49,640)
# nrow(LVRaw %>% filter(caption_text_clean == "<No caption available>"))

### select only documents 2010+, filter out ACS and original caption columns, and rows with no caption available (n = 103,379)
docs <- LVRaw %>%
    rename(place_fips = st_fips,
           meeting_type = place_govt) %>% 
    mutate(transcript_year = str_sub(meeting_date, 1, 4),
           place_fips = str_pad(place_fips, 7, "left", "0")) %>% 
    filter(transcript_year >= 2010 & !(caption_text_clean == "<No caption available>") & state_name != "Alaska") %>% 
    select(vid_id, transcript_year, state_name, place_fips, place_name, meeting_type,caption_text_clean)

### uncomment the following lines to download only the state name, place name, and place fips for use in Cartography/MajorityCounties.r
# geo <- docs %>% select(place_name, place_fips)
# write.csv(geo, "./Data/LocalView/LVModifiedData/LVPlaceOnly.csv", row.names = FALSE)

## filter out 9,434 observations in the local view data used county FIPS instead of place FIPS
docsCounty <- docs %>% 
    filter(grepl("County", place_name)) %>% 
    mutate(stcounty_fips = str_sub(place_fips, 3, 7))

## separate out Hampton City, Norfolk City, and Petersburg City, Virginia (n = 105)
docsVirginia <- docs %>% 
    filter(place_fips == "5151650" | place_fips == "5151730" | place_fips == "5151710") %>% 
    mutate(stcounty_fips = str_sub(place_fips, 3, 7)) %>% 
    bind_rows(docsCounty)

## data that does not contain County (n = 93,840)
docsNoCounty <- docs %>% 
    filter(!grepl("County", place_name) & !(place_fips == "5151650" | place_fips == "5151730" | place_fips == "5151710"))

## merge dataVirginia data with place data
dcvp <- docsVirginia %>% left_join(places, by="stcounty_fips", multiple = "any") %>%  
    select(-place_name.y, -place_fips.y) %>%                                           
    rename(place_name = place_name.x,
           place_fips = place_fips.x)

## merge dataNoCounty data with place data
dncp <- docsNoCounty %>% left_join(places, by = "place_fips", relationship = "many-to-many") %>%      
    distinct(vid_id, transcript_year, .keep_all = TRUE) %>% 
    select(-place_name.y) %>% 
    rename(place_name = place_name.x)

## recombine county and place FIPS data with places
# n = 103,372 total
# this is used in the summaries.r file
lv <- rbind(dcvp, dncp) 

lvPlaces <- lv %>%
    select(-state_name, -county_name) %>%
    group_by(transcript_year, stcounty_fips) %>%
    mutate(n_meettype = n_distinct(meeting_type),
           total_scriptCY = n(),
           n_scriptCC = ifelse(str_count(caption_text_clean, "climate change") > 0, 1, 0),
           sum_scriptCC = sum(n_scriptCC),
           prop_cc = (sum_scriptCC/total_scriptCY)*100) %>%
    select(transcript_year, stcounty_fips, n_meettype, total_scriptCY, sum_scriptCC, prop_cc) %>%
    distinct(transcript_year, stcounty_fips, .keep_all = TRUE) %>%
    ungroup()

## separate the local view data to combine with the the correct geographic boundaries
lvPlaces10 <- lvPlaces %>% 
    filter(transcript_year <= 2019)

lvPlaces20 <- lvPlaces %>% 
    filter(transcript_year >= 2020)

## merge 2010-2019 transcripts to the 2010 county information
empty10 <- list()
for(y in unique(lvPlaces10$transcript_year)){
    empty10[[y]] <- lvPlaces10 %>% 
        filter(transcript_year == y) %>% 
        select(-transcript_year) %>% 
        right_join(counties10) %>% 
        mutate(transcript_year = y)
    assign(paste("lvPlaces", y, sep="_"), df)
}
lvPlaces10 <- bind_rows(empty10)

## merge 2020-2023 transcripts to the 2020 county information
empty20 <- list()
for(y in unique(lvPlaces20$transcript_year)){
    empty20[[y]] <- lvPlaces20 %>% 
        filter(transcript_year == y) %>% 
        select(-transcript_year) %>% 
        right_join(counties20) %>% 
        mutate(transcript_year = y)
    assign(paste("lvPlaces", y, sep="_"), df)
}
lvPlaces20 <- bind_rows(empty20)

## merge all local view data together
lvAll <- rbind(lvPlaces10, lvPlaces20)

## filter transcripts to merge by election year
# n = 6,228
lp1011 <- lvAll %>% 
    filter(transcript_year <= 2011)

# n = 13,771
lp1215 <- lvAll %>% 
    filter(transcript_year >= 2012 & transcript_year <= 2015)

# n = 39,945
lp1619 <- lvAll %>% 
    filter(transcript_year >= 2016 & transcript_year <= 2019)

# n = 47,591
lp2023 <- lvAll %>% 
    filter(transcript_year >= 2020 & transcript_year <= 2023)

### Algara-Sharif / ACS ########################################################
##                                                                            ##
##         this section loads and processes the algara-sharif/acs data        ##
##                                                                            ##
################################################################################
## election data
algaraACS <- read.csv("./Data/Political/PoliticalModified/algaraACS.csv") %>% 
    mutate(stcounty_fips = str_pad(stcounty_fips, 5, "left", 0)) %>% 
    select(-county_name, -state_name)

## for loop to save algara data by election year
for(year in unique(algaraACS$election_year)){
    df <- algaraACS %>% 
        filter(election_year == year) %>% 
        select(-election_year) %>% 
        mutate(election_year = year)
    assign(paste("algara", year, sep=""), df)
}

### merges county year #########################################################
##                                                                            ##
##                          merge all data  - county year                     ##
##                                                                            ##
################################################################################

## merge 2010-2011 Local View data with 2008 election data
algara08LV <- lp1011 %>% left_join(algara2008, by = "stcounty_fips")

## merge 2012-2015 Local View data with 2012 election data
algara12LV <- lp1215 %>% left_join(algara2012, by = "stcounty_fips")

## merge 2016-2019 Local View data with 2016 election data
algara16LV <- lp1619 %>% left_join(algara2016, by = "stcounty_fips")

## merge 2020-2023 Local View data with 2020 election data
algara20LV <- lp2023 %>% left_join(algara2020, by = "stcounty_fips")

## all years, all data
allData <- rbind(algara08LV, algara12LV, algara16LV, algara20LV) %>% 
    select(-state_fips, -county_fips) %>%
    filter(!(is.na(DVP))) 
    
## fix problems with county names - ñ was appearing as a unicode block
allData$county_name <- iconv(allData$county_name, 'utf8', 'ascii', sub = "9")
allData <- allData %>%  
    mutate(county_name = case_when(county_name == "Do9a Ana County" ~ "Dona Ana County",
                                   county_name == "Do99a Ana County" ~ "Dona Ana County",
                                   county_name == "District of Columbia NA" ~ "District of Columbia",
                                   county_name == "Carson City NA" ~ "Carson City",
                                   .default = county_name))       

### save county year ###########################################################
##                                                                            ##
##                  save data with and without county boundaries              ##
##                                                                            ##
################################################################################

## write all transcript years to geopackate or use in arcGIS
# for(y in unique(allData$transcript_year)){
#     df <- allData %>% filter(transcript_year == y)
#     st_write(df, paste0("./Data/LocalView/LVModifiedData/geography/lvCounty_", y, ".gpkg"))
# }

## save without geometry
allData <- allData %>% select(-geometry)

# write.csv(allData, "./Data/LocalView/LVModifiedData/LVCounty.csv")

### merges state ###############################################################
##                                                                            ##
##                          merge all data  - state year                      ##
##                                                                            ##
################################################################################

## state level data
allData_state <- allData %>%
    group_by(transcript_year, state_name) %>%
    mutate(stcounty_fips = str_pad(stcounty_fips, 5, "left", 0),
           state_cy = sum(total_scriptCY, na.rm = TRUE),            # total number of transcripts
           state_cc = sum(sum_scriptCC, na.rm=TRUE),                # sum of transcripts with CC mention
           state_propCC = ifelse(state_cc == 0, 0, round((state_cc/state_cy)*100, 2)),                        # state proportion of CC mentions
           total_vp = sum(DVP) + sum(RVP),                          # state total vote percentage
           state_dvp = round((sum(DVP)/total_vp)*100, 2),           # state dvp
           state_rvp = round((sum(RVP)/total_vp)*100, 2),           # state rvp
           n_county = n_distinct(county_name),
           state_pop = sum(totalpop),
           state_medage = sum(medage)/n_county,
           state_medgrossrent = sum(medgrossrent/n_county),
           state_medhhic = sum(medhhic)/n_county, 
           state_percHispanic = (sum(perc_hispanic))/n_county,
           state_percOther = (sum(perc_other))/n_county,
           state_percWhite = (sum(perc_white))/n_county,
           state_percBlack = (sum(perc_black))/n_county) %>% 
    ungroup() %>% 
    distinct(transcript_year, state_name, .keep_all = TRUE) %>% 
    select(state_name, n_places_incounty, starts_with("state_")) %>% 
    left_join(states_geo, by = "state_name")

### save state #################################################################
##                                                                            ##
##                  save data with and without state boundaries               ##
##                                                                            ##
################################################################################

## save
# st_write(allData_state, "./Data/LocalView/LVModifiedData/geography/lvStates.gpkg")

allData_state <- allData_state %>% select(-geometry)
# write.csv(allData_state, "./Data/LocalView/LVModifiedData/LVState.csv", row.names = FALSE)
