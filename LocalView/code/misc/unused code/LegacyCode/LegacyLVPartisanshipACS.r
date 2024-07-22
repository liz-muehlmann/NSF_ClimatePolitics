################################################################################
##                                                                            ##
## This file merges the local view data with ACS and election data.           ##
##                                                                            ##
## geographic boundaries downloaded using the tidy census data                 ##
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
## Election + ACS + County data were merged in                                ##        
##      Code/Political/AlgaraACSPlaces.r                                      ##
##                                                                            ##
## ** Majority counties were determined using ArcGIS' Tabulate Intersection   ##
##    function and processed using Google Docs.                               ##  
##    Methodology is available upon request.                                  ##               
##                                                                            ##
################################################################################

### load preliminaries & packages ##############################################
source("./Code/LocalView/TextAnalysis/Prelims.r")

### load place data ############################################################
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

### load local view data #######################################################
## filter only videos with transcripts (n = 103,812)
data <- all_docs  %>%  
    filter(state_name != "Alaska") %>% 
    rename(place_fips = st_fips,
           meeting_type = place_govt) %>% 
    mutate(place_fips = str_pad(place_fips, 7, "left", "0")) %>% 
    select(vid_id, transcript_year, place_fips, place_name, meeting_type, state_name, caption_text_clean)

## filter out 9,434 observations in the local view data used county FIPS instead of place FIPS
dataCounty <- data %>% 
    filter(grepl("County", place_name)) %>% 
    mutate(stcounty_fips = str_sub(place_fips, 3, 7))

## separate out Hampton City, Norfolk City, and Petersburg City, Virginia (n = 105)
dataVirginia <- data %>% 
    filter(place_fips == "5151650" | place_fips == "5151730" | place_fips == "5151710") %>% 
    mutate(stcounty_fips = str_sub(place_fips, 3, 7)) 

## merge data that uses county FIPS
dataCountyVA <- rbind(dataCounty, dataVirginia)     

## data that does not contain County (n = 93,840)
dataNoCounty <- data %>% 
    filter(!grepl("County", place_name) & !(place_fips == "5151650" | place_fips == "5151730" | place_fips == "5151710"))

## merge dataCountyVA data with place data
dcvp <- dataCountyVA %>% left_join(places, by="stcounty_fips", multiple = "any") %>%  
    select(-place_name.y, -place_fips.y) %>%                                           
    rename(place_name = place_name.x,
           place_fips = place_fips.x)

## merge dataNoCounty data with place data
dncp <- dataNoCounty %>% left_join(places, by = "place_fips") %>%      
    distinct(vid_id, transcript_year, .keep_all = TRUE) %>% 
    select(-place_name.y) %>% 
    rename(place_name = place_name.x)

## recombine county and place FIPS data with places
data_places <- rbind(dcvp, dncp) %>% 
    select(-state_name, -county_name)

## filter transcripts to merge by election year
# n = 2,065
data1011 <- data_places %>% 
    filter(transcript_year <= 2011)

# n = 13,771
data1215 <- data_places %>% 
    filter(transcript_year >= 2012 & transcript_year <= 2015)

# n = 39,945
data1619 <- data_places %>% 
    filter(transcript_year >= 2016 & transcript_year <= 2019)

# n = 47,591
data2023 <- data_places %>% 
    filter(transcript_year >= 2020 & transcript_year <= 2023)

### load election and ACS data #################################################
algaraACS <- read.csv("./Data/Political/PoliticalModified/algaraACS.csv") %>% 
    mutate(stcounty_fips = str_pad(stcounty_fips, 5, "left", 0)) 

## for loop to save algara data by election year
for(year in unique(algaraACS$election_year)){
    df <- algaraACS %>% 
        filter(election_year == year) %>% 
        select(-election_year) %>% 
        mutate(election_year = year)
    assign(paste("algara", year, sep=""), df)
}

### load state and county data #################################################
states <- states() %>%
    filter(GEOID < 57 & GEOID != "02") %>%
    rename(state_fips = GEOID,
           state_name = NAME) %>%
    select(state_fips, state_name) %>%
    st_drop_geometry()

counties10 <- counties(year = 2010) %>%
    filter(STATEFP10 < 57 & STATEFP10 != "02") %>%
    rename(stcounty_fips = GEOID10,
           county_name = NAMELSAD10,
           state_fips = STATEFP10,
           county_fips = COUNTYFP10) %>%
    select(state_fips, county_fips, stcounty_fips, county_name) %>%
    st_drop_geometry() %>%
    left_join(states) 

counties20 <- counties(year = 2020) %>%
    filter(STATEFP < 57 & STATEFP != "02") %>%
    rename(stcounty_fips = GEOID,
           county_name = NAMELSAD) %>%
    mutate(state_fips = str_sub(stcounty_fips, 1, 2),
           county_fips = str_sub(stcounty_fips, 3)) %>%
    select(state_fips, county_fips, stcounty_fips, county_name) %>%
    st_drop_geometry() %>%
    left_join(states)

### merges #####################################################################
## merge 2010-2011 Local View data with 2008 election data
algara08LV <- data1011 %>% left_join(algara2008, by = "stcounty_fips")

## merge 2012-2015 Local View data with 2012 election data
algara12LV <- data1215 %>% left_join(algara2012, by = "stcounty_fips")

## merge 2016-2019 Local View data with 2016 election data
algara16LV <- data1619 %>% left_join(algara2016, by = "stcounty_fips")

## merge 2020-2023 Local View data with 2020 election data
algara20LV <- data2023 %>% left_join(algara2020, by = "stcounty_fips")

## merge local view data with election data ###################################
# n = 103,372
lvElection <- rbind(algara08LV, algara12LV, algara16LV, algara20LV) %>% 
    group_by(transcript_year, state_name, county_name) %>% 
    mutate(n_meettype_countyNA = n_distinct(meeting_type),
           n_script_cc = str_count(caption_text_clean, "climate change"),
           total_scriptCY = n()) %>% 
    ungroup() %>% 
    select(-vid_id, -place_fips, -place_name, -meeting_type, -caption_text_clean, -state_name, -state_fips, -county_name, -county_fips,-DVP, -RVP, -medage, -medgrossrent, -medhhic, -totalpop, -perc_white, -perc_black, -perc_hispanic, -perc_other, -edu_percentPop, -n_places_incounty)

## separate local view and election data by year 
for(y in unique(lvElection$transcript_year)) {
    df <- lvElection %>% 
        filter(transcript_year == y) %>% 
        distinct(stcounty_fips, .keep_all = TRUE) %>% 
        select(-election_year)
    assign(paste("lvElection", y, sep="_"), df)
}

## local view transcript years
lvyears1015 <- list("2010", "2011", "2012", "2013", "2014", "2015")
lvyears1619 <- list("2016", "2017", "2018", "2019")
lvyears2023 <- list("2020", "2021", "2022", "2023")

## merge 2012 algara, 2010-2015 local view data, and 2010 county data
for(y in lvyears1015){
    df <- algara2012 %>% 
        left_join(get(paste("lvElection", y, sep = "_")), by = "stcounty_fips") %>% 
        left_join(counties10) %>% 
        left_join(np, by = "stcounty_fips") %>% 
        mutate(transcript_year = y,
               n_meettype_county0 = ifelse(is.na(n_meettype_countyNA), 0, n_meettype_countyNA),
               prop_scriptCC = n_script_cc/total_scriptCY) %>% 
        group_by(transcript_year, stcounty_fips) %>%
        mutate(n_scriptcc_binary = ifelse(n_script_cc > 0, 1, 0),
               total_scriptcc_binary = sum(n_scriptcc_binary, na.rm = TRUE),
               prop_scriptcc_binary = total_scriptcc_binary/total_scriptCY,
               vp_binary = ifelse(DVP > RVP, 1, 0)) %>%
        ungroup()
    assign(paste("alv", y, sep = "_"), df)
}

## merge 2016 algara, 2016-2019 local view data, and 2010 county data
for(y in lvyears1619){
    df <- algara2016 %>% 
        left_join(get(paste("lvElection", y, sep = "_")), by = "stcounty_fips") %>% 
        left_join(counties10) %>% 
        left_join(np, by = "stcounty_fips") %>% 
        mutate(transcript_year = y,
               n_meettype_county0 = ifelse(is.na(n_meettype_countyNA), 0, n_meettype_countyNA),
               prop_scriptCC = n_script_cc/total_scriptCY) %>% 
        group_by(transcript_year, stcounty_fips) %>%
        mutate(n_scriptcc_binary = ifelse(n_script_cc > 0, 1, 0),
               total_scriptcc_binary = sum(n_scriptcc_binary, na.rm = TRUE),
               prop_scriptcc_binary = total_scriptcc_binary/total_scriptCY,
               vp_binary = ifelse(DVP > RVP, 1, 0)) %>%
        ungroup()
    assign(paste("alv", y, sep = "_"), df)
}

## merge 2020 algara, 2020-2023 local view data, and 2020 county data
for(y in lvyears2023){
    df <- algara2020 %>% 
        left_join(get(paste("lvElection", y, sep = "_")), by = "stcounty_fips") %>% 
        left_join(counties10) %>% 
        left_join(np, by = "stcounty_fips") %>% 
        mutate(transcript_year = y,
               n_meettype_county0 = ifelse(is.na(n_meettype_countyNA), 0, n_meettype_countyNA),
               prop_scriptCC = n_script_cc/total_scriptCY) %>% 
        group_by(transcript_year, stcounty_fips) %>%
        mutate(n_scriptcc_binary = ifelse(n_script_cc > 0, 1, 0),
               total_scriptcc_binary = sum(n_scriptcc_binary, na.rm = TRUE),
               prop_scriptcc_binary = total_scriptcc_binary/total_scriptCY,
               vp_binary = ifelse(DVP > RVP, 1, 0)) %>%
        ungroup()
    assign(paste("alv", y, sep = "_"), df)
}

### merge all years ############################################################
all_years <- rbind(alv_2010, alv_2011, alv_2012, alv_2013, alv_2014, alv_2015, alv_2016, alv_2017, alv_2018, alv_2019, alv_2020, alv_2021, alv_2022, alv_2023) 

ay_lvdata <- all_years %>% 
    select(stcounty_fips, transcript_year, n_meettype_countyNA, n_script_cc, total_scriptCY, n_places_incounty, n_meettype_county0, prop_scriptCC, n_scriptcc_binary, total_scriptcc_binary, prop_scriptcc_binary, vp_binary) %>% filter(!(is.na(n_script_cc)))

# write.csv(ay_lvdata, "Data/LocalView/LVModifiedData/ay_lvdata.csv")

























































