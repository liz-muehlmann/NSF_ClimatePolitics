### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the local view data   ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##  
## Output:                                                                    ##
##      /LocalView/data/modified/lv_clean_transcript.rdata                    ##
##      /LocalView/data/modified/lv_clean_noTranscript.rdata                  ##
##      /LocalView/data/modified/lv_countyYear_noNA.rdata                     ##
##      /LocalView/data/modified/lv_countyYear_withNA.rdata                   ##
##                                                                            ##    
################################################################################ 

#   ____________________________________________________________________________
#   load packages                                                           ####
#       Note: the tidyverse() package is loaded below through the geographic_processing.r file

library(arrow)                                     # open and work with parquet format (Local View)
library(readtext)                                  # read filepaths (Local View)


#   ____________________________________________________________________________
#   state and county information is necessary for aggregating to            ####
#   the county-year level processing the geography data is done in the 
#   /processing_scripts/geography.r file
#   this will also load the preliminaries file at /processing_scripts/prelims.r

source("./LocalView/code/processing_scripts/geography.r")

#   ____________________________________________________________________________
#   local view                                                              ####

##  ............................................................................
##  load raw data: n = 153,452                                              ####
lvRaw <- open_dataset("./LocalView/data/original/meetings/")  
lvRaw <- Scanner$create(lvRaw)
lvRaw <- lvRaw$ToTable()
lvRaw <- as.data.frame(lvRaw) 

##  ............................................................................
##  create sample: n = 103,379                                              ####
##      years: 2010+ 
##      discard: ACS columns, original caption columns, and rows with no 
##      caption available five places use incorrect fips and are manually fixed.

lv <- lvRaw %>% 
    rename(place_fips = st_fips,
           meeting_type = place_govt) %>% 
    separate(meeting_date, into = c("transcript_year", 
                                    "transcript_month", 
                                    "transcript_day"), sep = "-") %>% 
    filter(transcript_year >= 2010 &
               !(caption_text_clean == "<No caption available>") &
               state_name != "Alaska") %>% 
    mutate(place_fips = ifelse(place_fips == "5151650", "5135000", place_fips), # hampton city, VA
           place_fips = ifelse(place_fips == "5151710", "5157000", place_fips), # norfolk city, VA
           place_fips = ifelse(place_fips == "5151730", "5161832", place_fips), # petersburg city, VA
           place_fips = ifelse(place_fips == "2501325", "2501370", place_fips), # amherst town, MA
           place_fips = ifelse(place_fips == "2527100", "2527060", place_fips), # greenfield, MA (incorrect place fips)
           place_fips = ifelse(place_name == "Gadsden city", "0128696", 
                               place_fips)) %>%                                 # does not have fips
    rename(transcript_id = vid_id) %>% 
    select(-starts_with(c("acs_", "channel", "vid_")), -caption_text)

##  ............................................................................
##  merge lv with county, county subdivision, and place data                ####

## 9,434 transcripts use two digit state + five digit county fips as place fips
lvCounty <- lv %>% 
    filter(grepl("County", place_name)) %>% 
    mutate(stcounty_fips = str_sub(place_fips, 3, 7),
           county_name = place_name) %>% 
    createFips()

## 93,945 transcripts use place (88,023) or county subdivision data (5,922)
lvPlace <- lv %>% 
    filter(!grepl("County", place_name)) %>% 
    left_join(ipcdp, by = "place_fips")

# 5,922 transcripts use county-subdivision information
lvCountySub <- lvPlace %>% 
    filter(is.na(stcounty_fips)) %>% 
    select(-county_name, -stcounty_fips) %>%
    left_join(countySub, by = "place_fips")

## 88,023 transcripts use correct place data
lvPlace <- lvPlace %>% 
    filter(!is.na(stcounty_fips)) 

## add county information to local view data
lvCounty <- ipcdp %>% 
    right_join(lvCounty, multiple = "any")

## combine and save clean version of Local View data before further processing
lvClean_transcript <- rbind(lvPlace, lvCounty, lvCountySub) %>% 
    select(-state_fips, -county_fips) %>% 
    createFips()

# save(lvClean_transcript, file = "./LocalView/data/modified/lv_clean_transcript.rdata")

#   ____________________________________________________________________________
#   calculate climate change use by meeting                                 ####

lvClean_noScript <- lvClean_transcript %>% 
    mutate(n_ccMentions = str_count(caption_text_clean, "climate change"),   # number of climate change mentions in each transcript
           ccBinary = ifelse(n_ccMentions > 0, 1, 0)) %>%                    # binary indicator 1 = climate change mentioned 0 = no climate change mention
    select(-caption_text_clean)

# save(lvClean_noScript, file = "./LocalView/data/modified/lv_clean_noTranscript.rdata")

#   ____________________________________________________________________________
#   aggregate local view data to the county level (n 3,668)                 ####

lv_countyYear_noNA <- lvClean_noScript %>% 
    group_by(transcript_year, stcounty_fips) %>% 
    mutate(n_transcripts = n(),                                               # number of transcripts in a county-year
           n_meetingTypes = n_distinct(meeting_type),                         # number of meeting types in a county-year
           n_scriptCCMention = sum(ccBinary),                                 # number of transcripts with at least one mention of climate change in a county-year
           n_ccMentions = sum(n_ccMentions),                                  # total number of climate change mentions in a county-year
           ccBinary = ifelse(n_ccMentions > 0, 1, 0),                         # binary indicator 1 = climate change mentioned in the county-year, 0 = no climate change mention
           prop_ccMentions = (n_ccMentions/n_transcripts),                    # proportion of total climate change mentions in a given county-year
           prop_scriptCCMention = n_scriptCCMention/n_transcripts) %>%        # proportion of transcripts with at least one climate change mention in a given county-year         
    distinct(transcript_year, stcounty_fips, .keep_all = TRUE) %>% 
    ungroup() %>% 
    select(state_name, state_fips, county_name, county_fips, transcript_year, 
           ccBinary, starts_with(c("n_", "prop_"))) %>% 
    left_join(counties)

# save(lv_countyYear_noNA, file = "./LocalView/data/modified/lv_countyYear_noNA.rdata")

#   ____________________________________________________________________________
#   merge with all counties                                                 ####
#     NA indicates there was no transcript in that county for that year 
#     (n = 43,512)

lv_countyYear_withNA <- list()
for(y in unique(lv_countyYear_noNA$transcript_year)) {
    lv_countyYear_withNA[[y]] <- lv_countyYear_noNA %>%
        filter(transcript_year == y) %>%
        select(-transcript_year) %>%
        right_join(counties) %>%
        mutate(transcript_year = y,
               has_transcript = ifelse(is.na(n_meetingTypes), 0, 1))
}
lv_countyYear_withNA <- bind_rows(lv_countyYear_withNA)

# save(lv_countyYear_withNA, file = "./LocalView/data/modified/lv_countyYear_withNA.rdata")
