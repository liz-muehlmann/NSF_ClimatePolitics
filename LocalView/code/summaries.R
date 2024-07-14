################################################################################
##                                                                            ##
## This file compiles summary statistics                                      ##
##                                                                            ##    
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##              
##          Algara-Sharif (2008-2020)                                         ##
##              Sharif (2021) "Replication Data for: Partisanship & Nationalization
##              in American Elections: Evidence from Presidential, Senatorial, &   
##              Gubernatorial Elections in the U.S. Counties, 1872-2020",     ##      
##              https://doi.org/10.7910/DVN/DGUMFI                            ##      
##          American Community Survey (2010, 2015, 2020)                      ##
##              2006-2010 ACS > 2008                                          ##
##              2011-2015 ACS > 2012 Election                                 ##
##              2016-2020 ACS > 2016 & 2020 Elections                         ##
##          FEMA (2010-2023)                                                  ## 
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
##          Climate Change Vulnerability (2010-2023)                          ## 
##              https://github.com/wachiuphd/CVI
##          USDA Rural-Urban                                                  ## 
##          Incorporated/Census Designated Places (2023)                      ## 
##              IP/CDP csvs were saved using the tabulate intersection function in ArcGIS 
##          Virginia                                                          ##
##              https://www.census.gov/library/reference/code-lists/ansi.2020.html#cousub
##          Counties (2010, 2015, 2020)                                       ##
##              tigris                                                        ##
##          States                                                            ##
##              tigris                                                        ##   
##                                                                            ##
################################################################################ 

### libraries ##################################################################
##                                                                            ##
##                    load libraries and preliminaries                        ##
##                                                                            ##
################################################################################
library(tidyverse)
load("./LocalView/data/Processed_LocalView.rdata")

### original summaries #########################################################
##                                                                            ##
##                        original data set summaries                         ##
##      these summaries are on the original local view data n=103,372         ##
##      this data set is at the place & meeting year level and does not       ##
##                        include election or ACS data.                       ##
##                                                                            ##
################################################################################
lv <- lvCountyNA %>% filter(!is.na(n_meetTypeCY))

## transcripts available by year and state
yearState <- lv %>% 
    group_by(transcript_year, state_name) %>% 
    count()

# write.csv(yearState, "./LocalView/results/summaries/YearState.csv")

yearStateCounty <- lv %>% 
    filter(!is.na(n_meetTypeCY)) %>% 
    select(transcript_year, state_name, stcounty_fips, n_counties, n_scriptCY, n_ccScriptCY, prop_cc) %>% 
    group_by(state_name, transcript_year) %>% 
    mutate(nws = n_distinct(stcounty_fips),
           n_withScript = sum(nws),
           nwCC = ifelse(n_scriptCY > 0,1,0),
           n_scriptCC = sum(nwCC, na.rm=TRUE)) %>% 
    distinct(state_name, .keep_all = TRUE) %>% 
    select(state_name, transcript_year, n_counties, n_withScript, n_scriptCC)

write.csv(yearStateCounty, "./LocalView/results/summaries/YearStateCounty.csv", row.names = FALSE)

## transcripts available by year, state, and meeting_type
stateType <- lv %>%
    group_by(transcript_year, state_name, meeting_type) %>% 
    count()

# write.csv(stateType, "./Results/LocalView/Summaries/YearStateMeetingType.csv", row.names = FALSE)

## transcripts available by year, state, and place name
statePlace <- lv %>%
    group_by(transcript_year, state_name, place_name) %>% 
    count()

# write.csv(statePlace, "./Results/LocalView/Summaries/YearStatePlace.csv", row.names = FALSE)

stateSummary <- lv %>%
    group_by(state_name) %>% 
    count()

## count and proportion of climate change use by year
prop_CCuse <- lv %>% 
    select(transcript_year, vid_id, stcounty_fips, state_name, county_name, 
           caption_text_clean, n_ccMentions) %>% 
    group_by(transcript_year) %>% 
    mutate(n_county = n_distinct(stcounty_fips),                                # number of unique counties represented in LV data
           total_ccMentionYear = sum(n_ccMentions),                             # total climate change mentions in a year
           n_ccBinaryYear = ifelse(n_ccMentions > 0, 1, 0),                     # 1 = transcript has cc mention, 0 = no cc mention
           n_scriptYear = n(),                                                  # total number of transcripts in a year
           n_scriptCCYear = sum(n_ccBinaryYear),                                # total number of transcripts with at least one climate change mention
           prop_scriptCCYear = (n_scriptCCYear/n_scriptYear)*100,               # proportion of transcripts with at least one climate change mention
           prop_ccMentionYear = (total_ccMentionYear/n_scriptYear)*100) %>%     # proportion of all climate change mentions in a give year
    filter(n_ccBinaryYear == 1) %>% 
    mutate(n_counties_ccMention = n_distinct(stcounty_fips),
           prop_countiesCCMention = n_counties_ccMention/n_county) %>%          # proportion of climate change mentions in counties represented in LV
    select(transcript_year, prop_scriptCCYear, prop_ccMentionYear, n_county, 
           n_counties_ccMention, prop_countiesCCMention) %>% 
    distinct(transcript_year, .keep_all = TRUE)

# write.csv(prop_CCuse, "./LocalView/results/summaries/sample_propCCuse.csv", row.names = FALSE)

### state summaries ############################################################
##                                                                            ##
##                           full data set summaries                          ##
##      these summaries are on the modified local view data n=43,592          ##
##                                                                            ##
################################################################################

## number of counties in each state-year with a transcript
data_noNA <- allData %>% 
    group_by(stcounty_fips) %>% 
    mutate(state_nScriptAY = n()) %>% 
    ungroup() 

## balanced panel check
table(data_noNA$state_nScriptAY)

# write.csv(states_noNA, "./Results/Samples/statesACAY.csv", row.names = FALSE)

### county summaries ###########################################################
##                                                                            ##
##        counties with any transcript & number of total transcripts          ##
##                                                                            ##
################################################################################




