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
load("./LocalView/data/modified/lv_clean_noTranscript.rdata")
load("./LocalView/data/modified/lv_countyYear_noNA.rdata")
load("./LocalView/data/modified/allData_state.rdata")

### original summaries #########################################################
##                                                                            ##
##                        original data set summaries                         ##
##      these summaries are on the original local view data n=103,372         ##
##      this data set is at the place & meeting year level and does not       ##
##                        include election or ACS data.                       ##
##                                                                            ##
################################################################################
## transcripts available by year and state
yearState <- lvClean_noScript %>% 
    group_by(transcript_year, state_name) %>% 
    count()

# write.csv(yearState, "./LocalView/results/summaries/YearState.csv", row.names = FALSE)

yearStateCounty <- allData_state %>% 
    select(state_name, transcript_year, n_countiesInState, state_n_transcripts, state_n_scriptCCMention)

# write.csv(yearStateCounty, "./LocalView/results/summaries/YearStateCounty.csv", row.names = FALSE)

## transcripts available by year, state, and meeting_type
stateType <- lvClean_noScript %>%
    group_by(transcript_year, state_name, meeting_type) %>% 
    count()

# write.csv(stateType, "./LocalView/results/summaries/YearStateMeetingType.csv", row.names = FALSE)

## transcripts available by year, state, and place name
statePlace <- lvClean_noScript %>%
    group_by(transcript_year, state_name, place_name) %>% 
    count()

# write.csv(statePlace, "./LocalView/results/summaries/YearStatePlace.csv", row.names = FALSE)

stateSummary <- lvClean_noScript %>%
    group_by(state_name) %>% 
    count()

## count and proportion of climate change use by year
prop_CCuse <- lvClean_noScript %>% 
    select(transcript_year, stcounty_fips, n_ccMentions, ccBinary) %>% 
    group_by(transcript_year) %>% 
    mutate(year_nCounties = n_distinct(stcounty_fips),                                       # total distinct counties in the LV data by year
           year_n_transcripts = n(),                                                         # total number of transcripts in a year
           year_nScriptCC = sum(ccBinary),                                                   # number of transcripts in a year with at least one mention of climate change
           year_nCCmentions = sum(n_ccMentions),                                             # total number of climate change mentions in a year
           year_propScriptCC = year_nScriptCC/year_n_transcripts,                            # proportion of transcripts with at least one climate change mention
           year_propCCmentions = year_nCCmentions/year_n_transcripts,                        # proportion of all metions of climate change in a given year
           year_ccBinary = ifelse(ccBinary > 0, 1, 0)) %>%                                   # binary indicator of whether there's at least one mention of climate change in that year
    filter(year_ccBinary == 1) %>%  
    mutate(n_countiesCCmention = n_distinct(stcounty_fips),                                  # number of unique counties that mention climate change in a year 
           prop_countiesCCMention = n_countiesCCmention/year_nCounties) %>%                  # proportion of counties in the data that mention climate change at least once
    select(transcript_year, n_ccMentions, prop_countiesCCMention, starts_with("year_")) %>% 
    distinct(transcript_year, .keep_all = TRUE)


# write.csv(prop_CCuse, "./LocalView/results/summaries/prop_ccUse.csv", row.names = FALSE)

### state summaries ############################################################
##                                                                            ##
##                           full data set summaries                          ##
##      these summaries are on the modified local view data n=43,592          ##
##                                                                            ##
################################################################################

## number of counties in each state-year with a transcript
data_noNA <- lv_countyYear_noNA %>% 
    group_by(stcounty_fips) %>% 
    mutate(state_nScriptAY = n()) %>% 
    ungroup() 

## balanced panel check
table(data_noNA$state_nScriptAY)

