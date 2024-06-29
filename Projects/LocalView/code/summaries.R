################################################################################
##                                                                            ##
## This file compiles summary statistics for the local view, election, and    ##
##   ACS data                                                                 ##
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
##      Code/Political/LVPartisanshipACS.r                                    ##
##                                                                            ##
## ** Majority counties were determined using ArcGIS' Tabulate Intersection   ##
##    function and processed using Google Docs.                               ##  
##    Methodology is available upon request.                                  ##               
##                                                                            ##
################################################################################

### libraries ##################################################################
##                                                                            ##
##                    load libraries and preliminaries                        ##
##                                                                            ##
################################################################################
library(tidyverse)
library(tidytext)
library(quanteda)
source("./Code/LocalView/Analysis/processing.r")

### original summaries #########################################################
##                                                                            ##
##                        original data set summaries                         ##
##      these summaries are on the original local view data n=103,372         ##
##      this data set is at the place & meeting year level and does not       ##
##                        include election or ACS data.                       ##
##                                                                            ##
################################################################################

## transcripts available by year and state
yearState <- lv %>% 
    group_by(transcript_year, state_name) %>% 
    count()

## transcripts available by year, state, and meeting_type
stateType <- lv %>% 
    group_by(transcript_year, state_name, meeting_type) %>% 
    count()

# write.csv(stateGov, "./Results/LocalView/Summaries/YearStateType.csv", row.names = FALSE)

## transcripts available by year, state, and place name
statePlace <- lv %>% 
    group_by(transcript_year, state_name, place_name) %>% 
    count()

# write.csv(statePlace, "./Results/LocalView/Summaries/YearStatePlace.csv", row.names = FALSE)
stateSummary <- aggregate(stateType$n, by=list(state=stateType$state_name), FUN=sum)

## create corpus
lvTidy <- tidy(corpus(lv, text_field = "caption_text_clean"))

## count and proportion of climate change use by year
cc_summary <- lvTidy %>% 
    select(transcript_year, vid_id, stcounty_fips, state_name, county_name, text) %>% 
    mutate(cc_mentions = str_count(text, "climate change"),
           cc_binary = ifelse(cc_mentions >= 1, 1, 0)) %>% 
    select(-text) %>% 
    group_by(transcript_year) %>% 
    mutate(cc_sum = sum(cc_mentions),
           n_script = n(),
           cc_sumBinary = sum(cc_binary),
           prop_cc = cc_sum/n_script,
           prop_ccBinary = cc_sum/n_script,
           n_county = n_distinct(stcounty_fips)) %>% 
    filter(cc_binary == 1) %>% 
    mutate(n_countyCC = n_distinct(stcounty_fips),
           prop_county = n_countyCC/n_county) %>% 
    distinct(transcript_year, .keep_all = TRUE) %>% 
    select(transcript_year, cc_sum, n_script, cc_sumBinary, prop_cc, prop_ccBinary, n_county, n_countyCC, prop_county) %>% 
    distinct(transcript_year, .keep_all = TRUE)

# write.csv(cc_summary, "/GitHub/NSF_ClimatePolitics/Results/Samples/LV Summaries/cc_mentions.csv")

### full data summaries ########################################################
##                                                                            ##
##                           full data set summaries                          ##
##      these summaries are on the modified local view data n=43,592          ##
##      this data set is at the county &  year level and includes             ##
##                             election & ACS data.                           ##
##                                                                            ##
################################################################################

## other.
data_noNA <- data %>% 
    filter(!is.na(n_meettype)) %>% 
    group_by(stcounty_fips) %>% 
    mutate(state_nScriptAY = n()) %>% 
    ungroup()

## balanced panel check
table(data_noNA$state_nScriptAY)

states <- data_noNA %>% 
    group_by(state_name) %>% 
    mutate(state_nScript_ACAY = sum(total_scriptCY)) %>% 
    select(transcript_year, state_name, total_scriptCY, sum_scriptCC, prop_cc, state_dvp, state_nScript_ACAY)

# write.csv(states, "./Results/Samples/statesACAY.csv", row.names = FALSE)




