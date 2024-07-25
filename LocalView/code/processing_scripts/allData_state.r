### file description ###############################################################################
##                                                                                                ##
## This file uses the complete merged data from /LocalView/code/merging_scripts/countyYear_noNA.r ##
##      and calculate the state level values for each of the variables. The data included is      ##
##      listed below along with the individual processing files. detailed processing              ##
##      documentation is available upon request.                                                  ##
##                                                                                                ##
##      Data included:                                                                            ##
##          Local View (2010-2023)                                                                ##
##              https://doi.org/10.7910/DVN/NJTBEM                                                ##   
##              ./processing_scripts/local_view.r                                                 ##
##          Algara-Sharif (2008-2020)                                                             ##
##              Sharif (2021) "Replication Data for: Partisanship & Nationalization               ##
##              in American Elections: Evidence from Presidential, Senatorial, &                  ##
##              Gubernatorial Elections in the U.S. Counties, 1872-2020",                         ##      
##              https://doi.org/10.7910/DVN/DGUMFI                                                ##
##              ./processing_scripts/algara_sharif.r                                              ##
##          American Community Survey (2010, 2015, 2020)                                          ##
##              2006-2010 ACS > 2008                                                              ##
##              2011-2015 ACS > 2012 Election                                                     ##
##              2016-2020 ACS > 2016 & 2020 Elections                                             ##
##              ./processing_scripts/acs.r                                                        ##
##          FEMA (2010-2023)                                                                      ## 
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2        ##
##          Climate Change Vulnerability (2010-2023)                                              ## 
##              https://github.com/wachiuphd/CVI                                                  ## 
##              ./processing_scripts/cvi.r                                                        ##
##          USDA Rural-Urban                                                                      ## 
##              https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/documentation/ ##
##              ./processing_scripts/rural_urban.r                                                ##
##                                                                                                ##
#################################################################################################### 


#   ________________________________________________________________________________________________
#   load preliminaries and data                                                                 ####

source("./LocalView/code/processing_scripts/preliminaries.r")
load("./LocalView/data/modified/allData_withNA.rdata")


allData_state <- allData_withNA %>% 
    group_by(transcript_year, state_name) %>%
    mutate(state_total_pop = sum(total_pop),
           state_fema_decInStateYear = sum(fema_decInCountyYear, na.rm = TRUE),
           state_fema_binary = ifelse(state_fema_decInStateYear > 0, 1, 0),
           state_fema_propCountyWithDec = sum(fema_binary)/n_countiesInState,
           state_total_votes = sum(total_votes),
           state_DVP = sum(DEM)/state_total_votes,
           state_RVP = sum(REP)/state_total_votes,
           state_n_transcripts = sum(n_transcripts, na.rm=TRUE),
           state_n_meetingTypes = sum(n_meetingTypes, na.rm = TRUE),
           state_n_ccMentions = sum(n_ccMentions, na.rm = TRUE),
           state_ccBinary = ifelse(state_n_ccMentions > 0, 1, 0),
           state_n_scriptCCMention = sum(n_scriptCCMention, na.rm = TRUE),
           state_prop_ccMentions = state_n_ccMentions/state_n_transcripts,
           state_prop_scriptCCMentions = state_n_scriptCCMention/state_n_transcripts,
           across(c(overall_cvi, baseline_all, baseline_health, baseline_socioEcon, 
                       baseline_infrastructure, baseline_environ, climate_all, climate_health, 
                       climate_socioEcon, climate_extreme, med_age, med_grossRent, 
                       med_hhic, edu_percentPop, perc_hispanic, perc_other, perc_black, perc_white),
                  ~ sum(.x, na.rm=TRUE)/n_countiesInState, .names = "state_{.col}")) %>% 
    select(transcript_year, starts_with("state_"), n_countiesInState) %>% 
    distinct(transcript_year, state_name, .keep_all = TRUE)
    
# save(allData_state, file = "./LocalView/data/modified/allData_State.rdata")

