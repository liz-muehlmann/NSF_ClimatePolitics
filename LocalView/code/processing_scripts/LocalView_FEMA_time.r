### file description ###############################################################################
##                                                                                                ##
## This file uses the complete clean local view data without transcripts to calculate the number  ##
##      of days between a FEMA disaster declaration and the meeting date                          ##
##                                                                                                ##
##      Data included:                                                                            ##
##          Local View (2010-2023)                                                                ##
##              https://doi.org/10.7910/DVN/NJTBEM                                                ##   
##              ./processing_scripts/local_view.r                                                 ##
##          FEMA (2010-2023)                                                                      ## 
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2        ##
##                                                                                                ##
#################################################################################################### 


#   ________________________________________________________________________________________________
#   load preliminaries and data                                                                 ####

library(lubridate)
source("./LocalView/code/processing_scripts/preliminaries.r")
load("./LocalView/data/modified/lv_clean_noTranscript.rdata")
load("./LocalView/data/modified/fema_disasterYear.rdata") 

lv <- lvClean_noScript %>% 
    group_by(stcounty_fips, transcript_year) %>% 
    mutate(meeting_date = make_date(transcript_year, transcript_month, transcript_day),
           n_meetings_inCountyYear = n()) %>% 
    ungroup() %>% 
    group_by(stcounty_fips, meeting_date) %>% 
    mutate(n_meetings_onCountyDate = n()) %>% 
    select(-transcript_year, -transcript_day, -transcript_month) %>% 
    ungroup()

fema <- fema %>% 
    mutate(declaration_date = make_date(fema_year, fema_month, fema_day)) %>% 
    group_by(stcounty_fips, declaration_date) %>% 
    mutate(n_declarations_onCountyDate = n()) %>% 
    select(-fema_year, -fema_month, -fema_day) %>% 
    ungroup()

#   ________________________________________________________________________________________________
#   merge data                                                                                  ####

lvFema <- left_join(lv, fema, relationship="many-to-many") %>% 
    mutate(days_since_declaration = difftime(declaration_date, meeting_date, unit = "days"))

nearestDecl_beforeMeeting <- lvFema %>% 
    group_by(stcounty_fips, meeting_date) %>%
    filter(days_since_declaration < 0 & days_since_declaration == min(days_since_declaration)) %>% 
    select(state_name, county_name, stcounty_fips, ccBinary, n_ccMentions, fema_decInCountyYear, 
           fema_decTypeCountyYear, n_declarations_onCountyDate, meeting_date, declaration_date, 
           days_since_declaration)

# write.csv(nearestDecl_beforeMeeting, "./LocalView/results/summaries/LargeFiles/nearestDeclaration_beforeMeeting.csv", row.names = FALSE)
    


























