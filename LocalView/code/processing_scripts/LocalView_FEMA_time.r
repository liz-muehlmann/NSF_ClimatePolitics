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

library(lubridate)          # work with dates
source("./LocalView/code/processing_scripts/regression_prelims.r") 
load("./LocalView/data/modified/allData_transcriptLevel.rdata")
load("./LocalView/data/modified/fema_disasterYear.rdata")

# n = 103,379
lv <- allData_transcriptLevel %>%
  mutate(
    meeting_date = make_date(transcript_year, transcript_month, transcript_day)
  ) %>%
  select(-transcript_year, -transcript_day, -transcript_month, -starts_with("fema_"))

# n = 30,135
fema <- fema %>%
  mutate(declaration_date = make_date(fema_year, fema_month, fema_day)) %>%
  select(-fema_year, -fema_month, -fema_day)

#   ____________________________________________________________________________
#   merge data                                                              ####

#n = 693,562
lvFema <- left_join(lv, fema, relationship = "many-to-many") %>%
  mutate(days_since_declaration = difftime(declaration_date, meeting_date, 
                                           unit = "days"),
         days_since_decFactor = case_when(days_since_declaration > 0 ~ "Declaration occurred after meeting date",
                                          days_since_declaration == 0 ~ "Declaration occured on meeting date",
                                          days_since_declaration >= -30 ~ "Declaration occured less than 30 days before meeting date",
                                          days_since_declaration <= -31 &
                                            days_since_declaration >= -60 ~ "Declaration occured 2 months before meeting date",
                                          days_since_declaration <= -61 &
                                            days_since_declaration >= -90 ~ "Declaration occured 3 months before meeting date",
                                          days_since_declaration <= -91 &
                                            days_since_declaration >= -120 ~ "Declaration occured 4 months before meeting date",
                                          days_since_declaration <= -121 &
                                            days_since_declaration >= -150 ~ "Declaration occured 5 months before meeting date",
                                          days_since_declaration <= -151 &
                                            days_since_declaration >= -364 ~ "Declaration occured 6 to 11 months before meeting date",
                                          days_since_declaration <= -365 ~ "Declaration occured 1 or more years before meeting date",
                                          is.na(days_since_declaration) ~ "No declaration in county")) %>% 
  select(stcounty_fips, state_name, county_name, transcript_id, meeting_date, 
         declaration_date, days_since_declaration, days_since_decFactor, 
         fema_id, fema_incidentType) %>% 
  group_by(transcript_id) %>%
  mutate(
    min_days_before = ifelse(any(days_since_declaration < 0), 
                             max(days_since_declaration[days_since_declaration < 0], na.rm = TRUE), 
                             NA),
    min_days_after = ifelse(any(days_since_declaration > 0), 
                            min(days_since_declaration[days_since_declaration > 0], na.rm = TRUE), 
                            NA),
    closest_meeting = case_when(
      days_since_declaration == min_days_before ~ "closest declaration before meeting",
      days_since_declaration == min_days_after ~ "closest declaration after meeting",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()


 # write.csv(nearestDecl_beforeMeeting, 
#           "./LocalView/results/summaries/LargeFiles/nearestDeclaration_beforeMeeting.csv", 
#           row.names = FALSE)

# save(dec_nearestMeeting, file = "./LocalView/data/modified/lv_fema_time.rdata")
