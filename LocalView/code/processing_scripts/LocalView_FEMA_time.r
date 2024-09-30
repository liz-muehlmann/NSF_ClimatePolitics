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

library(lubridate)                                             # work with dates
source("./LocalView/code/processing_scripts/analysis_prelims.r")      
load("./LocalView/data/modified/allData_transcriptLevel.rdata")
load("./LocalView/data/modified/fema_disasterYear.rdata")

# n = 103,350
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

# all disasters, all years
# n = 947,118
## NOTE: interval() returns a positive value if the start happened before the end
## declaration (start) happened before meeting (end) the value is positive
lvFema <- left_join(lv, fema, relationship = "many-to-many") %>%
  mutate(meeting_date = ymd(meeting_date),
         declaration_date = ymd(declaration_date),
         months_btwn_decMeeting = round(interval(declaration_date, meeting_date)/months(1), 2)) %>% 
  filter(months_btwn_decMeeting >= 0) %>% 
  mutate(time_btwn_decMeetingFactor = case_when(
    months_btwn_decMeeting <= 1 ~ "1 month",
    months_btwn_decMeeting <= 2 ~ "2 months",
    months_btwn_decMeeting <= 6 ~ "3-6 months",
    months_btwn_decMeeting <= 9 ~ "7-9 months",
    months_btwn_decMeeting <= 12 ~ "10-12 months",
    months_btwn_decMeeting <= 24 ~ "2 years",
    months_btwn_decMeeting <= 36 ~ "3 years",
    months_btwn_decMeeting <= 48 ~ "4 years",
    months_btwn_decMeeting <= 60 ~ "5 years",
    months_btwn_decMeeting >= 60.01 ~ "6+ years"
  )) %>% 
  group_by(transcript_id, time_btwn_decMeetingFactor) %>% 
  mutate(nDec_MeetingFactor = n()) %>% 
  ungroup()

# save(lvFema, file = "./LocalView/data/modified/lvFema_all.rdata")

##  ............................................................................
##  number of declarations last five years                                  ####

lvf_fiveYears <- lvFema %>%
  pivot_wider(
    names_from = time_btwn_decMeetingFactor,
    values_from = nDec_MeetingFactor, values_fill = 0, names_sep = "_") %>%
  rowwise() %>%
  mutate(nDec_FiveYears = sum(`1 month`, 
                              `2 months`, 
                              `3-6 months`, 
                              `7-9 months`, 
                              `10-12 months`, 
                              `2 years`, 
                              `3 years`, 
                              `4 years`, 
                              `5 years`)) %>%
  select(-`1 month`, -`2 months`, -`3-6 months`, -`7-9 months`, -`10-12 months`,
         -`2 years`, -`3 years`, -`4 years`, -`5 years`) %>% 
  rename(nDec_SixYears = `6+ years`)

# save(lvf_fiveYears, file = "./LocalView/data/modified/lvFema_allDeclarations.rdata")

##  ............................................................................
##  transcript level                                                        ####

# n = 103,350
lvf_transcript <- lvf_fiveYears %>%
  group_by(transcript_id) %>%
  slice(which.min(months_btwn_decMeeting)) %>% 
  mutate(time_btwn_decMeetingFactor = case_when(
    months_btwn_decMeeting <= 1 ~ "1 month",
    months_btwn_decMeeting <= 2 ~ "2 months",
    months_btwn_decMeeting <= 6 ~ "3-6 months",
    months_btwn_decMeeting <= 9 ~ "7-9 months",
    months_btwn_decMeeting <= 12 ~ "10-12 months",
    months_btwn_decMeeting <= 24 ~ "2 years",
    months_btwn_decMeeting <= 36 ~ "3 years",
    months_btwn_decMeeting <= 48 ~ "4 years",
    months_btwn_decMeeting <= 60 ~ "5 years",
    months_btwn_decMeeting >= 60.01 ~ "6+ years"
  )) %>% 
  ungroup()

# save(lvf_transcript, file = "./LocalView/data/modified/lvFema_transcript.rdata")


##  ............................................................................
##  transcripts with five or more declarations in the last five years       ####

nDec_fivePlus <- lvf_transcript %>%
  filter(nDec_FiveYears > 0 |
         nDec_SixYears > 0)

