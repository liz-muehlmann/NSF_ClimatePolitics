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
lvFema <- left_join(lv, fema, relationship = "many-to-many") %>%
  mutate(
    days_btwn_decMeeting = difftime(declaration_date, meeting_date,
      unit = "days")) %>%
    filter(days_btwn_decMeeting <= 0) %>%
    mutate(
      days_btwn_decMeeting = abs(days_btwn_decMeeting),
      time_btwn_decMeetingFactor = case_when(
        days_btwn_decMeeting <= 30 ~ "Zero to One month",
        days_btwn_decMeeting >= 31 &
          days_btwn_decMeeting <= 60 ~ "Two months",
        days_btwn_decMeeting >= 61 &
          days_btwn_decMeeting <= 90 ~ "Three months",
        days_btwn_decMeeting >= 91 &
          days_btwn_decMeeting <= 120 ~ "Four months",
        days_btwn_decMeeting >= 121 &
          days_btwn_decMeeting <= 150 ~ "Five months",
        days_btwn_decMeeting >= 151 &
          days_btwn_decMeeting <= 365 ~ "Six months to a year",
        days_btwn_decMeeting >= 366 &
          days_btwn_decMeeting <= 730 ~ "2 years",
        days_btwn_decMeeting >= 731 &
          days_btwn_decMeeting <= 1095 ~ "3 years",
        days_btwn_decMeeting >= 1096 &
          days_btwn_decMeeting <= 1460 ~ "4 years",
        days_btwn_decMeeting >= 1461 &
          days_btwn_decMeeting <= 1825 ~ "5 years",
        days_btwn_decMeeting >= 1826 ~ "6 or more years"
      )
    ) %>%
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
  mutate(nDec_FiveYears = sum(`2 years`, `5 years`, `Six months to a year`, 
                              `3 years`, `4 years`, `Four months`, 
                              `Three months`, `Two months`, 
                              `Zero to One month`, `Five months`)) %>%
  select(-`2 years`, -`5 years`, -`Six months to a year`,
         -`3 years`, -`4 years`, -`Four months`,
         -`Three months`, -`Two months`,
         -`Zero to One month`, -`Five months`) %>% 
  rename(nDec_SixYears = `6 or more years`)

# save(lvf_fiveYears, file = "./LocalView/data/modified/lvFema_allDeclarations.rdata")

##  ............................................................................
##  transcript level                                                        ####

# n = 103,350
lvf_transcript <- lvf_fiveYears %>%
  group_by(transcript_id) %>%
  slice(which.min(days_btwn_decMeeting))  %>% 
  ungroup() %>% 
  mutate(
    days_btwn_decMeeting = abs(days_btwn_decMeeting),
    time_btwn_decMeetingFactor = case_when(
      days_btwn_decMeeting <= 30 ~ "Zero to One month",
      days_btwn_decMeeting >= 31 &
        days_btwn_decMeeting <= 60 ~ "Two months",
      days_btwn_decMeeting >= 61 &
        days_btwn_decMeeting <= 90 ~ "Three months",
      days_btwn_decMeeting >= 91 &
        days_btwn_decMeeting <= 120 ~ "Four months",
      days_btwn_decMeeting >= 121 &
        days_btwn_decMeeting <= 150 ~ "Five months",
      days_btwn_decMeeting >= 151 &
        days_btwn_decMeeting <= 365 ~ "Six months to a year",
      days_btwn_decMeeting >= 366 &
        days_btwn_decMeeting <= 730 ~ "2 years",
      days_btwn_decMeeting >= 731 &
        days_btwn_decMeeting <= 1095 ~ "3 years",
      days_btwn_decMeeting >= 1096 &
        days_btwn_decMeeting <= 1460 ~ "4 years",
      days_btwn_decMeeting >= 1461 &
        days_btwn_decMeeting <= 1825 ~ "5 years",
      days_btwn_decMeeting >= 1826 ~ "6 or more years"
    ))

# save(lvf_transcript, file = "./LocalView/data/modified/lvFema_transcript.rdata")

