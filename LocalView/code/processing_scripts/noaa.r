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

noaa <- read.csv("./LocalView/data/original/")