################################################################################
##                                                                            ##
## This file runs linear regressions for the local view data                  ##
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

################################################################################
##                                                                            ##
##                     load preliminaries and packages                        ##
##                                                                            ##
################################################################################
library(tidyverse)
library(plm)
df <- read.csv("./Data/LocalView/LVModifiedData/LocalView_County.csv") %>% 
    group_by(transcript_year, state_name, county_name)

### grouped by transcript year, state name, and county name
## Main model: Regression results (meeting level data) with climate change mention (DV) ~ partisanship of county (main IV) 
main_model <- lm(prop_cc ~ DVP, data = df)

## Main model adding controls (population, average income, share white non-hispanic, average age, average education)
model_controls <- lm(prop_cc ~ DVP + totalpop + edu_percentPop + perc_white, data = df)

## Main model adding state FE (depending on how many meetings per state, in case too few observations per state)
model_stateFE <- plm(prop_cc ~ DVP, data = df, index = c("state_name"), model="within")

## Main model adding time trend as linear variable
model_linear_time <- lm(prop_cc ~ DVP + as.numeric(transcript_year), data = df)

## Main model adding years as dummy variables
model_dummy_time <- lm(prop_cc ~ DVP + as.factor(transcript_year), data = df)

