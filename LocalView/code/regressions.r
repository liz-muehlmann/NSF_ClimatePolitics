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
library(modelsummary)
df <- read.csv("./LocalView/data/modified/LVCounty.csv") 

### v2 #########################################################################
##                                                                            ##
##          grouped by transcript year, state name, and county name           ##
##                                                                            ##
################################################################################
## model v2 data
df1 <- df %>% 
    group_by(transcript_year, state_name, county_name)

df1_noNA <- df1 %>% filter(!is.na(n_meettype))

df_binary <- df %>% 
    mutate(hasScript = ifelse(!is.na(sum_scriptCC), 1, 0))

## Main model: Regression results (meeting level data) with climate change mention (DV) ~ partisanship of county (main IV) 
main_model <- lm(prop_cc ~ DVP, data = df1)

## Main model adding controls (population, average income, share white non-hispanic, average age, average education)
model_controls <- lm(prop_cc ~ DVP + totalpop + edu_percentPop + perc_white, data = df1)

## Main model adding state FE (depending on how many meetings per state, in case too few observations per state)
model_stateFE <- plm(prop_cc ~ DVP + totalpop + edu_percentPop + perc_white, data = df1_noNA, index = c("state_name"), model="within")

## Main model adding time trend as linear variable
model_linear_time <- lm(prop_cc ~ DVP + as.numeric(transcript_year), data = df1)

## Main model adding years as dummy variables
model_dummy_time <- lm(prop_cc ~ DVP + as.factor(transcript_year), data = df1)

### v3 #########################################################################
##                                                                            ##
##                      predicting county inclusion                           ##
##                                                                            ##
################################################################################
## model inclusion data
df_inclusion <- df %>% 
    group_by(stcounty_fips) %>% 
    mutate(has_script = ifelse(!is.na(sum_scriptCC), 1, 0)) %>% 
    select(-n_meettype, -total_scriptCY, -sum_scriptCC, -prop_cc, -n_places_incounty)

## model: all counties, all years, binary - at least one transcript, controls, time as linear variable  
inclusion_linear <- lm(has_script ~ overall_CVI + perc_white + log(medhhic) + edu_percentPop + DVP + log(totalpop) + as.factor(ruralUrban) + as.numeric(transcript_year), data = df_inclusion)

inclusion_dummy <- lm(has_script ~ overall_CVI + perc_white + log(medhhic) + edu_percentPop + DVP + log(totalpop) + as.factor(ruralUrban) + as.factor(transcript_year), data = df_inclusion)

## Main model adding state FE (1 = script, 0 = no transcript)
inclusion_stateFE <- plm(hasScript ~ DVP + log(totalpop) + edu_percentPop + perc_white, data = df_binary, index = c("state_name"), model="within")

### save #######################################################################
##                                                                            ##
##                              save regressions                              ##
##                                                                            ##
################################################################################
## format goodness of fit numbers
f <- function(x) format(round(x, 3), big.mark=",")
gof <- list(
    list("raw" = "nobs", "clean" = "N", "fmt" = f),
    list("raw" = "r.squared", "clean" = "R-Squared", "fmt" = 3))
gof_omit = 'DF|Deviance|R2 Adj.|AIC|BIC|Log.Lik.|F|RMSE'
stars = c('*' = 0.05, '**' = 0.01)
title <- 'What Predicts Selection Into Our Sample'
notes = "Notes: Cell entries are linear regression coefficients with standard errors in parentheses."

coef <- c(`(Intercept)` = "Constant", 
              overall_CVI = "Overall Climate Vulnerability", 
              medhhic = "Median Household Income", 
              edu_percentPop = "Percent Population with College Degree", 
              DVP = "Democratic Voting Percentage",
              totalpop = "Total Population",
              `as.factor(ruralUrban)2` = "Counties in metro areas of 250,000 to 1 million population",
              `as.factor(ruralUrban)3` = "Counties in metro areas of fewer than 250,000 population",
              `as.factor(ruralUrban)4` = "Urban population of 20,000 or more, adjacent to a metro area",
              `as.factor(ruralUrban)5` = "Urban population of 20,000 or more, not adjacent to a metro area",
              `as.factor(ruralUrban)6` = "Urban population of 5,000 to 20,000, adjacent to a metro area",
              `as.factor(ruralUrban)7` = "Urban population of 5,000 to 20,000, not adjacent to a metro area",
              `as.factor(ruralUrban)8` = "Urban population of fewer than 5,000, adjacent to a metro area",
              `as.factor(ruralUrban)9` = "Urban population of fewer than 5,000, not adjacent to a metro area",
              `as.factor(transcript_year)` = "Year",
              `as.factor(transcript_year)2011` = "2011",
              `as.factor(transcript_year)2012` = "2012",
              `as.factor(transcript_year)2013` = "2013",
              `as.factor(transcript_year)2014` = "2014",
              `as.factor(transcript_year)2015` = "2015",
              `as.factor(transcript_year)2016` = "2016",
              `as.factor(transcript_year)2017` = "2017",
              `as.factor(transcript_year)2018` = "2018",
              `as.factor(transcript_year)2019` = "2019",
              `as.factor(transcript_year)2020` = "2020",
              `as.factor(transcript_year)2021` = "2021",
              `as.factor(transcript_year)2022` = "2022",
              `as.factor(transcript_year)2023` = "2023")

modelsummary(list("Linear Time" = inclusion_linear,
                  "Dummy Time" = inclusion_dummy),
             coef_map = coef,
             stars = stars,
             title = title,
             gof_omit = gof_omit,
             gof_map = gof,
             notes = notes,
             output = "gt")

# for(y in unique(df_inclusion$transcript_year)){
#     df <- df_inclusion %>%
#           filter(transcript_year == y)
#     m <- lm(has_script ~ overall_CVI + perc_white + medhhic + edu_percentPop + DVP + totalpop + as.factor(ruralUrban), data = df)
#     modelsummary(list(m),
#                  coef_map = coef,
#                  stars = stars,
#                  title = paste(y, title, sep=" "),
#                  gof_omit = gof_omit,
#                  gof_map = gof,
#                  notes = notes,
#                  output = paste0("./LocalView/results/regressions/samplePredictions_", y, ".docx"))
# }
