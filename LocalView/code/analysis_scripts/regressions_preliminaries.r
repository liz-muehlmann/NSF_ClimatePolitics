################################################################################
##                                                                            ##
## This file includes the preliminary steps for the regressions               ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load packages and data                                                  ####

library(tidyverse) # data manipulation
library(plm) # fixed effects
library(modelsummary) # regression output tables
library(gt) # save regressions
library(strcode) # easy code separators
options(strcode = list(
  insert_with_shiny = FALSE, # set options
  char_length = 100,
  hash_in_sep = TRUE
))

load("./LocalView/data/modified/allData_noNA.rdata")
load("./LocalView/data/modified/allData_withNA.rdata")


# #   __________________________________________________________________________
# #   group by year and state+county FIPS                                   ####
#
# allData_withNA <- allData_withNA %>%
#     group_by(transcript_year, stcounty_fips)
#
# allData_noNA <- allData_noNA %>%
#     group_by(transcript_year, state_name, county_name)

## formatting ##################################################################
##                                                                            ##
##                                     Regression Formatting                  ##
##                                                                            ##
################################################################################
##  ............................................................................
##  format goodness of fit numbers                                          ####

f <- function(x) format(round(x, 3), big.mark = ",")
gof <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = f),
  list("raw" = "r.squared", "clean" = "R-Squared", "fmt" = 3)
)
gof_omit <- "DF|Deviance|R2 Adj.|AIC|BIC|Log.Lik.|F|RMSE"
stars <- c("*" = 0.05, "**" = 0.01)

##  ............................................................................
##  title and notes                                                         ####

title_inclusion <- "What Predicts Selection Into Our Sample"
title_substantive <- "Linear Regression Democratic Vote Percentage's 
                      Effect on Proportion of Climate Change Mentions"
notes <- "Notes: Cell entries are linear regression coefficients 
          with standard errors in parentheses."

coef <- c(
  `(Intercept)` = "Constant",
  DVP = "Democratic Voting Percentage",
  `log(total_pop)` = "Log of Total Population",
  `log(med_hhic)` = "Log of Median Household Income",
  med_age = "Median Age",
  perc_white = "Percent white, non-Hispanic",
  edu_percentPop = "Percent Population with College Degree",
  `as.factor(rural_urban_3pt)2` = "Suburban",
  `as.factor(rural_urban_3pt)3` = "Rural",
  overall_cvi = "Overall Climate Vulnerability",
  `as.factor(fema_binary)1` = "FEMA disaster declared in previous year",
  `as.numeric(transcript_year)` = "Meeting Year",
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
  `as.factor(transcript_year)2023` = "2023"
)
