################################################################################
##                                                                            ##
## This file includes the preliminary steps for the regressions               ##
##                                                                            ##
################################################################################


##  ............................................................................
##  Relevel the Rural-Urban codes so Suburban is default                    ####
##      Arguments:                                                            ##
##        df: a data frame                                                    ##
##      Returns:                                                              ##
##        A data frame with Rural, Suburban, Rural; Suburban is default      ##
##   ...........................................................................

refactor_ru <- function(df){
  df %>% 
    mutate(rural_urban_3pt = case_when(rural_urban_3pt == 1 ~ "Urban",
                                       rural_urban_3pt == 2 ~ "Suburban",
                                       rural_urban_3pt == 3 ~ "Rural"),
           rural_urban_3pt = as.factor(rural_urban_3pt),
           rural_urban_3pt = fct_relevel(rural_urban_3pt, c("Suburban", "Urban", 
                                                            "Rural")))
}

#   ____________________________________________________________________________
#   load packages and data                                                  ####

library(tidyverse)      # data manipulation
library(plm)            # fixed effects
library(modelsummary)   # regression output tables
library(gt)             # save regressions
library(sjPlot)         # plot predictions
library(sjmisc)         # utility functions for sjplot

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
##                          Regression Formatting                             ##
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
##  title and notes ccgw mention ~ DVP                                      ####

title_inclusion <- "What Predicts Selection Into Our Sample"
title_substantive <- "Linear Regression Democratic Vote Percentage's 
                      Effect on Proportion of Climate Change or Global Warming
                      Mentions"
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

##  ............................................................................
##  title and notes ccgwMention ~ days since declaration                    ####

title_fema <- "Linear Regression Climate Change or Global Warming Mention's 
               and Days Since Declaration"
notes_fema <- "Notes: Cell entries are linear regression coefficients 
          with standard errors in parentheses."

coef_fema <- c(
  `(Intercept)` = "Constant",
  DVP = "Democratic Voting Percentage",
  `as.factor(rural_urban_3pt)2` = "Suburban",
  `as.factor(rural_urban_3pt)3` = "Rural",
  `as.factor(days_since_decFactor)2 months` = "2 months",
  `as.factor(days_since_decFactor)3 months` = "3 months",
  `as.factor(days_since_decFactor)4 months` = "4 months",
  `as.factor(days_since_decFactor)5 months` = "5 months",
  `as.factor(days_since_decFactor)6+ months` = "6+ months",
  `census_division2` = "Middle Atlantic",
  `census_division3` = "East North Central",
  `census_division4` = "West North Central",
  `census_division5` = "South Atlantic",
  `census_division6` = "East South Central",
  `census_division7` = "West South Central",
  `census_division8` = "Mountain",
  `census_division9` = "Pacific"
)


#   ____________________________________________________________________________
#   improve lm function for adding controls                                 ####

better_lm <- function(data,
                  iv,
                  dv,
                  controls,
                  row.name = iv,
                  add.controls = NULL,
                  rm.controls = NULL,
                  caption = NULL) {
  model <-
    better_lm_return(data,
                       iv,
                       dv,
                       controls,
                       add.controls,
                       rm.controls)
  return((model))
}


better_lm_return <- function(data, iv, dv, controls, 
                             add.controls = NULL, rm.controls = NULL) {
  controls <- controls[!controls %in% rm.controls]
  if (!is.null(add.controls)) {
    controls <- unique(c(controls, add.controls))
  }
  
  # Only include controls if there are any
  if (length(controls) > 0) {
    controls <- paste(controls, collapse = " + ")
    iv <- paste(iv, controls, sep = " + ")
  }
  
  formula <- paste(dv, iv, sep = " ~ ")
  model <- lm(formula, data = data)
  return(model)
}
