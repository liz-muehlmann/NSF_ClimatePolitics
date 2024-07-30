################################################################################
##                                                                            ##
##                                     Substantive Regressions                ##
##                                                                            ##
##  Data: County-Year with NA for no transcript in that county-year           ##
##  Model: climate change mention (DV) ~ partisanship of county (main IV)     ##
##  DV: Proportion of climate change mentions in a county-year                ##
##  IV: Democratic Voting Percentage + Controls                               ##
##  Controls: log total pop, log median household income; median age,         ##
##            percent white, non-hispanic;                                    ##
##            population with college education, rural-urban-suburban,        ##
##            climate change vulnerability index, binary indicator for        ##
##            disaster declaration in previous year                           ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   Load preliminaries file                                                 ####
source("./LocalView/code/analysis_scripts/regressions_preliminaries.r")


#   ____________________________________________________________________________
#   Substantive Main Model                                                  ####

main_model <- lm(prop_ccMentions ~ DVP, data = allData_withNA)

##  ............................................................................
##   Substantive Model with Controls                                        ####

model_controls <- lm(
  prop_ccMentions ~ DVP + log(total_pop) + log(med_hhic) + med_age +
    perc_white + edu_percentPop + as.factor(rural_urban_3pt) +
    overall_cvi + as.factor(fema_binary),
  data = allData_withNA
)

##  ............................................................................
##   Substantive Model with Census Division Fixed Effects                   ####

model_divisionFE <- plm(
  prop_ccMentions ~ DVP + log(total_pop) + log(med_hhic) + med_age + perc_white +
    edu_percentPop + as.factor(rural_urban_3pt) + overall_cvi +
    as.factor(fema_binary),
  data = allData_withNA, index = c("census_division"), model = "within"
)

##  ............................................................................
##   Substantive Model with Time as a Linear Variable                       ####

model_linear_time <- lm(prop_ccMentions ~ DVP + log(total_pop) + log(med_hhic) +
  med_age + perc_white + edu_percentPop +
  as.factor(rural_urban_3pt) + overall_cvi +
  as.factor(fema_binary) +
  as.numeric(transcript_year), data = allData_withNA)


##  ............................................................................
##  Substantive Model with Time as a Dummy Variable                         ####

model_dummy_time <- lm(prop_ccMentions ~ DVP + log(total_pop) + log(med_hhic) +
  med_age + perc_white + edu_percentPop +
  as.factor(rural_urban_3pt) + overall_cvi +
  as.factor(fema_binary) +
  as.factor(transcript_year), data = allData_withNA)

#   ____________________________________________________________________________
#   substantive regressions by year                                         ####

for (y in unique(allData_noNA$transcript_year)) {
  df <- allData_noNA %>%
    filter(transcript_year == y)

  if (y == 2010) {
    ## 2010 has no transcripts in with prior year FEMA declaration

    m_sub <- lm(
      prop_ccMentions ~ DVP + perc_white + log(med_hhic) + edu_percentPop +
        as.factor(rural_urban_3pt) + overall_cvi + log(total_pop) + fema_binary,
      data = df
    )
  } else {
    m_sub <- lm(
      prop_ccMentions ~ DVP + perc_white + log(med_hhic) + edu_percentPop +
        as.factor(rural_urban_3pt) + overall_cvi + log(total_pop) +
        as.factor(fema_binary),
      data = df
    )
  }
  assign(paste("substantive", y, sep = "_"), m_sub)
}

##  ............................................................................
#   save substantive regressions 2010-2016                                  ####

# modelsummary(list("2010" = substantive_2010,
#                   "2011" = substantive_2011,
#                   "2012" = substantive_2012,
#                   "2013" = substantive_2013,
#                   "2014" = substantive_2014,
#                   "2015" = substantive_2015,
#                   "2016" = substantive_2016),
#              coef_map = coef,
#              stars = stars,
#              title = title_substantive,
#              gof_omit = gof_omit,
#              gof_map = gof,
#              notes = notes,
#              output = "./LocalView/results/regressions/substance/2010-2016_Substantive.docx")


##  ............................................................................
#   save substantive regressions 2017-2023                                  ####

# modelsummary(list("2017" = substantive_2017,
#                   "2018" = substantive_2018,
#                   "2019" = substantive_2019,
#                   "2020" = substantive_2020,
#                   "2021" = substantive_2021,
#                   "2022" = substantive_2022,
#                   "2023" = substantive_2023),
#              coef_map = coef,
#              stars = stars,
#              title = title_substantive,
#              gof_omit = gof_omit,
#              gof_map = gof,
#              notes = notes,
#              output = "./LocalView/results/regressions/substance/2017-2023_Substantive.docx")


################################################################################
##                                                                            ##
##                                     Substantive Regressions                ##
##                                                                            ##
##  Data: County-Year with NA for no transcript in that county-year           ##
##  Model: climate change mention (DV) ~ partisanship of county (main IV)     ##
##  DV: Proportion of transcripts with change mentions in a county-year       ##
##  IV: Democratic Voting Percentage + Controls                               ##
##  Controls: log total pop, log median household income; median age,         ##
##            percent white, non-hispanic;                                    ##
##            population with college education, rural-urban-suburban,        ##
##            climate change vulnerability index, binary indicator for        ##
##            disaster declaration in previous year                           ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   Substantive Main Model                                                  ####

main_model <- lm(prop_scriptCCMention ~ DVP, data = allData_withNA)

##  ............................................................................
##   Substantive Model with Controls                                        ####


model_controls <- lm(
  prop_scriptCCMention ~ DVP + log(total_pop) + log(med_hhic) + med_age +
    perc_white + edu_percentPop + as.factor(rural_urban_3pt) + overall_cvi +
    as.factor(fema_binary),
  data = allData_withNA
)

##  ............................................................................
##   Substantive Model with Census Division Fixed Effects                   ####

model_divisionFE <- plm(
  prop_scriptCCMention ~ DVP + log(total_pop) + log(med_hhic) + med_age +
    perc_white + edu_percentPop + as.factor(rural_urban_3pt) + overall_cvi +
    as.factor(fema_binary),
  data = allData_withNA, index = c("census_division"), model = "within"
)

##  ............................................................................
##   Substantive Model with Time as a Linear Variable                       ####

model_linear_time <- lm(prop_scriptCCMention ~ DVP + log(total_pop) +
  log(med_hhic) + med_age + perc_white + edu_percentPop +
  as.factor(rural_urban_3pt) + overall_cvi + as.factor(fema_binary) +
  as.numeric(transcript_year), data = allData_withNA)


##  ............................................................................
##  Substantive Model with Time as a Dummy Variable                         ####

model_dummy_time <- lm(prop_scriptCCMention ~ DVP + log(total_pop) +
  log(med_hhic) + med_age + perc_white + edu_percentPop +
  as.factor(rural_urban_3pt) + overall_cvi + as.factor(fema_binary) +
  as.factor(transcript_year), data = allData_withNA)

#   ____________________________________________________________________________
#   substantive regressions by year                                         ####

for (y in unique(allData_noNA$transcript_year)) {
  df <- allData_noNA %>%
    filter(transcript_year == y)

  if (y == 2010) {
    ## 2010 has no transcripts in with prior year FEMA declaration

    m_sub <- lm(
      prop_scriptCCMention ~ DVP + perc_white + log(med_hhic) + edu_percentPop +
        as.factor(rural_urban_3pt) + overall_cvi + log(total_pop) + fema_binary,
      data = df
    )
  } else {
    m_sub <- lm(
      prop_scriptCCMention ~ DVP + perc_white + log(med_hhic) + edu_percentPop +
        as.factor(rural_urban_3pt) + overall_cvi + log(total_pop) +
        as.factor(fema_binary),
      data = df
    )
  }
  assign(paste("substantive", y, sep = "_"), m_sub)
}

##  ............................................................................
#   save substantive regressions 2010-2016                                  ####

# modelsummary(list("2010" = substantive_2010,
#                   "2011" = substantive_2011,
#                   "2012" = substantive_2012,
#                   "2013" = substantive_2013,
#                   "2014" = substantive_2014,
#                   "2015" = substantive_2015,
#                   "2016" = substantive_2016),
#              coef_map = coef,
#              stars = stars,
#              title = title_substantive,
#              gof_omit = gof_omit,
#              gof_map = gof,
#              notes = notes,
#              output = "./LocalView/results/regressions/substance/2010-2016_Substantive_propScript.docx")


##  ............................................................................
#   save substantive regressions 2017-2023                                  ####

# modelsummary(list("2017" = substantive_2017,
#                   "2018" = substantive_2018,
#                   "2019" = substantive_2019,
#                   "2020" = substantive_2020,
#                   "2021" = substantive_2021,
#                   "2022" = substantive_2022,
#                   "2023" = substantive_2023),
#              coef_map = coef,
#              stars = stars,
#              title = title_substantive,
#              gof_omit = gof_omit,
#              gof_map = gof,
#              notes = notes,
#              output = "./LocalView/results/regressions/substance/2017-2023_Substantive_propScript.docx")
#
