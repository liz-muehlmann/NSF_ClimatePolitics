################################################################################
##                                                                            ##
##                          Substantive Regressions                           ##
##                                                                            ##
##  Data: transcript-level local view data with noaa episode information      ##
##  Model: climate change or global warming binary (DV)                       ##
##         ~ Days since NOAA episode began                                    ##
##  DV: binary indicator for if there was a CC or GW mention                  ##
##  IV: Days since event began (1-5 mo, 6 mo)                                 ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

source("./LocalView/code/analysis_scripts/regression_preliminaries.r")
load("./LocalView/data/modified/lvNoaa_transcriptLevel.rdata")

lvNoaa_transcriptLevel <- lvNoaa_transcriptLevel %>% 
    mutate(log_totalPop = log(total_pop),
           log_medhhic = log(med_hhic),
           party_lean = ifelse(round(DVP, 2) <= .50, 
                               "Leans Republican", "Leans Democratic"),
           anyEpisode_fiveYears = ifelse(nEpisode_fiveYears > 0, 1, 0)) 

#   ____________________________________________________________________________
##  RQ1 - IS CLIMATE CHANGE BEING DISCUSSED & HOW                           ####
##                                                                            ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  RQ1 - Main Model                                                        ####
##  Between effects; linear time; county-level controls; county clustered S.E.

rq1 <- felm(ccgwBinary ~ rural_urban_3pt + log(total_pop) + log(med_hhic) + 
                perc_white + edu_percentPop + transcript_year + overall_cvi +
                census_division|0|0|stcounty_fips, data=lvNoaa_transcriptLevel)

##  Table 1: Is Climate Change Being Discussed?
modelsummary(rq1,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_felm,         
             glance = glance_custom.felm,
             title = "Table 1: Is Climate Change Being Mentioned?",
             # output = "gt",
             output = "./LocalView/results/regressions/substance/NOAA/250106_rq1_table1.docx"
) 

##  ............................................................................
##  RQ1 - ROBUSTNESS CHECKS                                                 ####
############################################################################# ##

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ1.1 - Penalized Maximum Likelihood                                    ####

rq1.1 <- glm(ccgwBinary ~ rural_urban_3pt + log_totalPop + log_medhhic +
                 perc_white + edu_percentPop + transcript_year + 
                 overall_cvi + census_division, 
             data=lvNoaa_transcriptLevel, family = "binomial", method = brglmFit)

## Table 1.1: Penalized Maximum Likelihood
modelsummary(rq1.1,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_pml,
             # glance = glance_custom.pml,
             title = "Table 1.1: Penalized Maximum Likelihood",
             # output = "gt",
             output = "./LocalView/results/regressions/substance/NOAA/250106_rq1_Table1-1_PML.docx"
) 

## Table 1.1a: Penalized Maximum Likelihood Marginal Effects - Average Slopes
rq1.1a <-marginaleffects::avg_slopes(rq1.1, 
                                     variables = c("rural_urban_3pt", 
                                                   "log_totalPop",  
                                                   "log_medhhic", 
                                                   "perc_white",
                                                   "edu_percentPop",    
                                                   "transcript_year",
                                                   "overall_cvi")) 

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ1.2 - Time as a Factor Variable                                      ####

##  Between effects; time as a factor; county-level controls; county clustered S.E.
rq1.2 <- felm(ccgwBinary ~ rural_urban_3pt + log(total_pop) + 
                  log(med_hhic) + perc_white + edu_percentPop + overall_cvi +
                  as.factor(transcript_year) + census_division|0|0|stcounty_fips, 
              data=lvNoaa_transcriptLevel)

## Table 1.2 - Is Climate Change Being Discussed (Time as a Factor)?
modelsummary(rq1.2,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 1.2: Is Climate Change Being Discussed (Linear Time)?",
             # output = "gt",
             output = "./LocalView/results/regressions/substance/NOAA/250106_rq1_Table1-2.docx"
) 

#   ____________________________________________________________________________
##  RQ2 - DOES CC/GW MENTION VARY BY VOTE SHARE?                            ####
##                                                                            ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  RQ2 - Main Models                                                       ####
##  Between effects with DVP; linear time; county-level controls; county clustered S.E.
rq2 <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + 
                perc_white + edu_percentPop + transcript_year + overall_cvi +
                census_division|0|0|stcounty_fips, 
            data=lvNoaa_transcriptLevel)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ2 - Between effects with DVP; Linear Time                                   ####
##  DVP*time Interaction; county-level controls; county clustered S.E.

rq2_int_linear <- felm(ccgwBinary ~ DVP*transcript_year + rural_urban_3pt + 
                           log(total_pop) + log(med_hhic) + perc_white + 
                           edu_percentPop + transcript_year + overall_cvi +
                           census_division|0|0|stcounty_fips, 
                       data=lvNoaa_transcriptLevel)

## Table 2: Do Climate Change/Global Warming Mentions Vary by Vote Percentage?
modelsummary(list(rq2, rq2_int_linear),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 2: Do Climate Change/Global Warming Mentions Vary by Vote Percentage?",
             # output = "gt",
             output = "./LocalView/results/regressions/substance/NOAA/250106_rq2_Table2.docx"
)


##  ............................................................................
##  RQ2 - ROBUSTNESS CHECKS                                                 ####
############################################################################# ##

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ2.1 - Within Effects; County FE; Linear Time                          ####
##   county-level controls; county clustered S.E.

rq2.1 <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + 
                  perc_white + overall_cvi + edu_percentPop + overall_cvi +
                  transcript_year|stcounty_fips|0|stcounty_fips, 
              data=lvNoaa_transcriptLevel)

## Table 2.1 Do Climate Change/Global Warming Mentions Vary by Vote Percentage?
modelsummary(rq2.1,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 2.1: Within Effects with County Fixed Effects",
             # output = "gt",
             output = "./LocalView/results/regressions/substance/NOAA/250106_rq2_Table2-1.docx"
) 


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ2.2 - Penalized Maximum Likelihood                                    ####

rq2.2 <- glm(ccgwBinary ~ DVP +  rural_urban_3pt + log_totalPop + log_medhhic +
                 perc_white + edu_percentPop + transcript_year + overall_cvi,
             data=lvNoaa_transcriptLevel,
             family = "binomial", method = brglmFit)

## Table 2.2: Penalized Maximum Likelihood
modelsummary(rq2.2,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_pml,
             # glance = glance_custom.pml,
             title = "Table 2.2: Penalized Maximum Likelihood",
             # output = "gt",
             output = "./LocalView/results/regressions/substance/NOAA/250106_rq2_Table2-2.docx"
)


## Table 2.1a: Penalized Maximum Likelihood Marginal Effects - Average Slopes
rq2.2a <-marginaleffects::avg_slopes(rq2.2, 
                                     variables = c("DVP", 
                                                   "rural_urban_3pt", 
                                                   "log_totalPop",
                                                   "log_medhhic",
                                                   "perc_white",
                                                   "edu_percentPop",
                                                   "transcript_year",
                                                   "overall_cvi"))

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ2.3 - Between effects with DVP; Factor time                                   ####
##  DVP*time Interaction; county-level controls; county clustered S.E.
rq2.3 <- felm(ccgwBinary ~ DVP*as.factor(transcript_year) + rural_urban_3pt + 
                  log(total_pop) + log(med_hhic) + perc_white + 
                  edu_percentPop + overall_cvi + as.factor(transcript_year)|0|0|stcounty_fips, 
              data=lvNoaa_transcriptLevel)

modelsummary(rq2.3,
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             # glance = glance_custom.pml,
             title = "Table 2.3: Between Effects, Time as a Factor",
             # output = "gt",
             output = "./LocalView/results/regressions/substance/NOAA/250106_rq2_Table2-3.docx"
)


#   ____________________________________________________________________________
##  RQ3 - ARE THERE MORE MENTIONS OF CC/GW AFTER NOAA EPISODES? ####
##                                                                            ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  RQ3 - Main Models                                                       ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ3 - Between effects; Any Episode                                     ####
###  linear time; county-level controls; county clustered S.E.
rq3_anyEpisode <- felm(ccgwBinary ~ anyEpisode_fiveYears + DVP + rural_urban_3pt + 
                       log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + 
                       transcript_year + overall_cvi + census_division|0|0|stcounty_fips, 
                   data = lvNoaa_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ3 - Between effects; Number of Episodes                          ####
###  linear time; county-level controls; county clustered S.E. 

rq3_nEpisode <- felm(ccgwBinary ~ nEpisode_fiveYears + DVP + rural_urban_3pt + 
                     log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + 
                     transcript_year + overall_cvi + census_division|0|0|stcounty_fips, 
                 data=lvNoaa_transcriptLevel)


## Table 3: Are there more mentions of CC/GW after natural disasters/extreme events?
modelsummary(list(rq3_anyEpisode, rq3_nEpisode),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 3: Is a place more likely to mention climate change/global warming after NOAA Episodes?",
             # output = "gt",
             output = "./LocalView/results/regressions/substance/NOAA/250106_rq3_Table3.docx"
)

#   ____________________________________________________________________________
##  RQ4 - DO NOAA EPISODES CLOSE THE PARTISAN GAP IN MENTIONS?              ####
##                                                                            ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  RQ4 - Main Models                                                       ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ4 - between effects with DVP; any episode                                ####
###  DVP*anyepisode; county-level controls; county clustered S.E.

rq4_anydvp <- felm(ccgwBinary ~ anyEpisode_fiveYears*DVP + rural_urban_3pt + 
                       log(total_pop) + log(med_hhic) + perc_white + 
                       edu_percentPop + transcript_year + overall_cvi +
                       census_division|0|0|stcounty_fips, 
                   data=lvNoaa_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ4 - between effects with DVP; n Episode                                  ####
###  DVP*nEpisode; county-level controls; county clustered S.E.

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
rq4_nEpisodedvp <- felm(ccgwBinary ~ nEpisode_fiveYears*DVP + rural_urban_3pt + 
                        log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + 
                        transcript_year + overall_cvi +
                        census_division|0|0|stcounty_fips, 
                    data=lvNoaa_transcriptLevel)

## Table 4: Do extreme weather events close the partisan gap in mentions?
modelsummary(list(rq4_anydvp, rq4_nEpisodedvp),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 4: Do NOAA Episodes close the partisan gap in mentions?",
             # output = "gt",
             output = "./LocalView/results/regressions/substance/NOAA/250106_rq4_Table4.docx"
)

##  ............................................................................
##  RQ4 - Plotted Interactions                                              ####

## plot theme
set_theme(base = theme_classic(),
          theme.font = "serif")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ4 - lm; nEpisode*DVP plot                                                 ####
### county-level controls; county clustered S.E.

nEpisode5_dvp_lm <- lm(ccgwBinary ~ nEpisode_fiveYears*DVP + rural_urban_3pt + 
                       log(total_pop) + log(med_hhic) + perc_white + edu_percentPop +
                       census_division + transcript_year + overall_cvi,
                   data = lvNoaa_transcriptLevel)

plot_model(nEpisode5_dvp_lm, 
           type = "int",
           axis.title = c("Number of Episodes Last 5 Years", 
                          "Climate Change/Global Warming Mention"),
           title = "Predicted Values of Climate Change/Global Warming Mention") 


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ4 - lm; anyEpisode*DVP plot                                               ####
### county-level controls; county clustered S.E.

anyEpisode5_dvp_lm <- lm(ccgwBinary ~ anyEpisode_fiveYears*DVP + rural_urban_3pt + 
                         log(total_pop) + log(med_hhic) + perc_white + edu_percentPop +
                         census_division + transcript_year + overall_cvi,
                     data = lvNoaa_transcriptLevel)

plot_model(anyEpisode5_dvp_lm, 
           type = "int",
           axis.title = c("Any NOAA Episode Last Five Years", 
                          "Climate Change/Global Warming Mention"),
           title = "Predicted Values of Climate Change/Global Warming Mention") 
