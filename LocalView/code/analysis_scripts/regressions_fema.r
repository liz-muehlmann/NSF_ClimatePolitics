################################################################################
##                                                                            ##
##                          Substantive Regressions                           ##
##                                                                            ##
##  Data: transcript-level local view data with fema meeting information      ##
##  Model: climate change or global warming binary (DV)                       ##
##         ~ Days since FEMA declaration                                      ##
##  DV: binary indicator for if there was a CC or GW mention                  ##
##  IV: Days since disaster declaration (1-5 mo, 6 mo)                        ##
##                                                                            ##
################################################################################


#   ____________________________________________________________________________
#   load data                                                               ####
source("./LocalView/code/processing_scripts/regression_prelims.r")
load("./LocalView/data/modified/lvFema_allDeclarations.rdata")
load("./LocalView/data/modified/lvFema_transcript.rdata")
load("./LocalView/data/modified/lvFema_all.rdata")

#   ____________________________________________________________________________
#   ccgwBinary ~ |number of declarations in the last five years|            ####

days_fiveYears <- lm(ccgwBinary ~ nDec_FiveYears + 
                census_division + DVP, data = lvf_transcript)

# modelsummary(days_since_dec,
#              coef_map = coef_fema,
#              stars = stars,
#              title = title_fema,
#              gof_omit = gof_omit,
#              gof_map = gof,
#              notes = notes_fema,
#              output = "gt")


#   ____________________________________________________________________________
#   ccgwBinary ~ days since declaration                                     ####

days_sinceDec <- lm(ccgwBinary ~ time_btwn_decMeetingFactor + census_division +
                        DVP, data = lvFema)
