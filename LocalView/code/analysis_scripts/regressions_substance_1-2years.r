################################################################################
##                                                                            ##
##                          Substantive Regressions                           ##
##                                                                            ##
##  Data: transcript-level local view data with fema/ noaa information        ##
##  Model: climate change or global warming binary (DV)                       ##
##         ~ Days since FEMA declaration or NOAA episode                      ##
##  DV: binary indicator for if there was a CC or GW mention                  ##
##  IV: Days since disaster declaration or episode, last one or two years     ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

source("./LocalView/code/analysis_scripts/regression_preliminaries.r")
load("./LocalView/data/modified/merged_datasets/allData_transcriptLevel.rdata")

allData_transcriptLevel <- allData_transcriptLevel %>% 
    mutate(log_totalPop = log(total_pop),
           log_medhhic = log(med_hhic),
           party_lean = ifelse(round(DVP, 2) <= .50, 
                               "Leans Republican", "Leans Democratic"),
           anyDec_fiveYears = ifelse(nDec_fiveYears > 0, 1, 0),
           anyEpisode_fiveYears = ifelse(nEpisode_fiveYears > 0, 1, 0),
           transcript_year = as.numeric(transcript_year)) 
