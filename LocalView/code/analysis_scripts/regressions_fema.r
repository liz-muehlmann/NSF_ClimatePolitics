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
source("./LocalView/code/analysis_scripts/regressions_preliminaries.r")
load("./LocalView/data/modified/lvFema_allDeclarations.rdata")
load("./LocalView/data/modified/lvFema_transcript.rdata") 
load("./LocalView/data/modified/lvFema_all.rdata")

# lvf_transcript$time_btwn_decMeetingFactor <- relevel(factor(lvf_transcript$time_btwn_decMeetingFactor), ref = "1 month")

lvf_transcript$time_btwn_decMeetingFactor <- factor(lvf_transcript$time_btwn_decMeetingFactor, 
                                                    levels = c("1 month", 
                                                               "2 months", 
                                                               "3-6 months",
                                                               "7-9 months",
                                                               "10-12 months",
                                                               "1 year",
                                                               "2 years",
                                                               "3 years",
                                                               "4 years",
                                                               "5 years",
                                                               "6+ years"))
lvf_transcript$nDec_FiveYearsFactor <- as.factor(lvf_transcript$nDec_FiveYears)
lvFema$time_btwn_decMeetingFactor <- relevel(factor(lvFema$time_btwn_decMeetingFactor), ref = "1 month")

#   ____________________________________________________________________________
#   All-data all-years                                                      ####

##  ............................................................................
##   ccgwBinary ~ days since declaration                                    ####

days_sinceDec_ad <- lm(ccgwBinary ~ time_btwn_decMeetingFactor + census_division +
                        DVP, data = lvFema)

#   ____________________________________________________________________________
#   Transcript level, disaster closest to the meeting date                  ####

##  ............................................................................
##  ccgwBinary ~ |number of declarations in the last five years|            ####

days_fiveYears <- lm(ccgwBinary ~ nDec_FiveYears + census_division + 
                         DVP, data = lvf_transcript)

# modelsummary(days_fiveYears,
#              coef_map = coef_fema,
#              stars = stars,
#              title = title_fema,
#              gof_omit = gof_omit,
#              gof_map = gof,
#              notes = notes_fema,
#              output = "gt")

##  ............................................................................
##  ccgwBinary ~ days since declaration                                     ####

days_sinceDec_tl <- lm(ccgwBinary ~ time_btwn_decMeetingFactor + census_division +
                        DVP, data = lvf_transcript)

# modelsummary(days_sinceDec_tl,
#              coef_map = coef_fema,
#              stars = stars,
#              title = title_fema,
#              gof_omit = gof_omit,
#              gof_map = gof,
#              notes = notes_fema,
#              output = "gt")

plot_model(days_sinceDec_tl, 
           type = "pred", 
           terms = "time_btwn_decMeetingFactor",
           colors = "viridis") +
    theme_sjplot(base_size = 12, base_family = "serif") +
    labs(title = "Predicted Values of Climate Change or Global Warming use",
         x = "Time between FEMA declaration and meeting (Factor)",
         y = "Predicted values of Climate Change or Global Warming") 

##  ............................................................................
##  ccgwBinary ~ days since declaration interaction                         ####

days_sinceDectl_int <- lm(ccgwBinary ~ time_btwn_decMeetingFactor + census_division +
                           DVP + DVP*time_btwn_decMeetingFactor, 
                       data = lvf_transcript)

## interaction
plot_model(days_sinceDectl_int, type = "int") + 
    theme_sjplot(base_size = 12, base_family = "serif") +
    labs(title = "Predicted Values of Climate Change or Global Warming mention",
         x = "Time between FEMA declaration and meeting (Factor)",
         y = "Predicted values of Climate Change or Global Warming") 

## interaction with mean/standard deviation
plot_model(days_sinceDectl_int, type = "int", mdrt.values = "meansd") +
    theme_sjplot(base_size = 12, base_family = "serif") +
    labs(title = "Predicted Values of Climate Change or Global Warming mention",
         x = "Time between FEMA declaration and meeting (Factor)",
         y = "Predicted values of Climate Change or Global Warming") 


##  ............................................................................
##  ccgwBinary ~ nDec_FiveYearsFactor

n_Dectl_int <- lm(ccgwBinary ~ nDec_FiveYearsFactor + census_division +
                              DVP + DVP*nDec_FiveYearsFactor, 
                          data = lvf_transcript)


plot_model(days_sinceDectl_int, type = "int",  mdrt.values = "meansd")  +
    theme_sjplot(base_size = 12, base_family = "serif") +
    labs(title = "Predicted Values of Climate Change or Global Warming mention",
         x = "Number of Disaster Declarations Last Five Years (Factor)",
         y = "Predicted values of Climate Change or Global Warming") 

#   ____________________________________________________________________________
#   add in political lean | transcript level                                ####

tl_withLean <- lvf_transcript %>% 
    mutate(party_lean = ifelse(round(DVP, 2) <= .50, "Leans Republican", "Leans Democratic")) 

ggplot(tl_withLean, aes(x = n_ccgwMentions, fill = party_lean)) +
    geom_histogram(alpha = 0.6, bins = 30) +
    labs(x = "Number of Climate Change or Global Warming Mentions in a Transcript", 
         y = "Number of Transcripts", title = "Histogram of Transcript Level Climate Change/Global Warming Mentions by Partisanship") +
    theme_minimal() +
    scale_fill_manual(values = c("Leans Republican" = "red", "Leans Democratic" = "blue"))

##  ............................................................................
##  political lean | place level                                            ####

place_withLean <- tl_withLean %>% 
    group_by(place_fips, party_lean)%>%
    summarize(place_nDec = sum(nDec_FiveYears), .groups = "drop")
    
ggplot(place_withLean, aes(x = place_nDec, fill = party_lean)) +
    geom_histogram(alpha = 0.6, bins = 30) +
    labs(x = "Number of Declarations", y = "Number of Places", title = "Histogram of Declarations by Partisanship") +
    theme_minimal() +
    scale_fill_manual(values = c("Leans Republican" = "red", "Leans Democratic" = "blue"))








