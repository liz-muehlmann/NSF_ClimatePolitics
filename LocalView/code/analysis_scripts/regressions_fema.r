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
load("./LocalView/data/modified/lvFema_transcriptLevel.rdata") 

lvFema_transcriptLevel$nDec_fiveYearsFactor <- as.factor(lvFema_transcriptLevel$nDec_fiveYears)
lvFema_transcriptLevel$nDec_sixYearsFactor <- as.factor(lvFema_transcriptLevel$nDec_sixYears)
lvFema_transcriptLevel$transcript_year <- as.numeric(str_sub(lvFema_transcriptLevel$meeting_date, 1,4))

#   ____________________________________________________________________________
#   define control variables

lvFema_transcriptLevel.controls <-c("rural_urban_3pt", 
                                     "log(total_pop)",
                                     "log(med_hhic)",
                                     "perc_white",
                                     "edu_percentPop",
                                     "overall_cvi")

#   ____________________________________________________________________________
#   Transcript level, disaster closest to the meeting date                  ####

##  ............................................................................
##  RQ1: Is climate change being discussed? And how?

rq1a <- lm(ccgwBinary ~ census_division + transcript_year, data = lvFema_transcriptLevel)

rq1b <- lm(ccgwBinary ~ census_division + as.factor(transcript_year), data = lvFema_transcriptLevel)

rq1ba <- plot_model(rq1b, 
           type = "pred", 
           terms = "transcript_year",
           colors = "viridis") +
    theme_sjplot(base_size = 12, base_family = "serif") +
    labs(title = "Predicted Values of Climate Change or Global Warming Mentions By Year",
         x = "Meeting Year",
         y = "Predicted values of Climate Change or Global Warming") 

rq1c <- better_lm(data = lvFema_transcriptLevel,
          dv = "ccgwBinary",
          iv = "census_division",
          controls = lvFema_transcriptLevel.controls)



##  ............................................................................
##  RQ2: Does this vary by vote share?                                      ####

    







































































##  ............................................................................
##  ccgwBinary ~ |number of declarations in the last five years|            ####

days_fiveYears <- lm(ccgwBinary ~ nDec_FiveYears + census_division + 
                         DVP, data = lvFema_transcriptLevel)

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
                        DVP, data = lvFema_transcriptLevel)

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
                       data = lvFema_transcriptLevel)

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
                          data = lvFema_transcriptLevel)


plot_model(days_sinceDectl_int, type = "int",  mdrt.values = "meansd")  +
    theme_sjplot(base_size = 12, base_family = "serif") +
    labs(title = "Predicted Values of Climate Change or Global Warming mention",
         x = "Number of Disaster Declarations Last Five Years (Factor)",
         y = "Predicted values of Climate Change or Global Warming") 

#   ____________________________________________________________________________
#   add in political lean | transcript level                                ####

tl_withLean <- lvFema_transcriptLevel %>% 
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








