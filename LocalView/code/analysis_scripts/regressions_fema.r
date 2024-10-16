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
lvFema_transcriptLevel$transcript_yearFactor <- as.factor(lvFema_transcriptLevel$transcript_year)

## add in party lean
lvFema_transcriptLevel <- lvFema_transcriptLevel %>% 
    mutate(party_lean = ifelse(round(DVP, 2) <= .50, "Leans Republican", "Leans Democratic")) 

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
    labs(title = "Predicted Values of Climate Change or Global Warming Mention",
         x = "Meeting Year",
         y = "Predicted values of Climate Change or Global Warming (Binary)") 

rq1c <- better_lm(data = lvFema_transcriptLevel,
          dv.y = "ccgwBinary",
          iv.x = "census_division",
          controls = lvFema_transcriptLevel.controls)



##  ............................................................................
##  RQ2: Does this vary by vote share?                                      ####

rq2a <- lm(ccgwBinary ~ DVP + census_division + transcript_year, 
           data = lvFema_transcriptLevel)

rq2b <- lm(ccgwBinary ~ DVP*transcript_yearFactor + census_division,
           data = lvFema_transcriptLevel)

rq2ba <- plot_model(rq2b, type = "int") + 
    scale_colour_manual(values = c('#fe0000',
                                   '#fe6a00',
                                   '#ffd800',
                                   '#00fe21',
                                   '#0094fe',
                                   '#0026ff',
                                   '#b100fe',
                                   '#800001',
                                   '#803400',
                                   '#806b00',
                                   '#007f0e',
                                   '#00497e',
                                   '#001280',
                                   '#590080')) +
    theme_sjplot(base_size = 12, base_family = "serif") +
    labs(title = "Interaction between DVP & Transcript Year",
         x ="Democratic Vote Percentage",
         y =  "Climate Change or Global Warming Mention (Binary)") 

rq2c <- better_lm(data = lvFema_transcriptLevel,
                  dv.y = "ccgwBinary",
                  iv.x = "DVP",
                  controls = lvFema_transcriptLevel.controls,
                  add.controls = "census_division")


##  ............................................................................
##  RQ3: Do natural disasters act as focusing events?                       ####

rq3a <- lm(ccgwBinary ~ nDec_fiveYears + census_division, lvFema_transcriptLevel)

rq3b <- lm(ccgwBinary ~ time_btwn_decMeetingFactor + census_division, data = lvFema_transcriptLevel)



##  ............................................................................
##  RQ4: Does this close the partisan gap in # mentions?                    ####

rq4a <- lm(ccgwBinary ~ nDec_fiveYears*DVP + census_division, data = lvFema_transcriptLevel)

rq4ab <- plot_model(rq4a, type = "int") +
    theme_sjplot(base_size = 12, base_family = "serif") +
    labs(title = "Interaction between Number of Declarations in the last five years",
         x ="Number of declarations in the last five years",
         y =  "Climate Change or Global Warming Mention (Binary)") 

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
    
lvFema_transcriptLevel %>%
    group_by(place_fips, party_lean) %>%
    summarize(nDec_fiveYears = sum(nDec_fiveYears, na.rm = TRUE)) %>% 
    ggplot(aes(x = nDec_fiveYears, fill = party_lean)) +
    geom_histogram(alpha = 0.6, bins = 30) +
    labs(x = "Number of Declarations", 
         y = "Frequency", 
         title = "Histogram of Declarations by Partisanship (last five years)") +
    theme_minimal() +
    scale_fill_manual(values = c("Leans Republican" = "red", "Leans Democratic" = "blue"))

