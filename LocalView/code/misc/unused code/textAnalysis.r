################################################################################
##                                                                            ##
## This file does preliminary text analysis on all documents in the           ##
##      Local View data set                                                   ##
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
#   ____________________________________________________________________________
#   keywords in context                                                     ####
#   This section pulls various keywords in context using the local view data
#   Hurricane, Tornado, Wildfire, Heatwave, Extreme Heat, Tropical Storm
#   Flood (-ing, -s, -ed), drought, and wind



### libraries ##################################################################
##                                                                            ##
##                    load libraries and preliminaries                        ##
##                                                                            ##
################################################################################
library(quanteda)
library(tidytext)
source("./Code/LocalView/Analysis/processing.r")

### quanteda ###################################################################
##                                                                            ##
##                      text analysis using quanteda                          ##
##                                                                            ##
################################################################################

# keyword in context (Hazard Disaster words)
kwic <- kwic(tokens, pattern = hazard_disaster)
woi_hazardDisaster <- merge(kwic, docvars_df, by = "docname") %>% 
    as.data.frame()

# write.csv(woi_hazardDisaster, "./Results/Samples/LV WOI/woi_HazardDisaster.csv", row.names = FALSE)

## filter for only hurricanes, flooding, disaster, storm keywords
woi_climateStorm <- kwic_df  %>% 
    filter(keyword == "climate change" |
           keyword == "hurricane" |
           keyword == "flooding" |
           keyword == "disaster" | 
           keyword == "storm" |
           keyword == "hurricane ike" |
           keyword == "hurricane dolly" |
           keyword == "hurricane harvey" |
           keyword == "hurricane irma" |
           keyword == "hurricane imelda" |
           keyword == "hurricane laura" |
           keyword == "hurricane ian")

# write.csv(woi_climateStorm, "./Results/Samples/LV WOI/woi_climateStorm.csv", row.names = FALSE)

## subset only climate change keyword
woi_climateHurricane <- kwic_df %>% 
    filter(keyword == "climate change" |
           keyword == "hurricane ike" |
           keyword == "hurricane dolly" |
           keyword == "hurricane harvey" |
           keyword == "hurricane irma" |
           keyword == "hurricane imelda" |
           keyword == "hurricane laura" |
           keyword == "hurricane ian")

# write.csv(woi_climateHurricane, "./Results/Samples/LV WOI/woi_climatehurricane.csv", row.names = FALSE)

## subset only climate change keyword
woi_climateChange <- kwic_df %>% 
    filter(keyword == "climate change")

# write.csv(woi_climateChange, "./Results/Samples/LV WOI/woi_climateChange.csv", row.names=FALSE)

### summaries ##################################################################
##                                                                            ##
##                                                                            ##
################################################################################
kwcs_summary <- woi_climateStorm %>% 
    group_by(transcript_year, keyword) %>% 
    summarize(kcount = n())

kwch_summary <- woi_climateHurricane %>% 
    group_by(transcript_year, keyword) %>% 
    summarize(kcount = n())

kwcc_summary <- woi_climateChange %>% 
    group_by(transcript_year, keyword) %>% 
    summarize(kcount = n())

### graphs #####################################################################
##                                                                            ##
##                                                                            ##
################################################################################

## graph all keywords by year graph
kwcs_graph <- ggplot(data = kwcs_summary,
                     aes(x = transcript_year,
                         y = kcount,
                         group = keyword)) +
    geom_line(aes(color = keyword)) +
    ggtitle("Frequency of Hurricane & Climate Change Use - All Transcripts") +
    theme(plot.title = element_text(size=24, hjust = 0.5))

# ggsave("./LocalView/Results/Graphs/ClimateChangeStorm_ByYear.jpg", kwcs_graph)

## graph climate change and hurricanes by year
kwch_graph <- ggplot(data = kwch_summary,
                     aes(x = transcript_year,
                         y = kcount,
                         group = keyword)) +
    geom_line(aes(color = keyword)) +
    ggtitle("Frequency of Climate Change Use - All Transcripts") +
    theme(plot.title = element_text(size=24, hjust = 0.5))

# ggsave("./LocalView/Results/Graphs/ClimateChangeHurricane_ByYear.jpg", kwch_graph)

## graph climate change by year
kwcc_graph <- ggplot(data = kwcc_summary,
                     aes(x = transcript_year,
                         y = kcount,
                         group = keyword)) +
    geom_line(aes(color = keyword)) +
    ggtitle("Frequency of Climate Change Use - All Transcripts") +
    theme(plot.title = element_text(size=24, hjust = 0.5))

# ggsave("./LocalView/Results/Graphs/ClimateChange_ByYear.jpg", kwcc_graph)

### tidy text ##################################################################
##                                                                            ##
##                     convert data to tidy text format                       ##
##                                                                            ##
################################################################################
## convert to tidy text format
lvTidy <- tidy(corpus)

### subset documents that contain the phrase "climate change" (3,875 mentions) ###
lvCC <- filter(lvTidy, grepl("climate change", text, ignore.case = TRUE))

### text analysis ##############################################################
##                                                                            ##
##                      text analysis using tidy text                         ##
##                                                                            ##
################################################################################


# ### tokenize corpus, tidy ######################################################
# lvTidyTokens <- lvCC %>% unnest_tokens(word, text) %>%
#     anti_join(stop_words) %>%
#     filter(!(word %in% custom_stopwords) & !(grepl('[0-9]', word)))
