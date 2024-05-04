################################################################################
##                                                                            ##
## This file merges the local view data with ACS and election data for all    ##
##      years. It also subsets the data for mentions of climate change and    ##
##      saves to parquet format for use in Code/LocalView/LVTextAnalysis      ##  
##      Code/LocalView/LVTextAnalysis/TAClimateChange.r                       ##
##                                                                            ##    
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##    
##  Political data was downloaded from the Harvard Dataverse for Algara &     ##
##    Sharif (2021) "Replication Data for: Partisanship & Nationalization     ##
##    in American Elections: Evidence from Presidential, Senatorial, &        ##
##    Gubernatorial Elections in the U.S. Counties, 1872-2020",               ##
##      https://doi.org/10.7910/DVN/DGUMFI                                    ##
##                                                                            ##
##  ACS data was downloaded using the Tidy Census package                     ##
##      2006-2010 ACS > 2008                                                  ##
##      2011-2015 ACS > 2012 Election                                         ##
##      2016-2020 ACS > 2016 & 2020 Elections                                 ##
##                                                                            ##
## county names are downloaded using the Tigris package                       ##
##                                                                            ##
## Election + ACS + County data were merged in                                ##        
##      Code/Political/AlgaraACSPlaces.r                                      ##
##                                                                            ##
## ** Majority counties were determined using ArcGIS' Tabulate Intersection   ##
##    function and processed using Google Docs.                               ##  
##    Methodology is available upon request.                                  ##               
##                                                                            ##
################################################################################

# load preliminaries & packages ################################################
setwd("F:/GitHub/NSF_ClimatePolitics/")
source("./Code/LocalView/LVTextAnalysis/TAPrelim.r")

## load data ###################################################################
# load political data
algaraACS <- read.csv("./Data/Political/PoliticalModified/algaraACSlong.csv") %>% 
    mutate(full_fips = str_pad(full_fips, 5, "left", 0)) %>% 
    rename(stcounty_fips = full_fips)

# filter only videos with transcripts (n = 103,812)
# 9,434 observations in the local view data used county fips instead of place FIPS
data <- all_docs  %>% 
    filter(caption_text_clean != "<No caption available>" & state_name != "Alaska") %>% 
    rename(transcriptYear = year,
           place_fips = st_fips) %>% 
    mutate(place_fips = str_pad(place_fips, 7, "left", "0"),
           stcounty_fips = ifelse(grepl("County", place_name), str_sub(place_fips, 3, 7), NA),
           stcounty_fips = ifelse(place_fips == 5151650, str_sub(place_fips, 3, 7), stcounty_fips),
           stcounty_fips = ifelse(place_fips == 5151730, str_sub(place_fips, 3, 7), stcounty_fips),
           stcounty_fips = ifelse(place_fips == 5151710, str_sub(place_fips, 3, 7), stcounty_fips),
           place_fips = ifelse(is.na(stcounty_fips), place_fips, NA),
           match_column = ifelse(is.na(place_fips), stcounty_fips, place_fips)) %>% 
    select(-place_fips, -stcounty_fips)

# select only geography columns in local view data for use in Code/Cartography/MajorityCounties.R
# lvPlaceOnly <- data %>%
#     select(state_name, place_fips, place_name, stcounty_fips)
# write.csv(lvPlaceOnly, "./Data/LocalView/LVModifiedData/LVPlaceOnly.csv")

# load place data
places <- read.csv("./Data/Cartography/CartographyModified/2020_AllPlaces.csv") %>% 
    select(-X, -place_name) %>% 
    mutate(place_fips = as.character(place_fips),
           place_fips = str_pad(place_fips, 7, "left", 0),
           stcounty_fips = as.character(stcounty_fips),
           stcounty_fips = str_pad(stcounty_fips, 5, "left", 0),
           county_fips = str_pad(county_fips, 3, "left", 0),
           match_column = ifelse(is.na(place_fips), stcounty_fips, place_fips))

# merge local view data with place data
data_place <- left_join(data, places, by = "match_column") %>% 
    distinct() %>% 
    mutate(state_fips = str_pad(state_fips, 2, "left", 0),
           county_fips = str_pad(county_fips, 3, "left", 0))

# merge local view data with election data
lvElection <- left_join(data_place, algaraACS, by = "stcounty_fips")

## quanteda ####################################################################
# create corpus
lvCorpus <- createCorpus(data_place)

# tokenize the corpus
tokens <- tokenizeCorpus(lvCorpus)
docvars_df <- docvars(tokens)
docvars_df$docname <- paste("text", 1:nrow(docvars_df), sep="")

# find climate change in context
kwic <- kwic(tokens, pattern = phrase("climate change"))

# merge key words in context with document data
kwic_df <- merge(kwic, docvars_df, by = "docname")  %>%
    mutate(year = str_sub(meeting_date, start = 1, end = 4))  %>%
    as.data.frame()

# count frequency of climate change use over time
kwicCount <- kwic_df  %>% 
    group_by(year, keyword)  %>% 
    summarize(kcount = n())  

# plot frequency of climate change use over time
# kwXyear <- ggplot(data = kwicCount,
#                   aes(x = year,
#                       y = kcount,
#                       group = keyword)) +
#     geom_line(aes(color = keyword)) +
#     ggtitle("Climate Change Frequency by Year - All Transcripts") +
#     theme(plot.title = element_text(size=24, hjust = 0.5))

# subset documents that contain the phrase "climate change"
lvCC <- corpus_subset(lvCorpus, vid_id %in% kwic_df$vid_id)

## tidytext ##################################################################
# convert corpus to tidy text format 
lvTidy <- tidy(lvCorpus)

# subset documents that contain the phrase "climate change" (5,429 mentions)
lvCC <- filter(lvTidy, grepl("climate change", text, ignore.case = TRUE))

lvCCSummary <- lvCC %>% group_by(place_name, state_name) %>% summarize(c = n())

write.csv(lvCCSummary, "./Results/LocalView/Summaries/ClimateChangeMentions.csv")


lvTidyTokens <- lvCC %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    filter(!(word %in% custom_stopwords) &  !(grepl('[0-9]', word))) %>% # remove custom stop words and numbers
    lemmatize_words()

## lemmetize the text next. 
















