################################################################################
##                                                                            ##
## This file handles the preliminary steps necessary for text analysis        ##          
##      Code/LocalView/LVTextAnalysis/TAallDocs.r                             ##  
##                                                                            ##  
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##     
################################################################################

# set working directory ########################################################
setwd("F:/GitHub/NSF_ClimatePolitics/")

# suppress warnings & increase java heap space #################################
options(warn = -1)
options(java.parameters = "-Xmx8000m")

## load libraries ##############################################################
library(arrow)                     # open and work with parquet filepaths
library(readtext)                  # read filepaths
library(quanteda)                  # create dictionaries
library(quanteda.textstats)        # frequencies
library(quanteda.textplots)        # plotting text
library(tidyverse)                 # data manipulation
library(ggplot2)                   # data visualization
library(openxlsx)                  # save summaries
library(stringr)                   # string manipulation
library(tidytext)                  # sentiment analysis
library(textstem)                  # lemmatization

# load parquet data and convert it to data frame (N = 153,452/ n = 103,812) ####
all_docs <- open_dataset("/GitHub/NSF_ClimatePolitics/Data/LocalView/LVOriginalData/meetings/")  
all_docs <- Scanner$create(all_docs)
all_docs <- all_docs$ToTable()
all_docs <- as.data.frame(all_docs)  %>% 
        mutate(year = str_sub(meeting_date, start = 1, end = 4))
lv0609 <- all_docs  %>% 
        filter(year <= 2009)
all_docs <- all_docs  %>% 
        filter(year >= 2010) %>% 
        select(!(contains("acs")), -caption_text)
noCap <- all_docs  %>%                                                   #49,640
        filter(caption_text_clean == "<No caption available>")


# ## find the proportion of missing captions by year #############################
# prop__noCap <- all_docs  %>% 
#         select(st_fips, state_name, place_name, year, caption_text)  %>% 
#         mutate(iscaptioned = 
#                case_when(caption_text == "<No caption available>" ~ "0",
#                          caption_text != "<No caption available>" ~ "1"))  %>% 
#         group_by(year)  %>% 
#         mutate(total_year = n())  %>% 
#         ungroup()  %>% 
#         select(st_fips, state_name, place_name, year, iscaptioned, total_year)  %>% 
#         group_by(year, iscaptioned)  %>% 
#         mutate(count = n())  %>% 
#         ungroup()  %>% 
#         mutate(prop = count/total_year)  %>% 
#         distinct(year, iscaptioned, .keep_all = TRUE)
# 
# # write.csv(prop_noCap, "./LocalView/Results/Summaries/NoCapProportion.csv")

# define stop words ############################################################
custom_stopwords <- c("pause", 
                      "music", 
                      "um", 
                      "uh", 
                      "okay", 
                      "really", 
                      "hi", 
                      "hello", 
                      "goodbye",
                      "bye",
                      "thanks", 
                      "thank you", 
                      "oh", 
                      "please", 
                      "mr", 
                      "mrs", 
                      "dr", 
                      "sir", 
                      "just", 
                      "thank", 
                      "like", 
                      "alright", 
                      "welcome", 
                      "good",
                      "fellas", 
                      "y'all",
                      "yeah")

# disaster words of interest ###################################################
hazard_disaster <- dictionary(list(hazard = c(
                            "climate change",
                            "extreme weather",
                            "storm*",
                            "sea level ris*",
                            "erod*",
                            "eros*",
                            "flood*",
                            "wind*",
                            "wildfire",
                            "tornado"),
                        disaster = c("hurricane*",
                            "hurricane ike",
                            "hurricane harvey",
                            "hurricane dolly",
                            "hurricane laura",
                            "hurricane rita",
                            "hurricane irma",
                            "hurricane imelda",
                            "hurricane ian")))

# define functions #############################################################
# create corpus
createCorpus <- function(c) {
        corpus(c, text_field = "caption_text_clean")
}

# summarize corpus
summarizeCorpus <- function(c) {            
        summary(c, n = 10) 
}

# tokenize corpus
tokenizeCorpus <- function(c) {             
        c %>% 
        tokens(remove_punct = TRUE,
        remove_url = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE) %>% 
        tokens_remove(c(stopwords("en"), custom_stopwords))  %>% 
        tokens_tolower() 
}

# create document-feature matrix
dfmTokens <- function(t) {
        dfm(t) 
}

# create keyword in context
kwicTokens <- function(t) {
        kwic(t, pattern = hazard_disaster)              # change hazard_disaster to search for specific WOIs
}

