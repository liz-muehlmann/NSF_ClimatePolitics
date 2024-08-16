################################################################################ 
##                                                                            ##
## This file includes the custom functions used to do text analysis using     ##
##     the local view data                                                    ##
##                                                                            ##
## This file is the es the local view data for text analysis.                 ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##  
##                                                                            ##
##  Output:                                                                   ##
##      None                                                                  ##
################################################################################ 

#   ____________________________________________________________________________
#   load source file, data, and increase heap space                         ####

library(tidyverse)
library(tidytext)
library(quanteda)
library(strcode)                                   # easy code separators
options(strcode = list(insert_with_shiny = FALSE,  # set options
                       char_length = 80, 
                       hash_in_sep= TRUE))
options(java.parameters = "-Xmx8000m")

load("./LocalView/data/modified/lvClean_transcript.rdata")

#   ____________________________________________________________________________
#   create corpus                                                           ####

corpus <- corpus(lvClean_transcript, text_field = "caption_text_clean")

##  ............................................................................
##  define stop words                                                       ####

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

##  ............................................................................
##  tokenize corpus                                                         ####

tokens <- corpus %>% 
    quanteda::tokens(remove_punct = TRUE,
                     remove_url = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = TRUE) %>% 
    tokens_remove(c(stopwords("en"), custom_stopwords))  %>% 
    tokens_tolower() 


##  ............................................................................
##  assign document variables                                               ####

docvars_df <- docvars(tokens)
docvars_df$docname <- paste("text", 1:nrow(docvars_df), sep="")

##  ............................................................................
##  define disaster words                                                   ####

disasters <- quanteda::dictionary(list(storms = c(
    "hurricane",
    "cyclone",
    "typhoon",
    "tropical storm"),
    wind = c("tornado",
            "wind",
            "high wind"),
    fire = c("wildfire",
             "fire"),
    flood = c("flooding",
            "flood",
            "flooded"),
    heat = c("extreme heat",
             "heat wave",
             "heatwave",
             "heat",
             "drought")))




































