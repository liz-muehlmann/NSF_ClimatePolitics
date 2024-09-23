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
                      "good bye",
                      "fellas", 
                      "y'all",
                      "yeah")

##  ............................................................................
##  tokenize corpus                                                         ####

tokens <- corpus %>% 
    quanteda::tokens(remove_punct = FALSE,
                     remove_url = FALSE,
                     remove_symbols = FALSE,
                     remove_numbers = FALSE) %>% 
    tokens_remove(c(stopwords("en"), custom_stopwords))  %>% 
    tokens_tolower() 


##  ............................................................................
##  assign document variables                                               ####

docvars_df <- docvars(tokens)
docvars_df$docname <- paste("text", 1:nrow(docvars_df), sep="")

##  ............................................................................
##  define disaster words                                                   ####

disasters <- quanteda::dictionary(list(
    storms = c("hurricane",
            "cyclone",
            "typhoon",
            "tropical storm",
            "tropical depression"),
    wind = c("tornado",
            "wind",
            "high wind"),
    fire = c("wildfire",
             "fire"),
    flood = c("flooding",
            "flood",
            "flooded",
            "floods"),
    heat = c("extreme heat",
             "heat wave",
             "heatwave",
             "heat",
             "drought"),
    climate = c("global warming",
                "climate change")))




































