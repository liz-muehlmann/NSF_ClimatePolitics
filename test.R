library(tidyverse)
library(openxlsx)                                  # save tables
library(pivottabler)                                 # make tables
library(strcode)                                   # easy code separators
options(strcode = list(insert_with_shiny = FALSE,  # set options
                       char_length = 100, 
                       hash_in_sep= TRUE))

load("./LocalView/data/modified/lv_clean_noTranscript.rdata")
load("./LocalView/data/modified/lv_countyYear_noNA.rdata")
load("./LocalView/data/modified/allData_state.rdata")

ca <- lvClean_noScript %>% 
    select(transcript_year, state_name, county_name, n_ccMentions, ccBinary) %>% 
    filter(state_name == "California")  

# %>% 
#     summarize(n_script = n(),
#               n_script_ccMention = sum(ccBinary),
#               total_ccMention = sum(n_ccMentions)) 


