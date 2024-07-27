library(tidyverse)
library(openxlsx)                                  # save tables
library(flextable)                                 # make tables
library(strcode)                                   # easy code separators
options(strcode = list(insert_with_shiny = FALSE,  # set options
                       char_length = 100, 
                       hash_in_sep= TRUE))

load("./LocalView/data/modified/lv_clean_noTranscript.rdata")
load("./LocalView/data/modified/lv_countyYear_noNA.rdata")
load("./LocalView/data/modified/allData_state.rdata")

lv <- lvClean_noScript %>% 
    select(transcript_year, state_name, county_name, n_ccMentions, ccBinary) %>% 
    group_by(transcript_year, county_name) %>% 
    summarize(n_script = n(),
              n_script_ccMention = sum(ccBinary),
              total_ccMention = sum(n_ccMentions)) %>% 
    pivot_wider(names_from = transcript_year, 
                values_from = c(n_script, n_script_ccMention, total_ccMention))
### flextable