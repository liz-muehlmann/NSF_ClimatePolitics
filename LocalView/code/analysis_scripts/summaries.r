################################################################################
##                                                                            ##
## This file compiles summary statistics                                      ##
##                                                                            ##    
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##              
##          Algara-Sharif (2008-2020)                                         ##
##              Sharif (2021) "Replication Data for: Partisanship & Nationalization
##              in American Elections: Evidence from Presidential, Senatorial, &   
##              Gubernatorial Elections in the U.S. Counties, 1872-2020",     ##      
##              https://doi.org/10.7910/DVN/DGUMFI                            ##      
##          American Community Survey (2010, 2015, 2020)                      ##
##              2006-2010 ACS > 2008                                          ##
##              2011-2015 ACS > 2012 Election                                 ##
##              2016-2020 ACS > 2016 & 2020 Elections                         ##
##          FEMA (2010-2023)                                                  ## 
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
##          Climate Change Vulnerability (2010-2023)                          ## 
##              https://github.com/wachiuphd/CVI
##          USDA Rural-Urban                                                  ## 
##          Incorporated/Census Designated Places (2023)                      ## 
##              IP/CDP csvs were saved using the tabulate intersection function in ArcGIS 
##          Virginia                                                          ##
##              https://www.census.gov/library/reference/code-lists/ansi.2020.html#cousub
##          Counties (2010, 2015, 2020)                                       ##
##              tigris                                                        ##
##          States                                                            ##
##              tigris                                                        ##   
##                                                                            ##
################################################################################ 

### libraries ##################################################################
##                                                                            ##
##                       load libraries and data                              ##
##                                                                            ##
################################################################################
library(tidyverse)
library(openxlsx)
library(strcode)                                   # easy code separators
options(strcode = list(insert_with_shiny = FALSE,  # set options
                       char_length = 100, 
                       hash_in_sep= TRUE))

load("./LocalView/data/modified/lv_clean_noTranscript.rdata")
load("./LocalView/data/modified/lv_countyYear_noNA.rdata")
load("./LocalView/data/modified/allData_state.rdata")

### original summaries #########################################################
##                                                                            ##
##                        original data set summaries                         ##
##      these summaries are on the original local view data n=103,372         ##
##      this data set is at the place & meeting year level and does not       ##
##                        include election or ACS data.                       ##
##                                                                            ##
################################################################################
## transcripts available by year and state
yearState <- lvClean_noScript %>% 
    group_by(transcript_year, state_name) %>% 
    count()

# write.csv(yearState, "./LocalView/results/summaries/YearState.csv", row.names = FALSE)

yearStateCounty <- allData_state %>% 
    select(state_name, transcript_year, n_countiesInState, state_n_transcripts, state_n_scriptCCMention)

# write.csv(yearStateCounty, "./LocalView/results/summaries/YearStateCounty.csv", row.names = FALSE)

## transcripts available by year, state, and meeting_type
stateType <- lvClean_noScript %>%
    group_by(transcript_year, state_name, meeting_type) %>% 
    count()

# write.csv(stateType, "./LocalView/results/summaries/YearStateMeetingType.csv", row.names = FALSE)

## transcripts available by year, state, and place name
statePlace <- lvClean_noScript %>%
    group_by(transcript_year, state_name, place_name) %>% 
    count()

# write.csv(statePlace, "./LocalView/results/summaries/YearStatePlace.csv", row.names = FALSE)

stateSummary <- lvClean_noScript %>%
    group_by(state_name) %>% 
    count()

## count and proportion of climate change use by year
prop_CCuse <- lvClean_noScript %>% 
    select(transcript_year, stcounty_fips, n_ccMentions, ccBinary) %>% 
    group_by(transcript_year) %>% 
    mutate(year_nCounties = n_distinct(stcounty_fips),                                       # total distinct counties in the LV data by year
           year_n_transcripts = n(),                                                         # total number of transcripts in a year
           year_nScriptCC = sum(ccBinary),                                                   # number of transcripts in a year with at least one mention of climate change
           year_nCCmentions = sum(n_ccMentions),                                             # total number of climate change mentions in a year
           year_propScriptCC = year_nScriptCC/year_n_transcripts,                            # proportion of transcripts with at least one climate change mention
           year_propCCmentions = year_nCCmentions/year_n_transcripts,                        # proportion of all metions of climate change in a given year
           year_ccBinary = ifelse(ccBinary > 0, 1, 0)) %>%                                   # binary indicator of whether there's at least one mention of climate change in that year
    filter(year_ccBinary == 1) %>%  
    mutate(n_countiesCCmention = n_distinct(stcounty_fips),                                  # number of unique counties that mention climate change in a year 
           prop_countiesCCMention = n_countiesCCmention/year_nCounties) %>%                  # proportion of counties in the data that mention climate change at least once
    select(transcript_year, n_ccMentions, prop_countiesCCMention, starts_with("year_")) %>% 
    distinct(transcript_year, .keep_all = TRUE)


# write.csv(prop_CCuse, "./LocalView/results/summaries/prop_ccUse.csv", row.names = FALSE)

### state summaries ############################################################
##                                                                            ##
##                           full data set summaries                          ##
##      these summaries are on the modified local view data n=43,592          ##
##                                                                            ##
################################################################################

## number of counties in each state-year with a transcript
data_noNA <- lv_countyYear_noNA %>% 
    group_by(stcounty_fips) %>% 
    mutate(state_nScriptAY = n()) %>% 
    ungroup() 

## balanced panel check
table(data_noNA$state_nScriptAY)

### prop tables by county ######################################################
##                                                                            ##
##                           proportion tables by county                      ##
##                                                                            ##
################################################################################

state_tables <- createWorkbook()
prop_tables <- createWorkbook()

for (state in unique(lvClean_noScript$state_name)) {
    
    df <- lvClean_noScript %>% 
        filter(state_name == state)
    
    d_state <- df %>% 
        group_by(transcript_year, county_name) %>% 
        summarize(n_script = n(),
                  n_script_ccMention = sum(ccBinary),
                  total_ccMention = sum(n_ccMentions)) %>% 
        ungroup() %>% 
        mutate(n_distinct_counties = n_distinct(county_name)) %>% 
        group_by(transcript_year) %>% 
        mutate(n_counties_inYear = n_distinct(county_name),
               nScript_nCC = paste(n_script, "(", n_script_ccMention, ")", sep = " "))
    
    table <- d_state %>% 
        select(county_name, transcript_year, n_script, n_distinct_counties, n_counties_inYear)
    
    addWorksheet(state_tables, sheetName = state)
    
    writeData(state_tables, sheet = state, x = table)
    
    prop_table <- d_state %>% 
        select(transcript_year, county_name, nScript_nCC) %>% 
        distinct(county_name, .keep_all = TRUE ) %>% 
        pivot_wider(names_from = transcript_year, values_from = nScript_nCC)
    
    addWorksheet(prop_tables, sheetName = state)
    
    writeData(prop_tables, sheet = state, x = prop_table)
    
}

save_loc <- "./LocalView/results/summaries/"

saveWorkbook(state_tables, file = paste(save_loc, "state_tables.xlsx", sep = ""), overwrite = TRUE)
saveWorkbook(prop_tables, file = paste(save_loc, "prop_tables.xlsx", sep = ""), overwrite = TRUE)
# 
# ca <- lvClean_noScript %>% 
#     filter(state_name == "California") %>% 
# group_by(transcript_year, county_name) %>% 
#     summarize(n_script = n(),
#               n_script_ccMention = sum(ccBinary),
#               total_ccMention = sum(n_ccMentions)) %>% 
#     ungroup() %>% 
#     mutate(n_distinct_counties = n_distinct(county_name)) %>% 
#     group_by(transcript_year) %>% 
#     mutate(n_counties_inYear = n_distinct(county_name),
#            nScript_nCC = paste(n_script, "(", n_script_ccMention, ")", sep = " "))
# 
# prop_table <- ca %>% 
#     select(transcript_year, county_name, nScript_nCC) %>% 
#     distinct(county_name, .keep_all = TRUE ) %>% 
#     pivot_wider(names_from = transcript_year, values_from = nScript_nCC)



####################################
## model summary to put the number of unique counties at the top row?


#########################################
ca <- lvClean_noScript %>% 
    filter(state_name == "California") %>% 
    group_by(transcript_year, county_name) %>% 
    summarize(n_script = n(),
              n_script_ccMention = sum(ccBinary),
              total_ccMention = sum(n_ccMentions)) %>% 
    ungroup() %>% 
    mutate(n_distinct_counties = n_distinct(county_name), 
           n_script_ccMention = paste("(", n_script_ccMention, ")", sep = "")) %>% 
    group_by(transcript_year) %>% 
    mutate(n_counties_inYear = n_distinct(county_name),
           nScript_nCC = paste(n_script, n_script_ccMention, sep = " "))

library(gt)
prop_table <- ca %>% 
    select(transcript_year, county_name, nScript_nCC) %>% 
    distinct(county_name, .keep_all = TRUE ) %>% 
    pivot_wider(names_from = transcript_year, values_from = nScript_nCC) %>% 
    gt()
    

gt_propTable <- prop_table %>% 
    tab_header(
        title = md("**Local View**"),
        subtitle = "Transcripts & Climate Change Mentions") %>% 
    tab_source_note(
        source_note = md("Number of transcripts in county-year (Number of transcripts with at least one climate change mention) in parentheses.")) %>% 
    tab_stubhead(label = "County Name")































