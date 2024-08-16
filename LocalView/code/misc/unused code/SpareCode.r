
# n-grams ######################################################################
# bigrams <- tokens_ngrams(tokens, n = 2)  %>% 
#         dfm()  %>% 
#         dfm_sort(decreasing = TRUE, margin = "both")

# trigrams <- tokens_ngrams(tokens, n = 3)  %>%
#         dfm()  %>% 
#         dfm_sort(decreasing = TRUE, margin = "both")

# create document-feature matrix & sort ########################################
# dfm <- dfmTokens(tokens)
# dfmSort <- dfm_sort(dfm, decreasing = TRUE, margin = "both")        

# term-frequency inverse term-frequency ########################################
# tf_idf <- dfm_tfidf(dfm)
mutate(edu_total = eduM_total + eduF_total,
       edu_percentCollege = 
           edu_nohs = (eduM_nohs + eduF_nohs)/edu_total,
       edu_hs = (eduM_hs + eduF_hs)/edu_total,
       edu_aa = (eduM_aa + eduF_aa)/edu_total,
       edu_ba = (eduM_ba + eduF_ba)/edu_total,
       edu_ma = (eduM_ma + eduF_ma)/edu_total,
       edu_grad = (eduM_prof + eduF_prof + eduM_phd + eduF_phd)/edu_total,
       avg_edu = (edu_nohs * 0 + edu_hs * 1 + edu_aa * 2 +
                      edu_ba * 3 + edu_ma * 4 + edu_grad * 5),
       avg_edu_factor = case_when(
           avg_edu < 1 ~ 0,
           avg_edu >= 1 & avg_edu < 2 ~ 1,
           avg_edu >= 2 & avg_edu < 3 ~ 2,
           avg_edu >= 3 & avg_edu < 4 ~ 3,
           avg_edu >= 4 & avg_edu < 5 ~ 4,
           TRUE ~ 5),
       
       
       
       n_places_wtscript_incounty = n_distinct(place_name),
       
       
       
       n_tscriptcc_binary = ifelse(str_count(caption_text_clean, "climate change") >0, 1, 0), # create binary variable for climate change mention (0 = no, 1 = mentioned at least once)
       sum_tscriptcc_binary = sum(n_tscriptcc_binary)) %>% 

## load libraries ##############################################################

library(quanteda)                  # create dictionaries
library(quanteda.textstats)        # frequencies
library(quanteda.textplots)        # plotting text
library(tidyverse)                 # data manipulation
library(ggplot2)                   # data visualization
library(openxlsx)                  # save summaries
library(stringr)                   # string manipulation
library(tidytext)                  # sentiment analysis
library(textstem)                  # lemmatization

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

# summarize corpus
summarizeCorpus <- function(c) {            
    summary(c, n = 10) 
}

# tokenize corpus
tokenizeCorpus <- function(c) {             
    c %>% 
        quanteda::tokens(remove_punct = TRUE,
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


## extra keyword options - pulled from atlas.ti
# recovery_management = tolower(list('relief',
#                            'recovery',
#                            'emergency management',
#                            'emergency',
#                            'mitigation'))
# 
# relocation = tolower(list('relocation',
#                        'buyout',
#                        'buy out',
#                        'temporary relocation',
#                        'permanent relocation',
#                        'private buyout',
#                        'private buy out',
#                        'public buyout',
#                        'public buy out'))
# 
# general_words <- tolower(list('adaptation',
#                            'insurance',
#                            'insurance rates',
#                            'gray infrastructure',
#                            'infrastructure',
#                            'blue infrastructure',
#                            'green infrastructure',
#                            'risk',
#                            'risk perception',
#                            'weather change',
#                            'preparedness',
#                            'prepare',
#                            'resilience',
#                            'resilient'))
# 
# nbo <- tolower(list('residential voluntary association',
#                  'property owner associations',
#                  'home owners association',
#                  'HOA',
#                  'HOA Board',
#                  'Property Board',
#                  'NGO',
#                  'CBO',
#                  'mutual aid'))
# 
# gov_words <- tolower(list('informal organizing',
#                        'organizing',
#                        'activism',
#                        'federal',
#                        'state',
#                        'state government',
#                        'county',
#                        'city',
#                        'special district',
#                        'incorporation',
#                        'incorporated',
#                        'unincorporated',
#                        'polycentric',
#                        'jurisdiction',
#                        'annexation',
#                        'deannexation',
#                        'funding',
#                        'tax credit',
#                        'subsidies',
#                        'grant',
#                        'grants',
#                        'zoning',
#                        'regulation',
#                        'regulations',
#                        'social services',
#                        'water',
#                        'sanitation',
#                        'parks',
#                        'trees',
#                        'streets',
#                        'public safety',
#                        'food',
#                        'shelter',
#                        'temporary shelter',
#                        'electric',
#                        'electric company',
#                        'power grid'))
# 
# housing <- tolower(list('property',
#                      'property values',
#                      'affordable housing',
#                      'public housing',
#                      'shelters',
#                      'trailer park',
#                      'trailer parks',
#                      'mobile homes',
#                      'mobile home',
#                      'short term rental',
#                      'short term rentals',
#                      'Air B&B',
#                      'air b and b',
#                      'airbandb',
#                      'airbnb',
#                      'air bnb',
#                      'unhoused',
#                      'homeless'))
# 
# participation <- tolower(list('city council',
#                            'county commission',
#                            'meeting',
#                            'community outreach',
#                            'political participation',
#                            'voting',
#                            'leadership',
#                            'activism',
#                            'protest',
#                            'turnout',
#                            'partisanship',
#                            'polarization'))
# 
# communication <- tolower(list('social media',
#                            'Facebook',
#                            'NextDoor',
#                            'Next Door',
#                            'Twitter',
#                            'TikTok',
#                            'Tik Tok',
#                            'BlueSky',
#                            'Blue Sky',
#                            'whatsapp',
#                            'whats app',
#                            'discord',
#                            'texting',
#                            'slack',
#                            'texted',
#                            'television',
#                            'TV',
#                            'newsletter',
#                            'newsletters',
#                            'news letters',
#                            'news'))
# 
# community <- tolower(list('social trust',
#                        'trust',
#                        'community',
#                        'social capital',
#                        'inequality',
#                        'inequalities',
#                        'social vulnerability',
#                        'vulnerable',
#                        'vulnerability',
#                        'demographic change',
#                        'demographics',
#                        'age',
#                        'education',
#                        'technology',
#                        'language',
#                        'class',
#                        'income',
#                        'poverty',
#                        'ethnicity',
#                        'ethnic',
#                        'race',
#                        'immigration',
#                        'documentation',
#                        'dependents',
#                        'care workers',
#                        'care worker',
#                        'LGTBQIA+',
#                        'LGTBQIA',
#                        'homeowner',
#                        'home owner',
#                        'renter',
#                        'disability',
#                        'housing tenure'))
# 
# economic <- tolower(list('economic development',
#                       'economy',
#                       'money',
#                       'space x',
#                       'nasa',
#                       'tourism'))

### load libraries
library(tidyverse)
library(tigris)
library(sf)
library(tidycensus)


### counties
counties00 <- counties(year = 2000) %>% 
    filter(STATEFP00 < 57 & STATEFP00 != "02") %>% 
    rename(full_fips = CNTYIDFP00,
           county_name = NAMELSAD00) %>% 
    select(full_fips, county_name)

counties20 <- counties(year = 2020) %>% 
    filter(STATEFP < 57 & STATEFP != "02") %>% 
    rename(full_fips = GEOID,
           county_name = NAMELSAD) %>% 
    select(full_fips, county_name)


B15002

M
002 Male
006 NO HS
010 HS 
014 AA
015 BA
016 MA
017 PROF
018 PHD




F
019 Female
023 NO HS
028 HS
031 AA
032 BA
033 MA
034 PROF
035 PHD


    