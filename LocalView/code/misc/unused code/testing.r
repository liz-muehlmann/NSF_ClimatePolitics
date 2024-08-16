
# 
# ## loop through all files in the corpus and create tidy text formats
# for (file in dir) {
#   a <- scan(file)
#   a <- tibble(text = a)  %>% 
#        unnest_tokens(word, text)  %>% 
#        anti_join(stop_words)
#   write.table(text = a,
#              con = "./Modified_Data/Political/Local View/LocalView_Tidy" + file)
# }

# #Create Term Document Matrix and create Matrix
# tdm <- TermDocumentMatrix(BigList['Data:AAPL'])
# m <- as.matrix(tdm)
#     
# ## convert data to tidytext format (one word per row)
# c_df <- tibble(text=c)  %>% 
#     unnest_tokens(word,text)
# 
# ## remove stop words
# data(stop_words)
# c_df <- c_df  %>% 
#     anti_join(stop_words)
# 
# ## count the most common words
# c_df  %>% 
#     count(word, sort=TRUE)  %>% 
#     filter(n > 20)  %>% 
#     mutate(word =)

## load corpus
# tbl <- list.files('./Modified_Data/Political/Local View/2023_LocalView_Corpus/2006/', full.names = TRUE, pattern = '*.txt')         
# data <- readtext(tbl)
# cor <- Corpus(VectorSource(data$text))
# dtm <- DocumentTermMatrix(cor)

## load parquet file
# par <- read_parquet("./Original_Data/Political/2023_LocalViewData/meetings.2018.parquet")

## load individual text file
# ind <- scan('./Modified_Data/Political/Local View/2023_LocalView_Corpus/2007-03-27_Texas_League City city_TlQHbwfVTfk.txt', what = "word", sep=" ", quote = "")
# ind_t <- tibble(text = ind)  %>% 
#          unnest_tokens(word, text)  %>% 
#          anti_join(stop_words)

hazards = tolower(list('climate change', 
                    'drainage', 
                    'storm water', 
                    'stormwater', 
                    'insufficient services', 
                    'sea level rise',
                    'erosion',
                    'flooding',
                    'wind',
                    'wildfire',
                    'fire',
                    'tornado',
                    'relief',
                    'rebuild',
                    'relocation',
                    'hurricane',
                    'hurricane Ike',
                    'hurricane Harvey',
                    'hurricane Dolly',
                    'hurricane Laura',
                    'hurricane Rita',
                    'hurricane Andrew',
                    'hurricane Irma',
                    'hurricane Imelda',
                    'hurricane Ian',
                    'hurricane Katrina'))

recovery_management = tolower(list('relief',
                           'recovery',
                           'emergency management',
                           'emergency',
                           'mitigation'))

relocation = tolower(list('relocation',
                       'buyout',
                       'buy out',
                       'temporary relocation',
                       'permanent relocation',
                       'private buyout',
                       'private buy out',
                       'public buyout',
                       'public buy out'))

general_words <- tolower(list('adaptation',
                           'insurance',
                           'insurance rates',
                           'gray infrastructure',
                           'infrastructure',
                           'blue infrastructure',
                           'green infrastructure',
                           'risk',
                           'risk perception',
                           'weather change',
                           'preparedness',
                           'prepare',
                           'resilience',
                           'resilient'))

nbo <- tolower(list('residential voluntary association',
                 'property owner associations',
                 'home owners association',
                 'HOA',
                 'HOA Board',
                 'Property Board',
                 'NGO',
                 'CBO',
                 'mutual aid'))

gov_words <- tolower(list('informal organizing',
                       'organizing',
                       'activism',
                       'federal',
                       'state',
                       'state government',
                       'county',
                       'city',
                       'special district',
                       'incorporation',
                       'incorporated',
                       'unincorporated',
                       'polycentric',
                       'jurisdiction',
                       'annexation',
                       'deannexation',
                       'funding',
                       'tax credit',
                       'subsidies',
                       'grant',
                       'grants',
                       'zoning',
                       'regulation',
                       'regulations',
                       'social services',
                       'water',
                       'sanitation',
                       'parks',
                       'trees',
                       'streets',
                       'public safety',
                       'food',
                       'shelter',
                       'temporary shelter',
                       'electric',
                       'electric company',
                       'power grid'))

housing <- tolower(list('property',
                     'property values',
                     'affordable housing',
                     'public housing',
                     'shelters',
                     'trailer park',
                     'trailer parks',
                     'mobile homes',
                     'mobile home',
                     'short term rental',
                     'short term rentals',
                     'Air B&B',
                     'air b and b',
                     'airbandb',
                     'airbnb',
                     'air bnb',
                     'unhoused',
                     'homeless'))

participation <- tolower(list('city council',
                           'county commission',
                           'meeting',
                           'community outreach',
                           'political participation',
                           'voting',
                           'leadership',
                           'activism',
                           'protest',
                           'turnout',
                           'partisanship',
                           'polarization'))

communication <- tolower(list('social media',
                           'Facebook',
                           'NextDoor',
                           'Next Door',
                           'Twitter',
                           'TikTok',
                           'Tik Tok',
                           'BlueSky',
                           'Blue Sky',
                           'whatsapp',
                           'whats app',
                           'discord',
                           'texting',
                           'slack',
                           'texted',
                           'television',
                           'TV',
                           'newsletter',
                           'newsletters',
                           'news letters',
                           'news'))

community <- tolower(list('social trust',
                       'trust',
                       'community',
                       'social capital',
                       'inequality',
                       'inequalities',
                       'social vulnerability',
                       'vulnerable',
                       'vulnerability',
                       'demographic change',
                       'demographics',
                       'age',
                       'education',
                       'technology',
                       'language',
                       'class',
                       'income',
                       'poverty',
                       'ethnicity',
                       'ethnic',
                       'race',
                       'immigration',
                       'documentation',
                       'dependents',
                       'care workers',
                       'care worker',
                       'LGTBQIA+',
                       'LGTBQIA',
                       'homeowner',
                       'home owner',
                       'renter',
                       'disability',
                       'housing tenure'))

economic <- tolower(list('economic development',
                      'economy',
                      'money',
                      'space x',
                      'nasa',
                      'tourism'))

# word frequency graphs ########################################################
wordsXdoc <- ggplot(word_count, aes(n/total, fill = doc_id)) +
                   geom_histogram(show.legend = FALSE) +
                   xlim(NA, 0.0009) +
                   facet_wrap(~doc_id, ncol = 2, scales = "free_y")

# zipf's law = the frequency that a word appears is inversely proportional to its rank
freq_by_rank <- word_count  %>% 
             group_by(doc_id)  %>% 
             mutate(rank = row_number(),
             term_frequency = n/total)  %>% 
             ungroup() 

freqXrank <-  freq_by_rank  %>% 
              ggplot(aes(rank, term_frequency, color = doc_id)) + 
              geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
              scale_x_log10() +
              scale_y_log10()

# tf-idf #######################################################################
tf_idf <- word_count  %>% 
          bind_tf_idf(word, doc_id, n)

tf_idf  %>% select(-total)  %>% 
            arrange(desc(tf_idf))

tfidfXdoc <- tf_idf %>%
             group_by(doc_id) %>%
             slice_max(tf_idf, n = 15) %>%
             ungroup() %>%
             ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = doc_id)) +
             geom_col(show.legend = FALSE) +
             facet_wrap(~doc_id, ncol = 2, scales = "free") +
             labs(x = "tf-idf", y = NULL)

single_woi <- tolower(c('drainage',
                     'erosion',
                     'flooding',
                     'wind',
                     'wildfire',
                     'fire',
                     'tornado',
                     'relief',
                     'rebuild',
                     'relocation',
                     'hurricane',
                     'relief',
                     'recovery',
                     'emergency',
                     'mitigation',
                     'relocation',
                     'buyout',
                     'adaptation',
                     'insurance',
                     'risk',
                     'prepare',
                     'preparedness',
                     'resilience',
                     'resilient',
                     'HOA',
                     'NGO',
                     'CBO',
                     'organizing',
                     'activism',
                     'federal',
                     'state',
                     'county',
                     'city',
                     'incorporation',
                     'incorporated',
                     'unincorporated',
                     'polycentric',
                     'jurisdiction',
                     'annexation',
                     'deannexation',
                     'funding',
                     'subsidies',
                     'grant',
                     'grants',
                     'zoning',
                     'regulation',
                     'regulations',
                     'water',
                     'sanitation',
                     'parks',
                     'trees',
                     'streets',
                     'food',
                     'shelter',
                     'electric',
                     'property',
                     'shelters',
                     'airbnb',
                     'unhoused',
                     'homeless',
                     'meeting',
                     'voting',
                     'leadership',
                     'activism',
                     'protest',
                     'turnout',
                     'partisanship',
                     'polarization',
                     'Facebook',
                     'NextDoor',
                     'Twitter',
                     'TikTok',
                     'BlueSky',
                     'whatsapp',
                     'discord',
                     'texting',
                     'slack',
                     'texted',
                     'television',
                     'TV',
                     'newsletter',
                     'newsletters',
                     'news',
                     'trust',
                     'community',
                     'inequality',
                     'inequalities',
                     'vulnerable',
                     'vulnerability',
                     'demographics',
                     'education',
                     'technology | tech',
                     'language',
                     'class',
                     'income',
                     'poverty',
                     'ethnicity',
                     'ethnic',
                     'race',
                     'immigration',
                     'documentation',
                     'dependents | dependent',
                     'LGTBQIA+',
                     'LGTBQIA',
                     'gay',
                     'lesbian',
                     'LGBT',
                     'homeowner',
                     'renter',
                     'disability',
                     'disabled',
                     'economy',
                     'money',
                     'nasa',
                     'tourism',
                     'stormwater',
                     'infrastructure',
                     'resaca',
                     'tidal',
                     'surge',
                     'displacement',
                     'replenishment',
                     'resacas',
                     'dunes',
                     'levees',
                     'dikes',
                     'mangroves',
                     'reefs',
                     'airbandb'))  

bigram_woi <- wordStem(tolower(c('climate change', 
                           'storm water', 
                           'insufficient services',
                           'hurricane Ike',
                           'hurricane Harvey',
                           'hurricane Dolly',
                           'hurricane Laura',
                           'hurricane Rita',
                           'hurricane Andrew',
                           'hurricane Irma',
                           'hurricane Imelda',
                           'hurricane Ian',
                           'hurricane Katrina',
                           'emergency management',
                           'buy out',
                           'temporary relocation',
                           'permanent relocation',
                           'private buyout',
                           'public buyout',
                           'insurance rates',
                           'gray infrastructure',
                           'blue infrastructure',
                           'green infrastructure',
                           'salt marsh',
                           'beach nourishment',
                           'risk perception',
                           'weather change',
                           'HOA Board',
                           'Property Board',
                           'mutual aid',
                           'informal organizing',
                           'state government',
                           'special district',
                           'tax credit',
                           'social services',
                           'public safety',
                           'temporary shelter',
                           'electric company',
                           'power grid',
                           'property values',
                           'affordable housing',
                           'public housing',
                           'trailer park',
                           'trailer parks',
                           'mobile homes',
                           'mobile home',
                           'air bnb',
                           'city council',
                           'county commission',
                           'community outreach',
                           'political participation',
                           'social media',
                           'Tik Tok',  
                           'Blue Sky',
                           'whats app',
                           'news letters',
                           'social trust',
                           'social capital',
                           'social vulnerability',
                           'demographic change',
                           'care workers',
                           'care worker',
                           'home owner',
                           'housing tenure',
                           'economic development',
                           'space x',
                           'Air B&B')))

trigram_woi <- wordStem(tolower(c('sea level rise',
                            'private buy out',
                            'public buy out',
                            'residential voluntary association',
                            'property owner associations',
                            'home owners association',
                            'short term rental',
                            'short term rentals')))


test_words <- tolower(c('meet', 'prepar'))
qtok <- text  %>% 
        quanteda::tokens()  %>% 
        dfm(tolower = TRUE)
dictionary <- data.frame(search=c("meet*"))
result <- tokens %>%
          filter(grepl(paste(dictionary$search, collapse="|"), word))

dict <- dictionary(list(meet = c("meet*")))
dfm <- dfm_lookup(qtok, dict, nomatch = "unmatched")


test_full <-  tolower(c('meet', 'meeting', 'prepared', 'preparedness'))
test <- tokens  %>% 
       filter(grepl(word, test_words))  %>% 
       count(doc_id, word)

       
result <- tokens %>%
          filter(grepl(paste(single_woi$search, collapse="|"), word))  %>% 
          count(doc_id, word)


text <- c("our house is in the middle of a street", " the drainage system is broken", "there was extreme flooding on tuesday", "our dunes are eroding my house has mice", "hurricane laura hit galveston very bad I have to be at the terminal at four california sees unprecedented wildfires")  %>% 
       quanteda::tokens() 

dict <- dictionary(list(
        hazards = c("drain*",
                    "tornado",
                    "flood*",
                    "*fire",
                    "erosion",
                    "erod*",
                    "wind*",
                    "hurricane laura")   
))

dfm <- kwic(text, dict)




# tf_idf: term-frequency inverse term frequency ################################
# ritaTFIDF <- dfm_tfidf(ritaDFM)  %>% 
#              dfm_trim(min_term=10, min_doc =2)
# ike_dollyTFIDF <- dfm_tfidf(ike_dollyDFM)
# harvey_irmaTFIDF <- dfm_tfidf(harvey_irmaDFM)
# imeldaTFIDF <- dfm_tfidf(imeldaDFM)
# lauraTFIDF <- dfm_tfidf(lauraDFM)
# ianTFIDF <- dfm_tfidf(ianDFM)

# create keyword in context table for disaster keywords for hurricanes, Texas ##
ritaDisasterWOI <- kwic(ritaTOK, pattern = hazard_disaster)  %>% 
        as.data.frame()  

ike_dollyDisasterWOI <- kwic(ike_dollyTOK, pattern = hazard_disaster)  %>% 
        as.data.frame()  
        
harvey_irmaDisasterWOI <- kwic(harvey_irmaTOK, pattern = hazard_disaster)  %>% 
        as.data.frame() 
        
imeldaDisasterWOI <- kwic(imeldaTOK, pattern = hazard_disaster)   %>% 
        as.data.frame() 
        
lauraDisasterWOI <- kwic(lauraTOK, pattern = hazard_disaster)   %>% 
        as.data.frame() 
        
ianDisasterWOI <- kwic(ianTOK, pattern = hazard_disaster)   %>% 
        as.data.frame()    


ritaDisasterWOI <- ritaDisasterWOI  %>%  mutate(year = substring(docname, 1,4))
ike_dollyDisasterWOI <- ike_dollyDisasterWOI %>% mutate(year = substring(docname, 1,4))
harvey_irmaDisasterWOI <- harvey_irmaDisasterWOI  %>% mutate(year = substring(docname, 1,4))        
imeldaDisasterWOI <- imeldaDisasterWOI  %>% mutate(year = substring(docname, 1,4))
lauraDisasterWOI <- lauraDisasterWOI  %>% mutate(year = substring(docname, 1,4)) 
ianDisasterWOI <- ianDisasterWOI %>% mutate(year = substring(docname, 1,4))


# save kwics  ##################################################################
# write.xlsx(ritaDisasterWOI, file="./Results/TX_DisasterWOISummaries.xlsx", sheetName="rita", row.names=FALSE)
# write.xlsx(ike_dollyDisasterWOI, file="./Results/TX_DisasterWOISummaries.xlsx", sheetName="ike_dolly", append=TRUE, row.names=FALSE)
# write.xlsx(harvey_irmaDisasterWOI, file="./Results/TX_DisasterWOISummaries.xlsx", sheetName="harvey_irma", append=TRUE, row.names=FALSE)
# write.xlsx(imeldaDisasterWOI, file="./Results/TX_DisasterWOISummaries.xlsx", sheetName="imelda", append=TRUE, row.names=FALSE)
# write.xlsx(lauraDisasterWOI, file="./Results/TX_DisasterWOISummaries.xlsx", sheetName="laura", append=TRUE, row.names=FALSE)
# write.xlsx(ianDisasterWOI, file="./Results/TX_DisasterWOISummaries.xlsx", sheetName="ian", append=TRUE, row.names=FALSE)

 
toks_news <- tokens(corp_news, remove_punct = TRUE) 
dfmat_news <- dfm(toks_news)
 
tstat_key <- textstat_keyness(dfmat_news, 
                              target = year(dfmat_news$date) >= 2016)
textplot_keyness(tstat_key)

# total documents = 103,783 
# data <- readtext(paste0(root, "*/*.txt"),                                       
#         docvarsfrom = "filenames",
#         docvarnames = c("date", "state", "location", "meetingID"),
#         dvsep = "_")  %>% 
#         mutate(year = str_sub(date, start = 1, end = 4))  %>% 
#         filter(state == "Texas")



# # merge 2011-2019 Local View data with place boundaries ################################################################
# for(s in unique(places2010$states)){
#         place <- places(state = s, year = 2011)  %>% 
#                  select(STATEFP, PLACEFP, GEOID, NAME, NAMELSAD, INTPTLAT, INTPTLON, geometry)
#         docs <- filteredDocs  %>% 
#                  filter(STUSPS == s,
#                         year >= 2011 & year <= 2019)
#         docsPlace <- left_join(docs,place) 
#         ###### uncomment to save to variable
#         # assign(paste0("LVPlace_", s), docsPlace)
#         ###### uncomment to save to gpkg
#         # st_write(docsPlace, 
#         #         paste0("./LocalView/ModifiedData/LVCartography/LVPlaceState/2011-2019/",
#         #         paste(s),"_LVPlace.gpkg"))
# }

# merge 2020-2023 Local View data with place boundaries ################################################################
for(s in unique(states$STUSPS)){
        place <- places(state = s, year = 2020)  %>% 
                 select(STATEFP, PLACEFP, GEOID, NAME, NAMELSAD, INTPTLAT, INTPTLON, geometry)
        docs <- filteredDocs  %>% 
                 filter(STUSPS == s,
                        year >= 2020 & year <= 2024)
        docsPlace <- left_join(docs,place) 
        ###### uncomment to save to variable
        # assign(paste0("LVPlace_", s), docsPlace)
        ###### uncomment to save to gpkg
        # st_write(docsPlace, 
        #         paste0("./LocalView/ModifiedData/LVCartography/LVPlaceState/2020-2023/",
        #         paste(s),"_LVPlace.gpkg"))
}

