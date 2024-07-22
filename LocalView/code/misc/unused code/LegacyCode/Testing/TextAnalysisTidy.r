################################################################################
##                                                                            ##
##                      Be sure to change the year!                           ##
##                                                                            ##      
## This file follows tidytext principles to analyze the Local View data       ##  
##                                                                            ##  
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##     
################################################################################

## load library ################################################################
library(readtext)                  # read filepaths
library(tidyverse)                 # data manipulation
library(tidytext)                  # corpus manipulation
library(broom)                     # make outputs tidy 
library(quanteda)                  # create dictionaries
library(textclean)                 # contractions list
library(stopwords)                 # stop word lists
library(ggplot2)                   # data visualization
library(ggraph)                    # network plots
library(patchwork)                 # plot composition 
library(SnowballC)                 # stemming words 
library(SemNetCleaner)             # singularize words      

# set data directory ###########################################################
root <- "./Modified_Data/Political/Local View/2023_LocalView_Corpus/"

# load data ####################################################################
data <- readtext(paste0(root, "2008/*.txt"),
        docvarsfrom = "filenames",
        docvarnames = c("year", "state", "location", "meetingID"),
        dvsep = "_")

# load the corpus ##############################################################
corpus <- tibble(data)

# remove stop words ############################################################
data(stop_words)
custom_stopwords <- c("pause", 
                      "music,", 
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
                      "y'all")

# tokenization #################################################################
tokens <- corpus  %>% 
          unnest_tokens(word, text) %>% 
          anti_join(stop_words, by = c("word" = "word"))  %>% 
          filter(!(word %in% custom_stopwords))  %>% 
          filter(!(grepl('[0-9]', word))) %>% 
          rename(ogword = word)  %>%
          rowwise()  %>% 
          mutate(word = singularize(ogword)) 
 
# count word occurrence ########################################################
token_count <- tokens  %>% 
              count(doc_id, word, sort = TRUE)  

# count the most common words by document and in total #########################      
total_words <- token_count  %>% 
               group_by(doc_id)  %>% 
               summarize(total = sum(n))

word_count <- left_join(token_count, total_words, by = "doc_id")

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
             slice_max(tf_idf, n = 3) %>%
             ungroup() %>%
             ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = doc_id)) +
             geom_col(show.legend = FALSE) +
             facet_wrap(~doc_id, ncol = 5, scales = "free") +
             labs(x = "tf-idf", y = NULL)

# bigrams ######################################################################
bigrams <- corpus  %>% 
           unnest_tokens(bigram, text, token = "ngrams", n = 2) 

## separate the bigrams into word 1 and 2, remove stop words and numerals.
bigram_sep <- bigrams %>% 
              separate(bigram, c("word1", "word2"), sep = " ") %>% 
              filter(!(word1 %in% stopwords("en"))) %>%
              filter(!(word1 %in% custom_stopwords)) %>% 
              filter(!(grepl('[0-9]', word1))) %>% 
              filter(!(word2 %in% stopwords("en"))) %>%
              filter(!(word2 %in% custom_stopwords)) %>% 
              filter(!(grepl('[0-9]', word2)))

## count the bigrams
bigram_count <- bigram_sep  %>% 
                count(word1, word2, sort = TRUE)
bigram_united <- bigram_sep  %>% 
                 unite(bigram_sep, c(word1, word2), sep = " ")

# tigrams ######################################################################
trigrams <- corpus  %>% 
           unnest_tokens(trigram, text, token = "ngrams", n = 3) 

## separate the tigrams into word 1 and 2, remove stop words and numerals.
trigram_sep <- trigrams %>% 
              separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
              filter(!(word1 %in% stopwords("en"))) %>%
              filter(!(word1 %in% custom_stopwords)) %>% 
              filter(!(grepl('[0-9]', word1))) %>% 
              filter(!(word2 %in% stopwords("en"))) %>%
              filter(!(word2 %in% custom_stopwords)) %>% 
              filter(!(grepl('[0-9]', word2)))     %>% 
              filter(!(word3 %in% stopwords("en"))) %>%
              filter(!(word3 %in% custom_stopwords)) %>% 
              filter(!(grepl('[0-9]', word3)))

## count the tigrams
trigram_count <- trigram_sep  %>% 
                count(word1, word2, word3, sort = TRUE)

trigram_united <- trigram_sep  %>% 
                 unite(trigram_sep, c(word1, word2, word3), sep = " ")

# words of interest ############################################################
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

## search corpus for words in one list
s_woi <- tokens  %>% 
       filter(word %in% single_woi)  %>% 
       count(doc_id, word)

b_woi <- bigrams  %>% 
       filter(bigram %in% bigram_woi)  %>% 
       count(doc_id, bigram)  %>% 
       arrange(desc(n))

t_woi <- trigrams  %>% 
       filter(trigram %in% trigram_woi)  %>% 
       count(doc_id, trigram)  %>% 
       arrange(desc(n))

# merge woi counts with total_words ############################################
swoi_count <- left_join(s_woi, total_words, by = "doc_id")
bwoi_count <- left_join(b_woi, total_words, by = "doc_id")

# calculate the tf_idf for the words of interest by document ###################
soi_tfidf <- swoi_count  %>% 
          bind_tf_idf(word, doc_id, n)

soi_tfidf  %>% select(-total)  %>% 
            arrange(desc(tf_idf))

# bwoi_count  %>% 
#           bind_tf_idf(word, doc_id, n)
# 
# boi_tfidf  %>% select(-total)  %>% 
#             arrange(desc(tf_idf))
# 
# toi_tfidf <- twoi_count  %>% 
#           bind_tf_idf(word, doc_id, n)
# 
# toi_tfidf  %>% select(-total)  %>% 
#             arrange(desc(tf_idf))

# graph the tf_idf by document #################################################
soi_tfidfXdoc <- soi_tfidf %>%
             group_by(doc_id) %>%
             slice_max(tf_idf, n = 15) %>%
             ungroup() %>%
             ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = doc_id)) +
             geom_col(show.legend = FALSE) +
             facet_wrap(~doc_id, ncol = 2, scales = "free") +
             labs(x = "tf-idf", y = NULL) +
             plot_annotation(title = "tf_idf of unigrams for Local View 2007", 
                             caption = "tf_idf ranks the importance of words in each document. It decreases the weight of common words and increases the weight of the least common words.") &  
             theme(plot.title = element_text(size = 25, hjust = 0.5)) 

# ggsave("./Results/tf_idf/2007_tx_soitfidf.jpg", soi_tfidfXdoc)
