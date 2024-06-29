################################################################################
##                                                                            ##
## This file uses the quanteda package to analyze the Local View data         ##
##      for Texas (all years)                                                 ##  
##                                                                            ##  
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##     
################################################################################

# load preliminaries file ######################################################
source("./Code/LocalView/TextAnalysis/Prelims.r")

# subset texas (n = 4,341) ##################################################### 
noCap <- all_docs  %>% 
         filter(caption_text_clean == "<No caption available>")  %>% 
         filter(state_name == "Texas")

data <- all_docs  %>% 
        filter(caption_text_clean != "<No caption available>")  %>% 
        mutate(year = str_sub(meeting_date, start = 1, end = 4))  %>% 
        filter(state_name == "Texas")

# create corpus ################################################################
corpus <- createCorpus(data)

# summarize full corpus ########################################################
summary <- summarizeCorpus(corpus)

# tokenize the corpus ##########################################################
tokens <- tokenizeCorpus(corpus)

# create document-feature matrix & sort ########################################
dfm <- dfmTokens(tokens)
dfmSort <- dfm_sort(dfm, decreasing = TRUE, margin = "both")        

# term-frequency inverse term-frequency ########################################
tf_idf <- dfm_tfidf(dfm)

# keywords in context & save ###################################################
kwic <- kwicTokens(tokens)
# kwicDF <- kwic  %>% as.data.frame()
# write.xlsx(kwic, file="./Results/TX_DisasterWOI.xlsx", sheetName="TX Disaster WOI", row.names=FALSE)

# bigrams ######################################################################
bigrams <- tokens_ngrams(tokens, n = 2)  %>% 
        dfm()  %>% 
        dfm_sort(decreasing = TRUE, margin = "both")

# trigrams ######################################################################
trigrams <- tokens_ngrams(tokens, n = 3)  %>%
        dfm()  %>% 
        dfm_sort(decreasing = TRUE, margin = "both")

# filter for specific keywords #################################################
kwFiltered <- kwic  %>% 
              filter(keyword == "climate change" |
              keyword == "hurricane" |
              keyword == "flooding" |
              keyword == "disaster" | 
              keyword == "storm" |
              keyword == "hurricane ike" |
              keyword == "hurricane dolly" |
              keyword == "hurricane harvey" |
              keyword == "hurricane irma" |
              keyword == "hurricane imelda" |
              keyword == "hurricane laura" |
              keyword == "hurricane ian")  %>% 
              as.data.frame()  %>%                 
              mutate(year = str_sub(docname, start = 1, end = 4))  %>% 
              select(docname, keyword, year)

# write.xlsx(kwFiltered, file="./Results/TX_Hurricane_Climate.xlsx", sheetName="Summary", row.names=FALSE)

# count keywords by year #######################################################
kwGrouped <- kwFiltered  %>% 
             group_by(year, keyword)  %>% 
             summarize(kcount = n()) 

# plot filtered keywords by year & save ########################################
kwXyear <- ggplot(data = kwGrouped,
              aes(x = year,
                  y = kcount,
                  group = keyword)) +
                  geom_line(aes(color = keyword)) +
                  ggtitle("Frequency of Keyword Use by Year - All Transcripts") +
                  theme(plot.title = element_text(size=24, hjust = 0.5))

# ggsave("./Graphs/TXkwordXYear.jpg", kwXyear)