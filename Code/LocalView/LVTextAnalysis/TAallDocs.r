################################################################################
##                                                                            ##
## This file uses the quanteda package to analyze the all Local View data     ##  
##                                                                            ##  
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##     
################################################################################

# load preliminaries file ######################################################
source("./Code/LocalView/LVTextAnalysis/TAPrelim.r")

# filter only videos with transcripts (n = 103,812)
data <- all_docs  %>% 
        filter(caption_text_clean != "<No caption available>")

# create corpus ################################################################
corpus <- createCorpus(data)

# summarize full corpus ########################################################
stateGov <- data  %>% 
           group_by(state_name, place_govt)  %>% 
           count()  %>% 
           as.data.frame()

placeSummary <- data  %>% 
           group_by(year, state_name, place_name)  %>% 
           count()  %>% 
           as.data.frame()
           
stateSummary <- aggregate(stateGov$n, by=list(state=stateGov$state_name), FUN=sum)

# write.xlsx(stateGov, file="./LocalView/Results/Summaries/AllMeetingTypesByState.xlsx", sheetName="State Transcript", row.names=FALSE)
# write.xlsx(placeSummary, file="./LocalView/Results/Summaries/AllMeetingTypesByState.xlsx", sheetName="State Gov Type", append= TRUE, row.names=FALSE)
# write.xlsx(stateTranscript, file="./LocalView/Results/Summaries/AllMeetingTypesByState.xlsx", sheetName="Year State Place Transcript", append= TRUE, row.names=FALSE)

# tokenize the corpus ##########################################################
tokens <- tokenizeCorpus(corpus)
docvars_df <- docvars(tokens)
docvars_df$docname <- paste("text", 1:nrow(docvars_df), sep="")

# create document-feature matrix & sort ########################################
# dfm <- dfmTokens(tokens)
# dfmSort <- dfm_sort(dfm, decreasing = TRUE, margin = "both")        

# term-frequency inverse term-frequency ########################################
# tf_idf <- dfm_tfidf(dfm)

# keywords in context & save ###################################################
kwic <- kwicTokens(tokens)
kwic_df <- merge(kwic, docvars_df, by = "docname")  %>% 
           mutate(year = str_sub(meeting_date, start = 1, end = 4))  %>% 
           as.data.frame()
           
# write.xlsx(kwic_df, file="./LocalView/Results/Summaries/All_DisasterWOI.xlsx", sheetName="All Disaster WOI", row.names=FALSE)
# n-grams ######################################################################
# bigrams <- tokens_ngrams(tokens, n = 2)  %>% 
#         dfm()  %>% 
#         dfm_sort(decreasing = TRUE, margin = "both")

# trigrams <- tokens_ngrams(tokens, n = 3)  %>%
#         dfm()  %>% 
#         dfm_sort(decreasing = TRUE, margin = "both")

# filter for specific keywords #################################################
kwFiltered <- kwic_df  %>% 
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
              keyword == "hurricane ian")

kwHCC <- kwic_df  %>% 
        filter(keyword == "climate change" |
               keyword == "hurricane") 

# count keywords by year #######################################################
kwGrouped <- kwFiltered  %>% 
             group_by(year, keyword)  %>% 
             summarize(kcount = n()) 

kwHCCGrouped <- kwHCC  %>% 
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

# ggsave("./LocalView/Results/Graphs/AllkwordXYear.jpg", kwXyear)

kwHCCXyear <- ggplot(data = kwHCCGrouped,
              aes(x = year,
                  y = kcount,
                  group = keyword)) +
                  geom_line(aes(color = keyword)) +
                  ggtitle("Frequency of Hurricane & Climate Change Use - All Transcripts") +
                  theme(plot.title = element_text(size=24, hjust = 0.5))

# ggsave("./LocalView/Results/Graphs/ClimateChange_HurricaneByYear.jpg", kwHCCXyear)
