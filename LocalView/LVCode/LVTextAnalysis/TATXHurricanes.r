################################################################################
##                                                                            ##
##                      Be sure to change the year!                           ##
##                                                                            ##      
## This file uses the quanteda package to analyze the Local View data         ##  
##                                                                            ##  
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##     
################################################################################

# load preliminaries file ######################################################
source("./Code/LocalView_TextAnalysis/TextAnalysisPreliminaries.r")

# load data ####################################################################
# total documents = 103,783 | texas = 4,341 
data <- readtext(paste0(root, "*/*.txt"),                                       
        docvarsfrom = "filenames",
        docvarnames = c("date", "state", "location", "meetingID"),
        dvsep = "_")  %>% 
        mutate(year = str_sub(date, start = 1, end = 4))  %>% 
        filter(state == "Texas")

# create corpus for +/- year surrounding a hurricane ###########################
ikeDolly <- filter(data, 
               state == "Texas" &
               grepl("2007-|2008-|2009-", date)) %>% 
               corpus(text_field = "text")

harveyIrma <- filter(data, 
               state == "Texas" &
               grepl("2016-|2017-|2018-", date)) %>% 
               corpus(text_field = "text")

imelda <- filter(data, 
               state == "Texas" &
               grepl("2018-|2019-|2020-", date)) %>% 
               corpus(text_field = "text")

laura <- filter(data, 
               state == "Texas" &
               grepl("2019-|2020-|2021-", date)) %>% 
               corpus(text_field = "text")

ian <- filter(data, 
               state == "Texas" &
               grepl("2021-|2022-|2023-", date)) %>% 
               corpus(text_field = "text")

# hurricane names ##############################################################
hurricanes <- list(ikeDolly, harveyIrma, imelda, laura, ian)
names <- c("ikeDolly", "harveyIrma", "imelda", "laura", "ian")

# analyze hurricane years ######################################################
# summaries <- lapply(hurricanes, summarizeCorpus)
# for(i in seq_along(names)){
#     assign(
#         paste(names, "Summary", sep = "")[i], summaries[[i]]
#         ) }

# write.xlsx(ritaSummary, file="./Results/TX_HurricaneSummaries.xlsx", sheetName="rita", row.names=FALSE)
# write.xlsx(ike_dollySummary, file="./Results/TX_HurricaneSummaries.xlsx", sheetName="ike_dolly", append=TRUE, row.names=FALSE)
# write.xlsx(harvey_irmaSummary, file="./Results/TX_HurricaneSummaries.xlsx", sheetName="harvey_irma", append=TRUE, row.names=FALSE)
# write.xlsx(imeldaSummary, file="./Results/TX_HurricaneSummaries.xlsx", sheetName="imelda", append=TRUE, row.names=FALSE)
# write.xlsx(lauraSummary, file="./Results/TX_HurricaneSummaries.xlsx", sheetName="laura", append=TRUE, row.names=FALSE)
# write.xlsx(ianSummary, file="./Results/TX_HurricaneSummaries.xlsx", sheetName="ian", append=TRUE, row.names=FALSE)

# tokenize hurricane years #####################################################
tokens <- lapply(hurricanes, tokenizeCorpus)
for(i in seq_along(names)){
    assign(
        paste(names, "TOK", sep = "")[i], tokens[[i]]
        ) }
hTOK <- list(ikeDollyTOK, harveyIrmaTOK, imeldaTOK, lauraTOK, ianTOK)

# document-feature matrix hurricane years ######################################
dfm <- lapply(hTOK, dfmTokens)
for(i in seq_along(names)){
    assign(
        paste(names, "DFM", sep = "")[i], dfm[[i]]
        ) }
hDFM <- list(ikeDollyDFM, harveyIrmaDFM, imeldaDFM, lauraDFM, ianDFM)

# tf_idf: term-frequency inverse term frequency ################################
# ritaTFIDF <- dfm_tfidf(ritaDFM)  %>% 
#              dfm_trim(min_term=10, min_doc =2)
# ikeDollyTFIDF <- dfm_tfidf(ikeDollyDFM)
# harveyIrmaTFIDF <- dfm_tfidf(harveyIrmaDFM)
# imeldaTFIDF <- dfm_tfidf(imeldaDFM)
# lauraTFIDF <- dfm_tfidf(lauraDFM)
# ianTFIDF <- dfm_tfidf(ianDFM)

# create KWIC table for disaster keywords for hurricanes, Texas ################
kwic <- lapply(hTOK, kwicTokens) 
for(i in seq_along(names)){
    assign(
        paste(names, "KWIC", sep = "")[i], kwic[[i]]
        ) }

# relative frequency analysis [keyness] ########################################
# harveyIrmaRFA <- textstat_keyness(harveyIrmaDFM, 
#                  target = harveyIrmaDFM$year == "2017")
# textplot_keyness(harveyIrmaRFA)

# summarize kwic ###############################################################
hiKWICsum <- harveyIrmaKWIC  %>% 
             group_by(docname)  %>% 
             count(keyword)