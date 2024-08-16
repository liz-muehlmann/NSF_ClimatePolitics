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
#   load source                                                             ####

source("./LocalView/code/processing_scripts/textAnalysis_prelims.r")

kwic <- kwic(tokens, pattern = disasters, window = 20)

##  ............................................................................
##  make kwic searchable                                                    ####

kwic_df <- merge(kwic, docvars_df, by = "docname") %>% 
    as.data.frame()


##  ............................................................................
##  filter for certain patterns                                             ####

storms <- kwic_df %>% 
    filter(pattern == "storms")

wind <- kwic_df %>% 
    filter(pattern == "wind" &
           keyword != "window") 

fire <- kwic_df %>% 
    filter(pattern == "fire")

floods <- kwic_df %>% 
    filter(pattern == "flood")

heat <- kwic_df %>% 
    filter(pattern == "heat")