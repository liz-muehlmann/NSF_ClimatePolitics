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

library(ggplot2)
library(RColorBrewer)

kwic <- kwic(tokens, pattern = disasters, window = 20)

##  ............................................................................
##  make kwic searchable                                                    ####

kwic_df <- merge(kwic, docvars_df, by = "docname") %>% 
    as.data.frame()

##  ............................................................................
##  create overall pattern summary                                          ####

pattern_summary <- kwic_df %>% 
    group_by(transcript_year, pattern) %>% 
    summarize(n_pattern = n(),
              n_transcripts = n_distinct(transcript_id))

# write.csv(pattern_summary, 
#           "./LocalView/results/summaries/240918_pattern_summary.csv", 
#           row.names = FALSE)

pattern_graph <- pattern_summary %>%   
    ggplot( aes(x=transcript_year, y=n_pattern, group=pattern, color=pattern)) +
    geom_line() 
# ggsave(plot = pattern_graph, "./LocalView/results/graphs/240918_pattern_summary.jpg")

##  ............................................................................
##  separate by pattern                                                     ####

storms <- kwic_df %>% 
    filter(pattern == "storms")

wind <- kwic_df %>% 
    filter(pattern == "wind" &
           keyword != "window") 

fire <- kwic_df %>% 
    filter(pattern == "fire" &
           lead(post, 1) != "department") 

floods <- kwic_df %>% 
    filter(pattern == "flood")

heat <- kwic_df %>% 
    filter(pattern == "heat")

climate <- kwic_df %>% 
    filter(pattern == "climate")

##  ............................................................................
##  summaries                                                               ####

storms_summary <- storms %>% 
    group_by(transcript_year, keyword) %>% 
    summarize(n_keyword = n(),
              n_transcripts = n_distinct(transcript_id))

# write.csv(storms_summary, 
#        "./LocalView/results/summaries/240918_storms_summary.csv", 
#        row.names = FALSE)

wind_summary <- wind %>% 
    group_by(transcript_year, keyword) %>% 
    summarize(n_keyword = n(),
              n_transcripts = n_distinct(transcript_id))

# write.csv(wind_summary, 
#           "./LocalView/results/summaries/240918_wind_summary.csv", 
#           row.names = FALSE)

fire_summary <- fire %>% 
    group_by(transcript_year, keyword) %>% 
    summarize(n_keyword = n(),
              n_transcripts = n_distinct(transcript_id))

# write.csv(fire_summary, 
#           "./LocalView/results/summaries/240918_fire_summary.csv", 
#           row.names = FALSE)

floods_summary <- floods %>% 
    group_by(transcript_year, keyword) %>% 
    summarize(n_keyword = n(),
              n_transcripts = n_distinct(transcript_id))

# write.csv(floods_summary, 
#           "./LocalView/results/summaries/240918_floods_summary.csv", 
#           row.names = FALSE)

heat_summary <- heat %>% 
    group_by(transcript_year, keyword) %>% 
    summarize(n_keyword = n(),
              n_transcripts = n_distinct(transcript_id))

# write.csv(heat_summary, 
#           "./LocalView/results/summaries/240918_heat_summary.csv", 
#           row.names = FALSE)

climate_summary <- climate %>% 
    group_by(transcript_year, keyword) %>% 
    summarize(n_keyword = n(),
              n_transcripts = n_distinct(transcript_id))

# write.csv(climate_summary, 
#           "./LocalView/results/summaries/240918_climate_summary.csv", 
#           row.names = FALSE)


##  ............................................................................
##  plotting summaries

storms_graph <- storms_summary %>%   
    ggplot( aes(x=transcript_year, y=n_keyword, group=keyword, color=keyword)) +
    geom_line() 
# ggsave(plot = storms_graph, "./LocalView/results/graphs/240918_storm_summary.jpg")

wind_graph <- wind_summary %>%   
    ggplot( aes(x=transcript_year, y=n_keyword, group=keyword, color=keyword)) +
    geom_line() 
# ggsave(plot = wind_graph, "./LocalView/results/graphs/240918_wind_summary.jpg")

fire_graph <- fire_summary %>%   
    ggplot( aes(x=transcript_year, y=n_keyword, group=keyword, color=keyword)) +
    geom_line() 
# ggsave(plot = fire_graph, "./LocalView/results/graphs/240918_fire_summary.jpg")

floods_graph <- floods_summary %>%   
    ggplot( aes(x=transcript_year, y=n_keyword, group=keyword, color=keyword)) +
    geom_line() 
# ggsave(plot = floods_graph, "./LocalView/results/graphs/240918_floods_summary.jpg")

heat_graph <- heat_summary %>%   
    ggplot( aes(x=transcript_year, y=n_keyword, group=keyword, color=keyword)) +
    geom_line() 
# ggsave(plot = heat_graph, "./LocalView/results/graphs/240918_heat_summary.jpg")

climate_graph <- climate_summary %>%   
    ggplot( aes(x=transcript_year, y=n_keyword, group=keyword, color=keyword)) +
    geom_line() 
# ggsave(plot = climate_graph, "./LocalView/results/graphs/240918_climate_summary.jpg")


##  ............................................................................
##  subset sample                                                           ####

kwic_sample <- kwic_df %>% 
    group_by(pattern) %>% 
    slice_sample(n = 50) %>% 
    ungroup()

# write.csv(kwic_sample, 
#           "./LocalView/results/summaries/240918_kwic_summary.csv", 
#           row.names = FALSE)


##  ............................................................................
##  transcripts to pull                                                     ####

transcript_pull <- kwic_sample %>% 
    group_by(pattern) %>% 
    slice_sample(n = 2) %>% 
    ungroup() 

transcripts <- lvClean_transcript %>% 
    filter(transcript_id %in% transcript_pull$transcript_id) %>% 
    left_join(transcript_pull)

for (i in 1:nrow(transcripts)) {
    doc <- read_docx()
    
    # Add a title
    doc <- doc %>%
        body_add_par("Meeting Details", style = "heading 1")
    
    # Add all values at the top
    for (col in names(transcripts)) {
        doc <- doc %>%
            body_add_par(paste(col, ":", transcripts[i, col]), style = "Normal")
    }
    
    # Add the caption text below
    doc <- doc %>%
        body_add_par("Caption Text:", style = "heading 2") %>%
        body_add_par(transcripts[i, "caption_text_clean"], style = "Normal")
    
    transcript_year <- transcripts[i, "transcript_year"]
    keyword <- transcripts[i, "keyword"]
    transcript_id <- transcripts[i, "transcript_id"]
    
    # Save the document
    doc_name <- paste0("./LocalView/results/sample_transcripts/", 
                       transcript_year, "_", 
                       keyword, "_", 
                       transcript_id, ".docx")
    print(doc, target = doc_name)
    message("Created: ", doc_name)
}









