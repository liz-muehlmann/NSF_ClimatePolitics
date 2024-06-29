################################################################################
##                                                                            ##
## This file creates several visualizations regarding the Local View data     ##
##    data is processed in /Code/LocalView/TextAnalysis/LVPartisanshipACS.r   ##
##                                                                            ##
## geographic boundaries downloaded using the tidy census data                ##
##                                                                            ##    
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##    
## Political data was downloaded from the Harvard Dataverse for Algara &      ##
##    Sharif (2021) "Replication Data for: Partisanship & Nationalization     ##
##    in American Elections: Evidence from Presidential, Senatorial, &        ##
##    Gubernatorial Elections in the U.S. Counties, 1872-2020",               ##
##      https://doi.org/10.7910/DVN/DGUMFI                                    ##
##                                                                            ##
## ACS data was downloaded using the Tidy Census package                      ##
##      2006-2010 ACS > 2008                                                  ##
##      2011-2015 ACS > 2012 Election                                         ##
##      2016-2020 ACS > 2016 & 2020 Elections                                 ##
##                                                                            ##
## Election + ACS + County data were merged in                                ##        
##      Code/Political/AlgaraACSPlaces.r                                      ##
##                                                                            ##
## ** Majority counties were determined using ArcGIS' Tabulate Intersection   ##
##    function and processed using Google Docs.                               ##  
##    Methodology is available upon request.                                  ##               
##                                                                            ##
################################################################################

################################################################################
##                                                                            ##
##                        load libraries and data                             ##
##                                                                            ##
################################################################################
library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggExtra)
library(sf)
library(tigris)

s_abbr <- states() %>% 
    filter(STATEFP <= "56" & STATEFP != "02") %>% 
    select(NAME, STUSPS) %>% 
    rename(state_name = NAME,
           state_abbr = STUSPS) %>% 
    st_drop_geometry()

data <- read.csv("./Data/LocalView/LVModifiedData/LocalView_County.csv") %>% 
    group_by(transcript_year) %>% 
    mutate(stcounty_fips = str_pad(stcounty_fips, 5, "left", 0))

states <- data %>% 
    left_join(s_abbr) %>% 
    filter(!(is.na(DVP))) %>% 
    group_by(transcript_year, state_name) %>% 
    mutate(state_cy = sum(total_scriptCY, na.rm = TRUE),    # total number of transcripts
           state_cc = sum(sum_scriptCC, na.rm=TRUE),      # sum of transcripts with CC mention
           state_propCC = round((state_cc/state_cy)*100, 2),          # state proportion of CC metions
           total_vp = sum(DVP) + sum(RVP),                  # state total vote percentage
           state_dvp = round((sum(DVP)/total_vp)*100, 2),   # state dvp
           state_rvp = round((sum(RVP)/total_vp)*100, 2),   # state rvp
           vp_factor = ifelse(state_dvp <= 20.99, "0-20",   # vp range
                       ifelse(state_dvp >= 21 & state_dvp <= 40.99, "21-40",
                       ifelse(state_dvp >= 41 & state_dvp <= 60.99, "41-60",
                       ifelse(state_dvp >= 61 & state_dvp <= 80.99, "61-80", "81-100")))),
           vp_grey = ifelse(is.na(total_scriptCY), "No Transcript",
                     ifelse(state_cc != 0, vp_factor, "No CC Mention"))) %>%
    select(transcript_year, state_name, state_abbr, total_scriptCY, sum_scriptCC, state_cc, state_propCC, state_dvp, state_rvp, vp_factor, vp_grey) %>% 
    distinct(transcript_year, state_name, .keep_all = TRUE) 

# write to csv
# write.csv(states, "./Data/LocalView/LVModifiedData/states_propsum.csv", row.names=FALSE)

################################################################################
##                                                                            ##
##               point graph, state cc mention, by year                       ##
##                                                                            ##
################################################################################
colors_points <- c("#EF4056", "#8B0015","#3B75E9","#0000ff","#D4D4D4","#FFFFFF")

## proportions
states %>%
    ggplot(aes(x=transcript_year, y=state_propCC, group= state_name, color=vp_grey)) +
    geom_point(size = 2) +
    scale_color_manual(values= colors_points, name = "Democratic Vote Percentage") +  # Apply manual colors
    geom_text(aes(label = ifelse(state_propCC >= 15, as.character(state_abbr),"")), hjust=1.5, vjust=0, color = "black", size = 3) +
    labs(title = "Number of transcripts with at least one mention of climate change by state, \n year, and 2020 vote percentage",
         caption = "Labeled points indicate states with a proportion of 15% or more.")

## count
states %>%
    ggplot(aes(x=transcript_year, y=sum_scriptCC, group= state_name, color=vp_grey)) +
    geom_point(size = 2) +
    scale_color_manual(values= colors_points, name = "Democratic Vote Percentage") +  # Apply manual colors
    geom_text(aes(label = ifelse(sum_scriptCC >= 5, as.character(state_abbr),"")), hjust=1.5, vjust=0, color = "black", size = 3) +
    labs(title = "Number of transcripts with at least one mention of climate change by state, \n year, and 2020 vote percentage",
         caption = "Labeled points indicate states with 5 or more climate change mentions")
    

################################################################################
##                                                                            ##
##                             hexbin usa                                     ##
##                                                                            ##
################################################################################
s <- read_sf("./Data/Cartography/CartographyOriginal/states_hexgrid.gpkg") %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>% 
    left_join(states, by = c("google_name" = "state_name"))

hexbin <-ggplot(s) +
    geom_sf(aes(fill = state_cc)) +
    geom_sf_text(aes(label = iso3166_2)) +
    theme_void()

################################################################################
##                                                                            ##
##                             blue state DVP                                 ##
##                                                                            ##
################################################################################
# ggplot(states, aes(x=transcript_year, y=state_name, fill=state_dvp)) +
#     scale_color_brewer(palette = "Blues") +
#     geom_tile(color = "black") +  
#     geom_text(data = subset(states, state_cc >0), aes(label=state_cc, color="white")) +
#     theme_minimal(base_size = 8)

################################################################################
##                                                                            ##
##               heatmap of DVP, by state, year, CC transcript number           ##
##                                                                            ##
################################################################################
colors <- c("#ff6e66", "#D885A0","#b19cd9","#939DD1","#759ec9")

heatmap_SYVP <- ggplot(states, aes(x=transcript_year, y=state_name, fill=vp_factor)) +
    geom_tile(color = "black") +  
    scale_fill_manual(values = colors, name = "Democratic Vote Percentage") +  # Apply manual colors
    geom_text(data = subset(states, state_cc > 0), aes(label = state_cc), color = "black", size = 3) +
    theme_minimal(base_size = 8) +
    labs(title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
         caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Boxes without a value had no climate change mentions. \n Rows with no transcripts were dropped.")

################################################################################
##                                                                            ##
##               heatmap of DVP, by state, year, proportion of cc             ##
##                                                                            ##
################################################################################
heatmap_prop_SYVP <- ggplot(states, aes(x=transcript_year, y=state_name, fill=vp_factor)) +
    geom_tile(color = "black") +  
    scale_fill_manual(values = colors, name = "Democratic Vote Percentage") +  # Apply manual colors
    geom_text(data = subset(states, state_cc > 0), aes(label = state_propCC), color = "black", size = 3) +
    theme_minimal(base_size = 8) +
    labs(title = "Proportion of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
         caption = "Numbers included inside the boxes are the proportion of transcripts for the state that include at least one mention of climate change. \n Boxes without a value had no climate change mentions. \n Rows with no transcripts were dropped.")

################################################################################
##                                                                            ##
##               heatmap of DVP, by state, year, CC transcript number & grey  ##
##                                                                            ##
################################################################################
colors_grey <- c("#ff6e66", "#D885A0","#b19cd9","#939DD1","#D4D4D4","#FFFFFF")

heatmap_SYVP_grey <- ggplot(states, aes(x=transcript_year, y=state_name, fill=vp_grey)) +
    geom_tile(color = "black") +  
    scale_fill_manual(values = colors_grey, name = "Democratic Vote Percentage") +  # Apply manual colors
    geom_text(data = subset(states, state_cc > 0), aes(label = state_cc), color = "black", size = 3) +
    theme_minimal(base_size = 8) +
    labs(title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
         caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention. ")

################################################################################
##                                                                            ##
##               heatmap of DVP, by state, year, CC transcript number & grey  ##
##                                                                            ##
################################################################################
colors_grey <- c("#ff6e66", "#D885A0","#b19cd9","#939DD1","#D4D4D4","#FFFFFF")

heatmap_SYVP_grey <- ggplot(states, aes(x=transcript_year, y=state_name, fill=vp_grey)) +
    geom_tile(color = "black") +  
    scale_fill_manual(values = colors_grey, name = "Democratic Vote Percentage") +  # Apply manual colors
    geom_text(data = subset(states, state_propCC > 0), aes(label = state_propCC), color = "black", size = 3) +
    theme_minimal(base_size = 8) +
    labs(title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
         caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention. ")




################################################################################
##                                                                            ##
##               heatmap of DVP, by state, year, CC transcript number & grey  ##
##                                                                            ##
################################################################################
colors_grey <- c("#ff6e66", "#D885A0","#b19cd9","#939DD1","#D4D4D4","#FFFFFF")

heatmap_SYVP_grey <- ggplot(states, aes(x=transcript_year, y=state_name, fill=state_propCC)) +
    geom_tile(color = "black") +  
    scale_fill_manual(values = colors_grey, name = "Democratic Vote Percentage") +  # Apply manual colors
    geom_text(data = subset(states, state_propCC > 0), aes(label = state_propCC), color = "black", size = 3) +
    theme_minimal(base_size = 8) +
    labs(title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
         caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention. ")









    









