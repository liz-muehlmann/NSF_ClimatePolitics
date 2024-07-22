################################################################################
##                                                                            ##
## This file creates a shiny app to display visualizations for the LV data    ##
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
library(shiny)
library(ggplot2)
library(ggthemes)
library(tigris)
library(sf)

################################################################################
##                                                                            ##
##                        load libraries and data                             ##
##                                                                            ##
################################################################################
s_abbr <- states() %>% 
    filter(STATEFP <= "56" & STATEFP != "02") %>% 
    select(NAME, STUSPS) %>% 
    rename(state_name = NAME,
           state_abbr = STUSPS) %>% 
    st_drop_geometry()

data <- read.csv("./LocalView/data/modified/LVCounty.csv") %>% 
    group_by(transcript_year) %>% 
    mutate(stcounty_fips = str_pad(stcounty_fips, 5, "left", 0)) %>% 
    left_join(s_abbr) %>% 
    filter(!(is.na(DVP))) %>% 
    mutate(vp_factor = ifelse(DVP <= 20.99, "0-20",   # vp range
                       ifelse(DVP >= 21 & DVP <= 40.99, "21-40",
                       ifelse(DVP >= 41 & DVP <= 60.99, "41-60",
                       ifelse(DVP >= 61 & DVP <= 80.99, "61-80", "81-100")))),
           vp_grey = ifelse(is.na(total_scriptCY), "No Transcript",
                            ifelse(sum_scriptCC != 0, vp_factor, "No CC Mention"))) %>% 
    arrange(state_name)

### ui #########################################################################
##                                                                            ##
##                                 shiny ui                                   ##
##                                                                            ##
################################################################################

ui <- fluidPage(
    selectInput("states", label = "Choose a state:", choices= unique(as.character(data$state_name))),
    mainPanel(
        column(6, div(style="width:1500px; 
                      height:4000px;",
                  plotOutput("heatmap", height="100%")))))

### server #####################################################################
##                                                                            ##
##                                 shiny server                               ##
##                                                                            ##
################################################################################
server <- function(input, output) {

    output$heatmap <- renderPlot({
        plot_data <- filter(data, state_name == input$states)
    
    ggplot(plot_data, aes(x=transcript_year, y=county_name, fill=vp_grey)) +
        geom_tile(color = "black") +
        scale_fill_manual(name = "Democratic Vote Percentage",
                          values = c("0-20" = "#ff6e66",
                                     "21-40" = "#D885A0",
                                     "41-60" = "#b19cd9",
                                     "61-80" = "#939DD1",
                                     "81-100" = "#769dcc",
                                     "No CC Mention" = "#D4D4D4",
                                     "No Transcript" = "#FFFFFF",
                           labels = c("0-20", "21-40", "41-60", "61-80", "81-100", "No CC Mention", "No Transcript"))) +
        # theme(text = element_text(size = 50)) +
        theme_tufte(base_size = 15) +
        ggtitle("Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage") +
        geom_text(data = subset(plot_data, sum_scriptCC > 0), aes(label = sum_scriptCC), color = "black") +
        labs(caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.",
             x = "County Name",
             y = "Year of Transcript")
 })}

shinyApp(ui, server)
# ### resize & add sum_cc labels.
## regressions s:|