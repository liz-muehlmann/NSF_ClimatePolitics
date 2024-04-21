################################################################################
##                                                                            ##      
## This file maps Texas' County, Precinct, and Census Blocks                  ##  
##                                                                            ##  
## County and Census Block data was downloaded using the tigris package and   ##
## the U.S. Census API. (Available here: https://github.com/walkerke/tigris)  ##
## County and Census Block Data is from 2020.                                 ##
##                                                                            ##      
## Precinct data was downloaded from the Texas Legislative Council Data       ##   
## Portal (Available here: https://data.capitol.texas.gov/dataset/precincts)  ##
## The precinct data is from 2020.                                            ##
################################################################################

## Load Libraries
library(tidyverse) # data manipulation
library(leaflet) # maps
library(tigris) # boundary data
library(sf) # read shapefiles

## Load Data
tx_shp <- read_sf("./GitHub/NSF_Climate_LocalPolitics/Original_Data/Political/2020_PL_TXBoundaries/Precincts20G_2020.shp")
tx_cnty <- read.csv("./GitHub/NSF_Climate_LocalPolitics/Original_Data/Political/2020_PL_TXData.csv")  %>% 
    select(COUNTY, PCTKEY)
precinct <- full_join(tx_shp, tx_cnty, by = "PCTKEY")
precinct <- st_transform(precinct, crs = "+proj=longlat +datum=WGS84")
county <- st_transform(tigris::counties("48", year = 2020), crs = "+proj=longlat +datum=WGS84")
census <- tigris::blocks("48", year = 2020)

## Map Stylization
p_hover <- sprintf("This is precinct <strong>%s</strong> in <strong>%s</strong> County, Texas.", 
    precinct$PREC, precinct$COUNTY)  %>% 
    lapply(htmltools::HTML)

p_highlight = highlightOptions(
    weight = 2,
    color = "#9fc5e8",
    fillOpacity = 0.7,
    bringToFront = TRUE)

c_hover <- sprintf("This is <strong>%s</strong> County, Texas.", 
    county$NAME)  %>% 
    lapply(htmltools::HTML)

c_highlight = highlightOptions(
    weight = 2,
    color = "#8080",
    fillOpacity = 0.7,
    bringToFront = TRUE)

labels = labelOptions(
                    style = list("font-weight" = "normal",
                    padding = "3x 8x"),
                    textsize = "15px",
                    direction = "auto")

## Map
texas <- leaflet()  %>% 
    addPolygons(data = county,
                smoothFactor = 0.2,
                fillColor = "#808080",
                stroke = TRUE,
                weight = 1,
                opacity = 0.5,
                color = "#808080",
                highlight = ~c_highlight,
                label = ~c_hover,
                labelOptions = ~labels,
            group = "County Map")  %>% 
    addPolygons(data = precinct,
                smoothFactor = 0.2,
                fillColor = "#ADD8E6",
                fillOpacity = 1,
                stroke = TRUE,
                weight = 1,
                opacity = 0.5,
                color = "#354f52",
                highlight = ~p_highlight,
            group = "Precinct Map")  %>% 
    addLayersControl(
        baseGroups = c("County Map"), 
        overlayGroups = c("Precinct Map"),
        options = layersControlOptions(collapsed = FALSE)) 

