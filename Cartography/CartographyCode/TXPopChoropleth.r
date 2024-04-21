################################################################################
##                                                                            ##      
## This file creates a choropleth of county population in texas for 2021      ##  
##                                                                            ##
## County shape date downloaded from the U.S. Census using the                ##
##      tigris() package                                                      ##  
 ##                                                                            ##    
################################################################################

## load libraries
library(tidyverse)
library(tigris)
library(leaflet)
library(sf)
library(tidycensus)
library(RColorBrewer)
library(htmlwidgets)

## load data
county <- st_transform(tigris::counties(year = 2020), crs = "+proj=longlat +datum=WGS84")  %>% 
    select(STATEFP, COUNTYFP, GEOID, INTPTLAT, INTPTLON, geometry)  %>% 
    mutate(GEOID = as.numeric(GEOID))  %>% 
    filter(STATEFP < 57)

population <- tidycensus::get_estimates(geography = "county", product = "population")  %>% 
    filter(variable == "POPESTIMATE")  %>% 
    mutate(GEOID = as.numeric(GEOID))  %>% 
    filter(GEOID <57000)

## merge geography with population
county_pop <- left_join(county, population, by = "GEOID")  %>% 
    filter(value != is.na(value))


## subset and sort texas counties by population
texas <- county_pop  %>% filter(STATEFP == "48")  %>% 
    arrange(value)

pop_oneyear <- texas  %>% filter(value >= 65000)
pop_threeyear <- texas  %>% filter(value >= 20000)
pop_fiveyear <- texas  %>% filter(value <20000)

nrow(pop_oneyear) #55
nrow(pop_threeyear) #120
nrow(pop_fiveyear) # 134

## US map stylization
hover <- sprintf("This is <strong>%s</strong>. It has a population of %s.", 
    county_pop$NAME, prettyNum(county_pop$value, big.mark = ",")) %>% 
    lapply(htmltools::HTML)


highlight = highlightOptions(
    weight = 2,
    color = "#9fc5e8",
    fillOpacity = 1,
    bringToFront = TRUE)


bins <- c(50, 1000, 5000, 25000, 100000, 50000, 1000000, 5000000, Inf)
pal <- colorBin("RdYlGn", domain = county_pop$values, bins = bins)

labels = labelOptions(
                    style = list("font-weight" = "normal",
                    padding = "3x 8x"),
                    textsize = "15px",
                    direction = "auto")

## US Map
map <- leaflet()  %>% 
    addPolygons(data = county_pop,
                smoothFactor = 0.2,
                fillColor = ~pal(value),
                fillOpacity =1,
                stroke = TRUE,
                weight = 1,
                opacity = 1,
                color = "#000",
                highlight = ~highlight,
                label = ~hover,
                labelOptions = ~labels)   %>% 
 addLegend(pal = pal,
            values = county_pop$value,
            position = "bottomright",
            title = "Estimated Population")

## save US map
# saveWidget(map, file="./Maps/2022_CountyPopulation.html")
 
tx_hover <- sprintf("This is <strong>%s</strong>. It has a population of %s.", 
    texas$NAME, prettyNum(texas$value, big.mark = ",")) %>% 
    lapply(htmltools::HTML)


tx_highlight = highlightOptions(
    weight = 2,
    color = "#9fc5e8",
    fillOpacity = 1,
    bringToFront = TRUE)
 
 # tricolor (for one, three, five year acs data)
tx_bins <- c(0, 20000, 65000, Inf)
tx_pal <- colorBin("RdYlGn", domain = texas$values, bins = tx_bins)


tx_labels = labelOptions(
                    style = list("font-weight" = "normal",
                    padding = "3x 8x"),
                    textsize = "15px",
                    direction = "auto")

## Map
tx_map <- leaflet()  %>% 
    addPolygons(data = texas,
                smoothFactor = 0.2,
                fillColor = ~tx_pal(value),
                fillOpacity =1,
                stroke = TRUE,
                weight = 1,
                opacity = 1,
                color = "#000",
                highlight = ~tx_highlight,
                label = ~tx_hover,
                labelOptions = ~tx_labels)  %>% 
 addLegend(pal = tx_pal,
            values = texas$value,
            position = "bottomright",
            title = "Estimated Population")


