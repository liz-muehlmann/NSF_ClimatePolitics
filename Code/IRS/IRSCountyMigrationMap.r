################################################################################
##                                                                            ##      
## This file maps IRS migration data.                                         ##
## Details about the data and processing can be found here:                   ##       
##      Code\2021_IRS_Migration.r                                             ##    
##                                                                            ##    
## THIS CODE IS INCLUDED FOR POSTERITY. THE MIGRATION FLOWS ARE TOO DENSE     ##   
## FOR R & LEAFLET. IT TENDS TO BREAK THE MAP                                 ##    
##                                                                            ##    
##                                                                            ##     
##                                                                            ##    
################################################################################

## load libraries
library(tidyverse)
library(leaflet)
library(sf)
library(RColorBrewer)

## load data
inflow <- read_sf("./Modified_Data/Cartographic/2021_TX_IRSMigration/2021_TX_IRSInflow.csv") %>% 
    rename(dest_lat = d_lat,
           dest_lon = in_dstln,
           origin_lon = in_rgnln,
           origin_lat = in_rgnlt)  %>% 
    mutate_at(c("dest_lat",
                "dest_lon",
                "origin_lat",
                "origin_lon"), as.numeric)
outflow <- read_sf("./Modified_Data/Cartographic/2021_TX_IRSOutflowMigration/2021_TX_IRSOutflow_Migration.shp")

## filter foreign ZIPS
inflow_foreign  <- inflow %>% filter(orgn_st == "FR")
inflow <- inflow  %>% filter(orgn_st != "FR")

## map testing
# inflow %>%  leaflet  %>% 
#     addProviderTiles(providers$CartoDB.Positron)  %>% 
#     setView(lng = -96.25, lat = 39.50, zoom = 4)  %>% 
#     addCircleMarkers(lng = ~as.numeric(dest_lon), 
#                     lat = ~as.numeric(dest_lat), 
#                     radius = 2, 
#                     color = "#033001", 
#                     stroke = FALSE, 
#                     fillOpacity = 0.5)  %>% 
#     addCircleMarkers(lng = ~as.numeric(origin_lon), 
#                     lat = ~as.numeric(origin_lat), 
#                     radius = 2, 
#                     color = "#033001", 
#                     stroke = FALSE, 
#                     fillOpacity = 0.5)

## basemap
inflow_map <- leaflet(data = inflow)  %>% 
              addProviderTiles(providers$CartoDB.Positron)  %>% 
              setView(lng = -96.25, lat = 39.50, zoom = 4) 

for (i in 1:nrow(inflow)) {
    inflow_map <- inflow_map %>% 
                  addPolylines(lat=c(inflow[i,]$origin_lat,
                               inflow[i,]$dest_lat),
                               lng=c(inflow[i,]$origin_lon,
                               inflow[i,]$dest_lon), 
                               weight = 1, 
                               opacity =3) %>%
    addCircleMarkers(lng = ~as.numeric(dest_lon), 
                    lat = ~as.numeric(dest_lat), 
                    radius = 2, 
                    color = "#033001", 
                    stroke = FALSE, 
                    fillOpacity = 0.5)  %>% 
    addCircleMarkers(lng = ~as.numeric(origin_lon), 
                    lat = ~as.numeric(origin_lat), 
                    radius = 2, 
                    color = "#033001", 
                    stroke = FALSE, 
                    fillOpacity = 0.5)
}

