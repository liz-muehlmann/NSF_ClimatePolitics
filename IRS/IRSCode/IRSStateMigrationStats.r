################################################################################
##                                                                            ##      
## This file is used to get descriptive statistics for 2020 migration data    ##
##      from the IRS.                                                         ##    
##                                                                            ##  
## IRS migration data based on tax returns downloaded from:                   ##
##      https://www.irs.gov/statistics/soi-tax-stats-migration-data           ##  
##                                                                            ##
## County shape date downloaded from the U.S. Census using the                ##
##      tigris() package                                                      ##  
##                                                                            ##    
################################################################################

## load libraries
library(plyr)
library(tidyverse)
library(sf)
library(tigris)

## load data
inflow <- read.csv("./Original_Data/Migration/2021/2021_Inflow_State.csv")
outflow <- read.csv("./Original_Data/Migration/2021/2021_Outflow_State.csv")  %>% 
    rename(oo_FIPS = oo_stateFIPS,
           od_FIPS = od_stateFIPS,)

states <- states()  %>% 
    select(STUSPS, NAME, INTPTLAT, INTPTLON, geometry)  %>% 
    rename(state_abbr = STUSPS)

## FIPS PROCESSING #############################################################
## add leading zeros to inflow data FIPS
inflow$id_FIPS <- sprintf("%02d", inflow$id_FIPS)
inflow$io_FIPS <- sprintf("%02d", inflow$io_FIPS)

outflow$oo_FIPS <- sprintf("%02d", outflow$oo_FIPS)
outflow$od_FIPS <- sprintf("%02d", outflow$od_FIPS)

# subset header rows 
inflow_head <- inflow  %>% 
    filter(io_FIPS > 57)

outflow_head <- outflow  %>% 
    filter(oo_FIPS > 48)

## subset texas
texas <- states  %>% filter(state_abbr == "TX")  %>% 
    st_drop_geometry(geometry)  %>% 
    rename(tx_abbr = state_abbr,
           tx_name = NAME,
           tx_lat = INTPTLAT,
           tx_lon = INTPTLON)

## subset people migrating INTO Texas & merge with GIS data
inflow_TX <- inflow  %>% 
    rename(state_abbr = io_stateabbr)  %>% 
    filter(id_FIPS == 48 & io_FIPS < 95 & state_abbr != "FR" & io_statename != "TX Non-migrants")  %>% 
    left_join(states, by = join_by(state_abbr))  
inflow_TX <- cbind(texas, inflow_TX)

## subset people migrating OUT of Texas
outflow_TX <- outflow  %>% 
    rename(state_abbr = od_stateabbr)  %>% 
    filter(oo_FIPS == 48 & od_FIPS < 95 & state_abbr != "FR" & od_stateName != "TX Non-migrants")  %>% 
    left_join(states, by = join_by(state_abbr))
outflow_TX <- cbind(texas, outflow_TX)


## save shapefiles
# st_write(inflow_TX, "./Modified_Data/Cartographic/2021_TX_IRSMigration/2021_IRS_TXMigration/2021_TX_InflowState.shp")
# st_write(outflow_TX, "./Modified_Data/Cartographic/2021_TX_IRSMigration/2021_IRS_TXMigration/2021_TX_outflowState.shp")
# st_write(states, "./Original_Data//Migration/2021/States/states.shp")
