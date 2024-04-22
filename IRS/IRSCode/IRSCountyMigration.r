################################################################################
##                                                                            ##      
## This file is used to process and combine the 2021 IRS migration data with  ##  
## Census cartography data at the county level. It saves inflow and           ##
## outflow migration as shapefiles. CSVs are saved to the GitHub via DVC.     ##
##                                                                            ##    
##      IF CODE IN THIS FILE IS MODIFIED DELETE THE ABOVE SHAPEFILES AND      ##
##                  UNCOMMENT LINES 102, 103 TO RESAVE!!                      ## 
##                                                                            ##  
## IRS migration data based on tax returns downloaded from:                   ##
##      https://www.irs.gov/statistics/soi-tax-stats-migration-data           ##  
##                                                                            ##
## County shape date downloaded from the U.S. Census using the                ##
##      tigris() package                                                      ##  
##                                                                            ##    
## Code for map using this data is available here:                            ##    
##      Code\2021_IRS_MigrationMap.r                                          ##    
##                                                                            ##    
################################################################################

## load libraries
library(plyr)
library(tidyverse)
library(tigris)
library(sf)


## load data
irs_inflow <- read.csv("./IRSMigration/Original Data/2021_IRS_County_InflowsNoSummaries.csv")  %>% 
    filter(dest_statefips == 48) # keep only rows who moved into texas
irs_outflow <- read.csv("./IRSMigration/Original Data/2021_IRS_County_OutflowsNoSummaries.csv")  %>% 
    filter(origin_statefips == 48) # keep only rows who moved out of texas
county <- st_transform(tigris::counties(year = 2020), crs = "+proj=longlat +datum=WGS84")  %>% 
    select(GEOID, INTPTLAT, INTPTLON, geometry)  %>% 
    rename(FIPS = GEOID)

## FIPS PROCESSING #############################################################
## add leading zeroes to inflow data FIPS
irs_inflow$origin_statefips <- sprintf("%02d", irs_inflow$origin_statefips)
irs_inflow$dest_countyfips <- sprintf("%03d", irs_inflow$dest_countyfips)
irs_inflow$origin_countyfips <- sprintf("%03d", irs_inflow$origin_countyfips)

## add leading zeros to outflow FIPS
irs_outflow$dest_statefips <- sprintf("%02d", irs_outflow$dest_statefips)
irs_outflow$dest_countyfips <- sprintf("%03d", irs_outflow$dest_countyfips)
irs_outflow$origin_countyfips <- sprintf("%03d", irs_outflow$origin_countyfips)

## create full FIPS
irs_inflow$origin_fips <- paste(irs_inflow$origin_statefips, irs_inflow$origin_countyfips, sep="")
irs_inflow$dest_fips <- paste(irs_inflow$dest_statefips, irs_inflow$dest_countyfips, sep="")

irs_outflow$origin_fips <- paste(irs_outflow$origin_statefips, irs_outflow$origin_countyfips, sep="")
irs_outflow$dest_fips <- paste(irs_outflow$dest_statefips, irs_outflow$dest_countyfips, sep="")

## simplify and merge fips
irs_inflow <- irs_inflow  %>% 
    select(origin_stateabbr, num_returns, num_individuals, agi, origin_countyname, origin_fips,dest_fips)  %>% 
    rename(n_returns = num_returns,
           n_individuals = num_individuals)

irs_outflow <- irs_outflow  %>% 
    select(dest_stateabbr, n_returns, n_individuals, adj_grossincome, dest_countyname, origin_fips,dest_fips)  %>% 
    rename(agi = adj_grossincome)

## separate into origin and destination & merge with geometry data
inflow_origin <- irs_inflow  %>% 
    select(origin_stateabbr, origin_countyname, origin_fips)  %>% 
    rename(FIPS = origin_fips)  %>% 
    left_join(county, by="FIPS")  %>% 
    rename(o_lat = INTPTLAT,
           o_long = INTPTLON,
           o_FIPS = FIPS)  %>% 
    st_drop_geometry(inflow_origin)  %>% 
    select(o_FIPS, origin_stateabbr, origin_countyname, o_lat,o_long)

inflow_dest <- irs_inflow  %>% 
    select(dest_fips, n_returns, n_individuals, agi)  %>% 
    rename(FIPS = dest_fips)  %>% 
    left_join(county, by="FIPS")  %>% 
    rename(d_lat = INTPTLAT,
           d_long = INTPTLON,
           d_FIPS = FIPS) %>%
    select(d_FIPS,n_returns,n_individuals, agi, d_lat, d_long)

outflow_origin <- irs_outflow  %>% 
    select(origin_fips, n_returns, n_individuals, agi)  %>% 
    rename(FIPS = origin_fips)  %>% 
    left_join(county, by="FIPS")  %>% 
    rename(o_lat = INTPTLAT,
           o_long = INTPTLON,
           o_FIPS = FIPS)  %>% 
    select(o_FIPS, n_returns, n_individuals, agi, o_lat, o_long)

outflow_dest <- irs_outflow  %>% 
    select(dest_fips, dest_stateabbr, dest_countyname)  %>% 
    rename(FIPS = dest_fips)  %>% 
    left_join(county, by="FIPS")  %>% 
    rename(d_lat = INTPTLAT,
           d_long = INTPTLON,
           d_FIPS = FIPS)  %>% 
    select(d_FIPS, dest_stateabbr, dest_countyname,d_lat, d_long) 

## merge inflow and outflow's origins and destinations
inflow <- cbind(inflow_origin, inflow_dest) 
outflow <- cbind(outflow_origin, outflow_dest)

## exclude within-texas migration
inflow_other <- inflow  %>% filter(origin_stateabbr != "TX")
outflow_other <- outflow  %>% filter(dest_stateabbr != "TX")

## subset inflow texas migration [Inflow counties match outflow, therefore outflow code is omitted]
inflow_tx <- inflow  %>% filter(origin_stateabbr == "TX")   %>% 
                         filter(!grepl("Non-migrants",origin_countyname))  %>% 
                         group_by(o_FIPS) %>%
                         filter(row_number()==1)

tx_counties <- county  %>% filter(FIPS >=48000 & FIPS <=48999)

tx_county <- st_transform(tigris::counties(year = 2020), crs = "+proj=longlat +datum=WGS84")  %>% 
    select(GEOID, NAMELSAD)  %>% 
    rename(FIPS = GEOID)
tx_county <- tx_county  %>% rename(o_FIPS = FIPS)

# subset counties not included in the inflow
inmissing_txcnties <- tx_counties  %>%  filter(! FIPS %in% inflow_tx$o_FIPS)  %>% 
                rename(o_FIPS = FIPS)  %>% 
                group_by(o_FIPS) %>%
               filter(row_number()==1)  %>% 
               st_drop_geometry(inmissing_txcnties)  %>% 
               left_join(tx_county, by = "o_FIPS")  
# st_write(inmissing_twenties, "/IRSMigration/Modified Data/IRSCartography/IRS_TXMissingCounties.gpkg")

## save to csv
# write_csv(inflow, "./IRSMigration/Results/Summaries/2021_IRS_Inflow_CountySummaries.csv")
# write_csv(outflow, "./IRSMigration/Results/Summaries/2021_TX_IRSOutflow.csv")

# write_csv(inflow, "./IRSMigration/Results/Summaries/2021_TX_InflowNoTX.csv")
# write_csv(outflow, "./IRSMigration/Results/Summaries/2021_TX_OutflowNoTX.csv")

