################################################################################
##                                                                            ##
## This file handles the preliminary steps necessary for merging the local    ##
##      view data with ACS, election, and geography data.                     ##
##                                                                            ##  
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ## 
## Geographic boundaries are downloaded tigris                                ##
##                                                                            ##
################################################################################

## load libraries ##############################################################
library(tidyverse)                 # data manipulation
library(arrow)                     # open and work with parquet filepaths
library(readtext)                  # read filepaths
library(tigris)                    # geographic boundaries
library(tidycensus)                # ACS data
library(sf)                        # work with geographic boundaries 
library(xlsx)                      # write excel sheets

## election function ###########################################################
processElections <- function(df){
    df  %>% 
    st_drop_geometry() %>% 
    pivot_wider(
      names_from = year,
      values_from = c(DVP, RVP, total_votes)
    )}

processACS <- function(df){
  df %>% 
    select(-moe)  %>% 
    pivot_wider(
      names_from = variable,
      values_from = estimate,
      values_fill = 0) %>% 
    rename(acs10_white = "B02001_002",
           acs10_black = "B02001_003",
           acs10_amind = "B02001_004",
           acs10_asian = "B02001_005",
           acs10_nhapi = "B02001_006",
           acs10_hispa = "B03001_003",
           acs10_medage = "B01002_001",
           acs10_medgrossrent = "B25064_001",
           acs10_medhhic = "B19001_001",
           acs10_totalpop = "B01003_001")  %>% 
    rename(full_fips = GEOID) %>% 
    select(-NAME)
}

processPlaces <- function(df){
  df  %>% 
    mutate(state_fips = str_pad(state_fips, width = 2, side = "left", pad ="0"),
           place_fips = str_pad(place_fips, width = 5, side = "left", pad ="0"),
           c1fips = str_pad(c1fips, width = 3, side = "left", pad = "0"),
           c2fips = str_pad(c2fips, width = 3, side = "left", pad = "0"),
           c3fips = str_pad(c3fips, width = 3, side = "left", pad = "0"),
           c4fips = str_pad(c4fips, width = 3, side = "left", pad = "0"),
           c5fips = str_pad(c5fips, width = 3, side = "left", pad = "0")) %>% 
    filter(state_fips != "02") 
}

oneCounty <- function(df){
  df  %>% filter_at(vars(c2fips, c3fips, c4fips, c5fips),all_vars(is.na(.)))  %>%
    mutate(c1Full = paste(state_fips, c1fips, sep=""))  %>%
    group_by(c1Full)  %>%
    select(-c2, -c3, -c4, -c5, -c2fips, -c3fips, -c4fips, -c5fips)
}

twoCounties <- function(df){
  df  %>%
    filter(!is.na(c2fips))  %>%
    mutate(c2Full = paste(state_fips, c2fips, sep=""))  %>%
    filter(!is.na(c1fips) &
             is.na(c3fips) &
             is.na(c4fips) &
             is.na(c5fips))  %>%
    select(place_fips, c2, c2fips, c2Full)
}

threeCounties <- function(df){
  df  %>%
    filter(!is.na(c3fips))  %>%
    mutate(c3Full = paste(state_fips, c3fips, sep=""))  %>%
    filter(!is.na(c1fips) &
             !is.na(c2fips) &
             is.na(c4fips) &
             is.na(c5fips))  %>%
    
    select(place_fips, c3, c3fips, c3Full)
}

fourCounties <- function(df){
  df  %>%
    filter(!is.na(c4fips))  %>%
    mutate(c4Full = paste(state_fips, c4fips, sep=""))  %>%
    filter(!is.na(c1fips) &
             !is.na(c2fips) &
             !is.na(c4fips) &
             is.na(c5fips))  %>%
    select(place_fips, c4, c4fips, c4Full)
}

fiveCounties <- function(df) {
  df  %>%
    filter(!is.na(c5fips))  %>%
    mutate(c5Full = paste(state_fips, c5fips, sep=""))  %>%
    filter_at(vars(c2fips, c3fips, c4fips, c5fips),all_vars(!is.na(.)))  %>%
    select(place_fips, c5, c5fips, c5Full)
}






