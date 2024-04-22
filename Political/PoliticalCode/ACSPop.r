################################# FILE DESCRIPTION #############################
##                                                                            ##
##  This file handles the preliminary steps necessary for                     ##  
##    the political data                                                      ##
##                                                                            ##
##  Geography data was downloaded using tigiris                               ##
##                                                                            ##
##  Political data was downloaded from the Harvard Dataverse for Algara &     ##
##    Sharif (2021) "Replication Data for: Partisanship & Nationalization     ##
##    in American Elections: Evidence from Presidential, Senatorial, &        ##
##    Gubernatorial Elections in the U.S. Counties, 1872-2020",               ##
##      https://doi.org/10.7910/DVN/DGUMFI                                    ##
##                                                                            ##
################################################################################

## suppress warnings ###########################################################
options(warn = -1)

## load libraries ##############################################################
library(tidyverse)                 # data manipulation
library(tigris)                    # geographic boundaries
library(tidycensus)                # ACS data
library(sf)                        # work with geographic boundaries

## load political data, with cartography #######################################
e08 <- read_sf("/GitHub/NSF_ClimatePolitics/Political/PoliticalModifiedData/Geopackages/us08_election.gpkg")
