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

## load data ###################################################################
# elections with cartographic boundaries
e08 <- read_sf("/GitHub/NSF_ClimatePolitics/Political/PoliticalModifiedData/Geopackages/us08_election.gpkg")
e1216 <- read_sf("/GitHub/NSF_ClimatePolitics/Political/PoliticalModifiedData/Geopackages/us1216_election.gpkg")
e20 <- read_sf("/GitHub/NSF_ClimatePolitics/Political/PoliticalModifiedData/Geopackages/us20_election.gpkg")

# incorporated and census designated places majority counties ###################
icdp20 <- read.csv("/GitHub/NSF_ClimatePolitics/Cartography/CartographyData/Place/2020_IPCDP_MajorityCounties.csv") %>% 
    mutate(place_fips = str_pad(place_fips, 7, side="left", 0),
           full_fips = str_pad(full_fips, 5, side="left", 0)) %>% 
    select(-county_name)

## ACS ###########################################################################
acsyears <- list(2010, 2015, 2020)
acsvars <- c("B02001_002E", "B02001_003E", "B02001_004E", "B02001_005E", "B02001_006E", "B03001_003E", "B01002_001E", "B25064_001E", "B19001_001E","B01003_001E")

for(year in acsyears) {
    df <- get_acs(geography = "county", year = year, geometry = FALSE, variables = acsvars) %>% 
        select(-moe)  %>% 
        pivot_wider(
            names_from = variable,
            values_from = estimate,
            values_fill = 0) %>%
        rename(!! sym(paste("acs", year, "_white", sep="")) := "B02001_002",
               !! sym(paste("acs", year, "_black", sep="")) := "B02001_003",
               !! sym(paste("acs", year, "_amind", sep="")) := "B02001_004",
               !! sym(paste("acs", year, "_asian", sep="")) := "B02001_005",
               !! sym(paste("acs", year, "_nhapi", sep="")) := "B02001_006",
               !! sym(paste("acs", year, "_hispanic", sep="")) := "B03001_003",
               !! sym(paste("acs", year, "_medage", sep="")) := "B01002_001",
               !! sym(paste("acs", year, "_medgrossrent", sep="")) := "B25064_001",
               !! sym(paste("acs", year, "_medhhic", sep="")) := "B19001_001",
               !! sym(paste("acs", year, "_totalpop", sep="")) := "B01003_001")  %>%
        rename(full_fips = GEOID) %>%
        select(-NAME)
    assign(paste("acs", year, sep=""), df)
}

## join election with ip/cdp #######################################################
eicdp20 <- left_join(e20, icdp20, by = "full_fips") %>% 
    st_drop_geometry()
write.csv(eicdp20, "/GitHub/NSF_ClimatePolitics/Political/PoliticalModifiedData/2020_Place_ACSPop.csv")
