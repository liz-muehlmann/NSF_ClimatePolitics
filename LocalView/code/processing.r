### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the local view data   ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##              
##          Algara-Sharif (2008-2020)                                         ##
##              Sharif (2021) "Replication Data for: Partisanship & Nationalization
##              in American Elections: Evidence from Presidential, Senatorial, &   
##              Gubernatorial Elections in the U.S. Counties, 1872-2020",     ##      
##              https://doi.org/10.7910/DVN/DGUMFI                            ##      
##          American Community Survey (2010, 2015, 2020)                      ##
##              2006-2010 ACS > 2008                                          ##
##              2011-2015 ACS > 2012 Election                                 ##
##              2016-2020 ACS > 2016 & 2020 Elections                         ##
##          FEMA (2010-2023)                                                  ## 
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
##          Climate Change Vulnerability (2010-2023)                          ## 
##              https://github.com/wachiuphd/CVI
##          USDA Rural-Urban                                                  ## 
##          Incorporated/Census Designated Places (2023)                      ## 
##              IP/CDP csvs were saved using the tabulate intersection function in ArcGIS 
##          Virginia                                                          ##
##              https://www.census.gov/library/reference/code-lists/ansi.2020.html#cousub
##          Counties (2010, 2015, 2020)                                       ##
##              tigris                                                        ##
##          States                                                            ##
##              tigris                                                        ##   
##                                                                            ##
################################################################################ 

### load packages ##############################################################
##                                                                            ##
##                                                                            ##
################################################################################
library(arrow)                     # open and work with parquet filepaths
library(readtext)                  # read filepaths
library(tidyverse)                 # data manipulation
library(sf)                        # work with shapefiles
library(tigris)                    # download geographic boundaries
library(tidycensus)                # ACS data
library(readxl)                    # work with xlsx files (CVI)
`%notin%` <- Negate(`%in%`)        # create not in operator

### custom functions ###########################################################
##                                                                            ##
##                  custom functions used during processing                   ##
##                                                                            ##
################################################################################
## calculate majority county-place overlap
majority <- function(df){
    df %>% mutate(coverage = round(coverage, 2)) %>% 
        filter(coverage != 0) %>% 
        group_by(place_fips) %>% 
        filter(coverage == max(coverage, na.rm = TRUE)) %>% 
        mutate(place_fips = str_pad(place_fips, 7, "left", "0"),
               stcounty_fips = str_pad(stcounty_fips, 5, "left", 0),
               coverage = as.numeric(coverage),
               state_fips = str_sub(stcounty_fips, 0, 2),
               county_fips = str_sub(stcounty_fips, 3, 5)) 
}

## fix problems with places that don't have an associated county
fixFIPS <- function(df){
    df %>% mutate(county_name = 
                      case_when(place_fips == "5109816" ~ "Bristol County",
                                place_fips == "5121344" ~ "Danville County",
                                place_fips == "5135000" ~ "Hampton County", 
                                place_fips == "5138424" ~ "Hopewell County",
                                place_fips == "5147672" ~ "Lynchburg County",
                                place_fips == "5157000" ~ "Norfolk County",
                                place_fips == "5161832" ~ "Petersburg County",
                                place_fips == "5163768" ~ "Poquoson County",
                                place_fips == "5164000" ~ "Portsmouth County",
                                place_fips == "5183680" ~ "Waynesboro County",
                                place_fips == "2205000" ~ "East Baton Rouge County",
                                place_fips == "2965000" ~ "St. Louis County",
                                .default = county_name))
}

### states #####################################################################
##                                                                            ##
##             download state information with and without geometry           ##
##                                                                            ##
################################################################################
statesGeo <- states() %>%
    filter(GEOID < 57 & GEOID != "02" & GEOID != "15") %>%
    rename(state_fips = GEOID,
           state_name = NAME) %>%
    select(state_fips, state_name)

states <- statesGeo %>% 
    st_drop_geometry()

### counties ###################################################################
##                                                                            ##
##             download county information with and without geometry          ##
##                                                                            ##
################################################################################
counties00Geo <- counties(year = 2000, cb = TRUE) %>% 
    filter(STATEFP < 57 & STATEFP != "02" & STATEFP != "15") %>%
    mutate(county_name = paste(NAME, LSAD_TRANS, sep=" "),
           stcounty_fips = paste(STATE, COUNTY, sep = ""),
           county_name = ifelse(stcounty_fips == "11001", "District of Columbia", county_name),
           county_name = ifelse(stcounty_fips == "17099", "LaSalle County", county_name)) %>%
    rename(state_fips = STATEFP,
           county_fips = COUNTY) %>%
    left_join(states, by = c("STATE" = "state_fips")) %>% 
    select(state_name, state_fips, county_fips, county_name, stcounty_fips) %>% 
    add_row(state_name = "Colorado", 
            state_fips = "08", 
            county_fips = "014", 
            county_name = "Broomfield County", 
            stcounty_fips = "08014")

counties00 <- counties00Geo %>% 
    st_drop_geometry()

counties10Geo <- counties(year = 2010, cb = TRUE) %>%
    filter(STATEFP < 57 & STATEFP != "02" & STATEFP != "15") %>%
    mutate(stcounty_fips = paste(STATE, COUNTY, sep = ""),
           county_name = paste(NAME, LSAD, sep=" "),
           county_name = ifelse(stcounty_fips == "35013", "Dona Ana County", county_name ),
           county_name = ifelse(stcounty_fips == "11001", "District of Columbia", county_name)) %>% 
    rename(state_fips = STATE,
           county_fips = COUNTY) %>%
    left_join(states) %>%
    select(state_name, state_fips, county_fips, county_name, stcounty_fips) 

counties10 <- counties10Geo %>% 
    st_drop_geometry()

counties15Geo <- counties(year = 2015, cb = TRUE) %>% 
    filter(STATEFP < 57 & STATEFP != "02" & STATEFP != "15") %>%
    rename(stcounty_fips = GEOID,
           county_name = NAME,
           state_fips = STATEFP,
           county_fips = COUNTYFP) %>%
    mutate(county_name = paste(county_name, "County", sep = " "),
           county_name = ifelse(stcounty_fips == "35013", "Dona Ana County", county_name),
           county_name = ifelse(county_fips == "11001", "District of Columbia", county_name)) %>% 
    left_join(states) %>%
    select(state_name, state_fips, county_fips, county_name, stcounty_fips) 
    
counties15 <- counties15Geo %>% 
    st_drop_geometry()

counties20Geo <- counties(year = 2020, cb = TRUE) %>% 
    filter(STATEFP < 57 & STATEFP != "02" & STATEFP != "15") %>%
    rename(stcounty_fips = GEOID,
           county_name = NAME,
           state_fips = STATEFP,
           county_fips = COUNTYFP) %>%
    mutate(county_name = ifelse(stcounty_fips == "35013", "Dona Ana County", county_name),
           county_name = ifelse(county_fips == "11001", "District of Columbia", county_name),
           county_name = paste(county_name, "County", sep = " ")) %>% 
    left_join(states) %>% 
    select(state_name, state_fips, county_fips, county_name, stcounty_fips) 

counties20 <- counties20Geo %>% 
    st_drop_geometry()

### places #####################################################################
##                                                                            ##
##                aggregate place data to the county level overlap            ##
##                                                                            ##
################################################################################
## find majority overlap and fix FIPS for Incorporated and Census Designated Places
ip <- majority(read.csv("./GIS/modified/2020_IPIntersections.csv")) 
cdp <- majority(read.csv("./GIS/modified/2020_CDPIntersections.csv")) 
ipcdp <- rbind(ip, cdp) %>% 
    fixFIPS()

# write.csv(ipcdp, "./GIS/modified/2020_IPCDPMajorityCounties.csv", row.names = FALSE)

## bedford county merged with Bedford County in 2013,
## the code below ensures it is in the final data
bedford <- ipcdp %>% 
    slice(which(stcounty_fips == 51019)) %>% 
    mutate(stcounty_fips = "51515",
           county_name = "Bedford city") 

## Shannon County changed to Oglala Lakota County in 2014.
## the code below ensures it is in the final data
oglala <- ipcdp %>% 
    slice(which(stcounty_fips == 46102)) %>% 
    mutate(stcounty_fips = "46113",
           county_name = "Shannon County")

## boundary changes for places in the US 2020
changes <- read.csv("./GIS/original/2020_PlaceBoundaryChanges.csv") %>%
    filter(state_abbr != "HI" & 
           state_abbr != "AK") %>%
    mutate(state_fips = str_pad(state_fips, 2, "left", 0),
           placeOnly_fips = str_pad(place_fips, 5, "left", 0),
           place_fips = paste(state_fips, placeOnly_fips, sep=""),
           coverage = 100) %>% 
    fixFIPS() %>% 
    left_join(counties20, relationship = "many-to-many") %>% 
    select(place_fips, place_name, county_name, state_fips, county_fips, 
           stcounty_fips, coverage) %>% 
    bind_rows(bedford) %>% 
    bind_rows(oglala)

## Virginia subdivisions
va <- read.csv("./GIS/original/VACountySubdivisions.csv") %>% 
    mutate(county_fips = str_pad(county_fips, 3, "left", 0),
           state_fips = as.character(state_fips),
           place_fips = as.character(place_fips),
           coverage = 100) %>% 
    select(-state_name)

## merge all boundaries together to places
## this includes all places in all years. It's later merged with the Local View data.
places <- rbind(changes, ipcdp) %>% 
    bind_rows(va) %>% 
    group_by(stcounty_fips) %>% 
    mutate(n_places_incounty = n_distinct(place_fips)) 

n_places <- places %>% 
    select(stcounty_fips, n_places_incounty) %>%
    distinct(stcounty_fips, .keep_all = TRUE)

# write.csv(places, "./GIS/modified/2020_AllPlaces.csv")

### ACS Data ###################################################################
##                                                                            ##
##        this section loads and processes the American Community Survey      ##
##                                                                            ##
################################################################################
acsyears <- list(2010, 2015, 2020)
acsvars <- c("B02001_002E",               # white
             "B02001_003E",               # Black
             "B02001_004E",               # American Indian
             "B02001_005E",               # Asian
             "B02001_006E",               # Native Hawaiian, Pacific Islander
             "B03001_003E",               # Hispanic
             "B01002_001E",               # median age
             "B25064_001E",               # median gross rent
             "B19001_001E",               # median house hold income
             "B01003_001E",               # total population     
             "B15002_002E",               # over 25 male population, education total
             "B15002_006E",               # over 25 male, no high school
             "B15002_010E",               # over 25 male, high school
             "B15002_014E",               # over 25 male, associate's
             "B15002_015E",               # over 25 male, bachelor's
             "B15002_016E",               # over 25 male, master's
             "B15002_017E",               # over 25 male, professional degree
             "B15002_018E",               # over 25 male, doctorate
             "B15002_019E",               # over 25 female population, education total
             "B15002_023E",               # over 25 female, no high school
             "B15002_028E",               # over 25 female, high school
             "B15002_031E",               # over 25 female, associate's
             "B15002_032E",               # over 25 female, bachelor's
             "B15002_033E",               # over 25 female, master's
             "B15002_034E",               # over 25 female, professional degree
             "B15002_035E")               # over 25 female, doctorate

## download and process acs years 2010, 2015, 2020
for(y in acsyears) {
    df <- get_acs(geography = "county", year = y, geometry = FALSE, variables = acsvars) %>% 
        select(-moe) %>% 
        rename(stcounty_fips = GEOID) %>% 
        separate(NAME, into = c("county_name", "state_name"), sep = ",") %>% 
        mutate(state_name = str_trim(state_name),
                var_names = 
                   case_when(variable == "B02001_002" ~ "white",
                             variable == "B02001_003" ~ "black",              
                             variable == "B02001_004" ~ "amind",              
                             variable == "B02001_005" ~ "asian",              
                             variable == "B02001_006" ~ "nhapi",              
                             variable == "B03001_003" ~ "hispanic",           
                             variable == "B01002_001" ~ "medage",             
                             variable == "B25064_001" ~ "medgrossrent",       
                             variable == "B19001_001" ~ "medhhic",            
                             variable == "B01003_001" ~ "totalpop",     
                             variable == "B15002_014" ~ "eduM_aa",      
                             variable == "B15002_015" ~ "eduM_ba",      
                             variable == "B15002_016" ~ "eduM_ma",      
                             variable == "B15002_017" ~ "eduM_prof",    
                             variable == "B15002_018" ~ "eduM_phd",     
                             variable == "B15002_031" ~ "eduF_aa",      
                             variable == "B15002_032" ~ "eduF_ba",      
                             variable == "B15002_033" ~ "eduF_ma",      
                             variable == "B15002_034" ~ "eduF_prof",    
                             variable == "B15002_035" ~ "eduF_phd",
                             TRUE ~ as.character(variable)),
               state_name = str_trim(state_name)) %>%
        filter(stcounty_fips < 56999 &
                   state_name != "Alaska" &
                   state_name != "Hawaii") %>% 
        select(-variable) %>% 
        pivot_wider(values_from = estimate,
                    names_from = var_names) %>%
        mutate(edu_percentPop = (eduM_aa + eduF_aa + eduM_ba + eduF_ba + eduM_ma + eduF_ma + 
                                 eduM_prof + eduF_prof + eduM_phd + eduF_phd)/totalpop,
               perc_white = white/totalpop,
               perc_black = black/totalpop,
               perc_hispanic = hispanic/totalpop,
               other = amind + asian + nhapi,
               perc_other = other/totalpop,
               medhhic = medhhic/10000,
               ACSyear = y,
               state_fips = str_sub(stcounty_fips, 0, 2),
               county_fips = str_sub(stcounty_fips, 3, 5)) %>%
        select(stcounty_fips, state_name, ACSyear, medage, totalpop, medgrossrent, 
               medhhic, perc_white, perc_black, perc_hispanic, perc_other, edu_percentPop) 
    assign(paste("acs", y, sep=""), df)
}

acs2010 <- left_join(acs2010, counties10)
acs2015 <- left_join(acs2015, counties15)
acs2020 <- left_join(acs2020, counties20)

### Algara-Sharif ##############################################################
##                                                                            ##
##           this section loads and processes the Algara-Sharif Data          ##
##                                                                            ##
################################################################################
load("./LocalView/data/original/AlgaraSharif.Rdata")

## select the presidential data for 2008-2020 elections
algara <- pres_elections_release %>% 
    filter(election_year >=2008 & office == "PRES" & election_type == "G") %>% 
    select(election_year, 
           fips, 
           democratic_raw_votes, 
           republican_raw_votes, 
           pres_raw_county_vote_totals_two_party) %>%
    rename(stcounty_fips = fips,
           DEM = democratic_raw_votes,
           REP = republican_raw_votes,
           total_votes = pres_raw_county_vote_totals_two_party) %>% 
    mutate(DVP = DEM/total_votes,                   
           RVP = REP/total_votes) %>% 
    select(stcounty_fips, election_year, DVP, RVP)

## loop through algara-sharif and merge it with county data
for(y in unique(algara$election_year)){
    a <- algara %>%
        filter(election_year == y)
    
    if (y == 2008) {
        a_merged <- a %>%
            right_join(counties00, by = "stcounty_fips") %>%
            mutate(election_year = y)
    } 
    else if (y == 2012){
        a_merged <- a %>%
            right_join(counties10, by = "stcounty_fips") %>%
            mutate(election_year = y)
    } 
    else if (y == 2016){
        a_merged <- a %>%
            right_join(counties15, by = "stcounty_fips") %>%
            mutate(election_year = y)
    }
    else{
        a_merged <- a %>%
            right_join(counties20, by = "stcounty_fips") %>%
            mutate(election_year = y)
    }
    assign(paste("algara", y, sep=""), a_merged)
}

### rural-urban ################################################################
##                                                                            ##
##          This section loads and processes the rural and urban code         ##
##                                                                            ##
################################################################################
ru03 <- read_excel("./GIS/original/RuralUrban/2003USDA_RuralUrbanCodes.xls") %>% 
    rename(stcounty_fips = "FIPS Code",
           ruralUrban = "2003 Rural-urban Continuum Code") %>% 
    mutate(stcounty_fips = ifelse(stcounty_fips == 46113, 46102, stcounty_fips),
           ruralUrban = case_when(ruralUrban >= 4 ~ 4,
                                  .default = as.numeric(ruralUrban)),
           state_fips = str_sub(stcounty_fips, 0, 2),
           county_fips = str_sub(stcounty_fips, 3, 5)) %>% 
    filter(stcounty_fips < 57 & stcounty_fips != "02" & stcounty_fips != "15") %>% 
    select(stcounty_fips, state_fips, county_fips, ruralUrban) %>% 
    left_join(counties00)

ru13 <- read_excel("./GIS/original/RuralUrban/2013USDA_RuralUrbanCodes.xls") %>% 
    rename(stcounty_fips = FIPS,
           ruralUrban = RUCC_2013) %>%
    mutate(stcounty_fips = ifelse(stcounty_fips == 46113, 46102, stcounty_fips),
           ruralUrban = case_when(ruralUrban >= 4 ~ 4,
                                  .default = as.numeric(ruralUrban)),
           state_fips = str_sub(stcounty_fips, 0, 2),
           county_fips = str_sub(stcounty_fips, 3, 5)) %>% 
    select(stcounty_fips, state_fips, county_fips, ruralUrban) %>% 
    filter(stcounty_fips < 57 & stcounty_fips != "02" & 
               stcounty_fips != "15" & stcounty_fips != 51515) %>% 
    left_join(counties10)

ru23 <- read_excel("./GIS/original/RuralUrban/2023USDA_RuralUrbanCodes.xlsx") %>% 
    rename(stcounty_fips = FIPS,
           ruralUrban = RUCC_2023) %>% 
    mutate(stcounty_fips = ifelse(stcounty_fips == 46113, 46102, stcounty_fips),
           ruralUrban = case_when(ruralUrban >= 4 ~ 4,
                                  .default = as.numeric(ruralUrban)),
           state_fips = str_sub(stcounty_fips, 0, 2),
           county_fips = str_sub(stcounty_fips, 3, 5)) %>% 
    filter(stcounty_fips < 57 & stcounty_fips != "02" & stcounty_fips != "15") %>% 
    select(stcounty_fips, state_fips, county_fips, ruralUrban) %>% 
    left_join(counties20)

### CVI ########################################################################
##                                                                            ##
##      This section loads and processes the climate vulnerability index      ##
##                                                                            ##
################################################################################
cvi <- read_xlsx("./LocalView/data/original/202310_CVI.xlsx", sheet = "Domain CVI Values") %>% 
    rename(state_name = State,
           county_name = County,
           stcounty_fips = `FIPS Code`,
           overall_CVI = `Overall CVI Score`,
           baseline_all = `Baseline: All`,
           baseline_health = `Baseline: Health`,
           baseline_socioEcon = `Baseline: Social Economic`,
           baseline_infrastructure = `Baseline: Infrastructure`,
           baseline_environ = `Baseline: Environment`,
           climate_all = `Climate Change: All`,
           climate_health = `Climate Change: Health`,
           climate_socioEcon = `Climate Change: Social Economic`,
           climate_extreme = `Climate Change: Extreme Events`) %>% 
    filter(state_name != "AK",
           state_name != "HI") %>% 
    mutate(stcounty_fips = str_sub(stcounty_fips, 1, 5)) %>%
    group_by(stcounty_fips) %>% 
    mutate(n_counties = n()) %>% 
    reframe(
        across(c(overall_CVI, baseline_all, baseline_health, baseline_socioEcon, 
                 baseline_infrastructure, baseline_environ, climate_all, climate_health, 
                 climate_socioEcon, climate_extreme), ~ sum(.x)/n_counties)) %>% 
    distinct(stcounty_fips, .keep_all = TRUE)

## below fixes CVI data to account for Shannon County changing to Oglala County in 2014
oglalaCVI <- cvi %>%
    slice(which(stcounty_fips == 46113)) %>%
    mutate(stcounty_fips = "46102")
cvi <- rbind(oglalaCVI, cvi)

### FEMA Disaster Declarations #################################################
##                                                                            ##
##          This section loads and processes the FEMA data                    ##
##                                                                            ##
################################################################################
fema <- read.csv("./LocalView/data/original/2023_FEMA.csv") %>% 
        separate(declarationDate, into = c("femaYear", "femaMonth", "date"), sep = "-") %>%
        mutate(femaDay = str_sub(date, 0, 2)) %>% 
        filter(femaYear >= 2009 & femaYear < 2023,
               incidentType == "Coastal Storm"| 
                   incidentType == "Fire" |
                   incidentType == "Flood" |
                   incidentType == "Freezing" |
                   incidentType == "Hurricane" |
                   incidentType == "Mud/Landslide" |
                   incidentType == "Severe Ice Storm" |
                   incidentType == "Severe Storm" |
                   incidentType == "Snowstorm" |
                   incidentType == "Tornado" |
                   incidentType == "Typhoon" |
                   incidentType == "Winter Storm" |
                   incidentType == "Tropical Storm" |
                   incidentType == "Tsunami",
                   fipsStateCode < 57 &
                   fipsStateCode != "2" & 
                   fipsStateCode != "15") %>% 
        mutate(fipsStateCode = str_pad(fipsStateCode, 2, "left", "0"),
               fipsCountyCode = str_pad(fipsCountyCode, 3, "left", "0"),
               stcounty_fips = paste(fipsStateCode, fipsCountyCode, sep="")) %>% 
        group_by(fyDeclared, stcounty_fips) %>%
        rename(femaID = id,
               state_fips = fipsStateCode,
               county_fips = fipsCountyCode) %>% 
        mutate(n_decInYear = n(),
               n_decTypeYear = n_distinct(declarationType)) %>% 
        ungroup()  %>%
        select(femaID, femaYear, femaMonth, femaDay, declarationType, incidentType, state_fips, 
               county_fips, stcounty_fips, n_decInYear, n_decTypeYear)
    
# write.csv(fema, "./LocalView/data/modified/FEMAdisasterDeclarations.csv", row.names = FALSE)

## uncomment these lines to save FEMA data by year.
# for(y in unique(fema$femaYear)){
#     f <- fema %>% 
#         filter(femaYear == y)
#     assign(paste("fema", y, sep=""), f)
# }

## create femaBinary data 
femaBinary <- fema %>% 
    select(femaYear, stcounty_fips, state_fips, county_fips, n_decInYear, n_decTypeYear) %>% 
    distinct(stcounty_fips, .keep_all = TRUE)

## loop through femaBinary and merge it with county data
f_merged <- list()
for(y in unique(femaBinary$femaYear)){
    f <- femaBinary %>%
        filter(femaYear == y)
    
    if (y == 2009){
        f_merged[[y]] <- f %>%
            select(-state_fips, -county_fips) %>%
            right_join(counties00, by = "stcounty_fips") %>%
            mutate(femaYear = as.numeric(y),
                   transcript_year = femaYear + 1) 
        
    }
    else if (y >= 2010 & y <= 2014) {
        f_merged[[y]] <- f %>%
            select(-state_fips, -county_fips) %>%
            right_join(counties10, by = "stcounty_fips") %>%
            mutate(femaYear = as.numeric(y),
                   transcript_year = femaYear + 1) 
    } 
    else if (y >= 2015 & y <= 2019){
        f_merged[[y]] <- f %>%
            select(-state_fips, -county_fips) %>%
            right_join(counties15, by = "stcounty_fips") %>%
            mutate(femaYear = as.numeric(y),
                   transcript_year = femaYear + 1) 
    } else{
        f_merged[[y]] <- f %>%
            select(-state_fips, -county_fips) %>%
            right_join(counties20, by = "stcounty_fips") %>%
            mutate(femaYear = as.numeric(y),
                   transcript_year = femaYear + 1) 
    }
}

## n = 43,518
femaBinaryNA <- bind_rows(f_merged) %>% 
    mutate(transcript_year = as.character(transcript_year))

### local view data ############################################################
##                                                                            ##
##                                                                            ##
##                                                                            ##
################################################################################
## load parquet data and convert it to data frame (N = 153,452) 
LVRaw <- open_dataset("./LocalView/data/original/meetings/")  
LVRaw <- Scanner$create(LVRaw)
LVRaw <- LVRaw$ToTable()
LVRaw <- as.data.frame(LVRaw) 

## select only documents 2010+, filter out ACS and original caption columns, 
## and rows with no caption available (n = 103,379)
docs <- LVRaw %>%
    rename(place_fips = st_fips,
           meeting_type = place_govt) %>% 
    mutate(transcript_year = str_sub(meeting_date, 1, 4),
           place_fips = str_pad(place_fips, 7, "left", "0"),
           place_fips = case_when(place_fips == "5151650" ~ "5135000",
                                  place_fips == "5151710" ~ "5157000",
                                  place_fips == "5151730" ~ "5161832",
                                  .default = as.character(place_fips))) %>%
    filter(transcript_year >= 2010 & 
           !(caption_text_clean == "<No caption available>") & 
           state_name != "Alaska") %>% 
    select(vid_id, transcript_year, state_name, place_fips, place_name, 
           meeting_type, caption_text_clean)

## uncomment for the number of transcripts with No Caption Available (n = 49,640)
# nrow(LVRaw %>% filter(caption_text_clean == "<No caption available>"))

## filter out 9,434 observations in the local view data used county FIPS instead of place FIPS
docsCounty <- docs %>% 
    filter(grepl("County", place_name)) %>% 
    mutate(stcounty_fips = str_sub(place_fips, 3, 7)) %>% 
    left_join(places, by = "stcounty_fips", multiple = "any") %>% 
    select(-place_name.y, -place_fips.y) %>%                                           
    rename(place_name = place_name.x,
           place_fips = place_fips.x) 

## LV data with correct place data (n = 93,840)
docsNoCounty <- docs %>% 
    filter(!grepl("County", place_name)) %>% 
    left_join(places, by = "place_fips", relationship = "many-to-many") %>%      
    select(-place_name.y) %>% 
    rename(place_name = place_name.x) %>% 
    distinct(vid_id, transcript_year, .keep_all = TRUE)

## local view data by place, county, and transcript year (n = 103,372)
lvPlaces <- rbind(docsCounty, docsNoCounty) 

## local view data by county and year (n = 3,677)
lvCounty <- lvPlaces %>%
    select(-state_name, -county_name) %>%
    group_by(transcript_year, stcounty_fips) %>%
    mutate(n_meettype = n_distinct(meeting_type),
           total_scriptCY = n(),
           n_scriptCC = ifelse(str_count(caption_text_clean, "climate change") > 0, 1, 0),
           sum_scriptCC = sum(n_scriptCC),
           prop_cc = (sum_scriptCC/total_scriptCY)*100) %>%
    select(transcript_year, stcounty_fips, n_meettype, total_scriptCY, sum_scriptCC, prop_cc) %>%
    distinct(transcript_year, stcounty_fips, .keep_all = TRUE) %>%
    ungroup() 

### merge data #################################################################
##                                                                            ##
##                                                                            ##
##                                                                            ##
################################################################################  
## separate the local view data to combine with counties so NA = no transcript available
lvCounty10 <- lvCounty %>% 
    filter(transcript_year <= 2015) 
lvCounty15 <- lvCounty %>% 
    filter(transcript_year >= 2016 & transcript_year <= 2019)
lvCounty20 <- lvCounty %>% 
    filter(transcript_year >= 2020)

## merge 2008-2014 LV data with 2010 counties & 2010 ACS
empty10 <- list()
for(y in unique(lvCounty10$transcript_year)){
    empty10[[y]] <- lvCounty10 %>% 
        filter(transcript_year == y) %>% 
        select(-transcript_year) %>% 
        right_join(counties10) %>% 
        left_join(acs2010) %>% 
        mutate(transcript_year = y)
}
lvCounty10 <- bind_rows(empty10)

## merge 2015-2019 LV data with 2015 counties & 2015 ACS
empty15 <- list()
for(y in unique(lvCounty15$transcript_year)){
    empty15[[y]] <- lvCounty15 %>% 
        filter(transcript_year == y) %>% 
        select(-transcript_year) %>% 
        right_join(counties15) %>% 
        left_join(acs2015) %>% 
        mutate(transcript_year = y)
}
lvCounty15 <- bind_rows(empty15)

## merge 2020-2023 LV data with 2020 counties & 2020 ACS
empty20 <- list()
for(y in unique(lvCounty20$transcript_year)){
    empty20[[y]] <- lvCounty20 %>% 
        filter(transcript_year == y) %>% 
        select(-transcript_year) %>% 
        right_join(counties20) %>% 
        left_join(acs2020) %>% 
        mutate(transcript_year = y)
}
lvCounty20 <- bind_rows(empty20)

## n = 43,518
## data is at the county-year level
## each transcript year (2010-2023) includes all counties (NA if no transcript available)
lvCountyNA <- rbind(lvCounty10, lvCounty15, lvCounty20) %>% 
    left_join(n_places) %>% 
    filter(!is.na(n_places_incounty))

## fema data is merged with a one year lag (i.e., 2011 LV transcript uses 2010 fema declaration)
allDataNA <- lvCountyNA %>% 
    left_join(femaBinaryNA)

## USDA rural-urban designation
## 2003 RU data merges with 2010-2013 LV Data
## 2013 RU data merges with 2014-2022 LV Data
## 2023 RU data merges with 2023 LV Data
allDataNA03 <- allDataNA %>% 
    filter(transcript_year < 2014) %>% 
    left_join(ru03)
allDataNA13 <- allDataNA %>% 
    filter(transcript_year >= 2014 & transcript_year <= 2022) %>% 
    left_join(ru13)
allDataNA23 <- allDataNA %>% 
    filter(transcript_year == 2023) %>% 
    left_join(ru23)

allDataNA <- rbind(allDataNA03, allDataNA13, allDataNA23)

## Algara-Sharif election data
## 2008 Algara-Sharif data merges with 2010-2011 LV Data
## 2012 Algara-Sharif data merges with 2012-2015 LV Data
## 2016 Algara-Sharif data merges with 2016-2019 LV Data
## 2020 Algara-Sharif data merges with 2020-2023 LV Data
allDataNA08 <- allDataNA %>% 
    filter(transcript_year < 2012) %>% 
    left_join(algara2008)
allDataNA12 <- allDataNA %>% 
    filter(transcript_year >= 2012 & transcript_year <= 2015) %>% 
    left_join(algara2012)
allDataNA16 <- allDataNA %>% 
    filter(transcript_year >= 2016 & transcript_year <= 2019) %>% 
    left_join(algara2016)
allDataNA20 <- allDataNA %>% 
    filter(transcript_year >= 2020) %>% 
    left_join(algara2020)

allDataNA <- rbind(allDataNA08, allDataNA12, allDataNA16, allDataNA20)

## CVI is only available for 2024
## allDataNA2024 is the data with CVI only merged with 2024
## allDataNA is the data with CVI data merged for every year 
## (i.e., 2024 CVI data merges with allprevious years)
allDataNA24 <- allDataNA %>% 
    filter(transcript_year == 2023)
allDataNAcvi <- allDataNA24 %>% 
    left_join(cvi)

allDataNA <- allDataNA %>%
    left_join(cvi, relationship = "many-to-one") %>% 
    select(state_name, state_fips, county_name, county_fips, stcounty_fips, transcript_year, n_meettype, total_scriptCY, sum_scriptCC, prop_cc, n_places_incounty, election_year, DVP, RVP, ACSyear, medage, totalpop, medgrossrent, medhhic, perc_white, perc_black, perc_hispanic, perc_other, edu_percentPop, femaYear, n_decInYear, n_decTypeYear, overall_CVI, baseline_all, baseline_health, baseline_socioEcon, baseline_infrastructure, baseline_environ, climate_all, climate_health, climate_socioEcon, climate_extreme)

### save county year ###########################################################
##                                                                            ##
##                  save data with and without county boundaries              ##
##                                                                            ##
################################################################################

# write.csv(allDataNA, "./LocalView/data/modified/LVCounty.csv", row.names = FALSE)

## separate by transcript year and write geopackage
# for(y in unique(allDataNA$transcript_year)){
#     a <- allDataNA %>%
#         filter(transcript_year == y)
# 
#     if (y < 2015) {
#         a_merged <- a %>%
#             right_join(counties10Geo)
#     }
#     else if (y >= 2015 & y <= 2019){
#         a_merged <- a %>%
#             right_join(counties15Geo)
#     }
#     else{
#         a_merged <- a %>%
#             right_join(counties20Geo)
#     }
#     st_write(a_merged, paste0("./GIS/modified/geopackages/TranscriptYears/allDataNA_",y, ".gpkg"))
# }

## separate by election year and write geopackage
# for(y in unique(allDataNA$election_year)){
#     a <- allDataNA %>%
#         filter(election_year == y)
# 
#     if (y == 2008) {
#         a_merged <- a %>%
#             right_join(counties00Geo)
#     }
#     else if (y == 2012){
#         a_merged <- a %>%
#             right_join(counties10Geo)
#     }
#     else if (y == 2016){
#         a_merged <- a %>%
#             right_join(counties15Geo)
#     }
#     else{
#         a_merged <- a %>%
#             right_join(counties20Geo)
#     }
#     st_write(a_merged, paste0("./GIS/modified/geopackages/ElectionYears/allDataNA_",y, "Election.gpkg"))
# }

### merges state ###############################################################
##                                                                            ##
##                          merge all data  - state year                      ##
##                                                                            ##
################################################################################

# allData_state <- allDataNA %>%
#     group_by(transcript_year, state_name) %>%
#     mutate(stcounty_fips = str_pad(stcounty_fips, 5, "left", 0),
#            state_totalScript = sum(total_scriptCY, na.rm = TRUE),   # total number of transcripts
#            state_cc = sum(sum_scriptCC, na.rm=TRUE),                # sum of transcripts with CC mention
#            state_propCC = ifelse(state_cc == 0, 0,
#                     round((state_cc/state_totalScript)*100, 2)),  # state proportion of CC mentions
#            total_vp = sum(DVP) + sum(RVP),                          # state total vote percentage
#            state_dvp = round((sum(DVP)/total_vp)*100, 2),           # state dvp
#            state_rvp = round((sum(RVP)/total_vp)*100, 2),           # state rvp
#            n_county = n_distinct(county_name),
#            state_pop = sum(totalpop),
#            state_medage = sum(medage)/n_county,
#            state_medgrossrent = (sum(medgrossrent/n_county, na.rm=TRUE)),
#            state_medhhic = sum(medhhic)/n_county,
#            state_percHispanic = sum(perc_hispanic)/n_county,
#            state_percOther = sum(perc_other)/n_county,
#            state_percWhite = sum(perc_white)/n_county,
#            state_percBlack = sum(perc_black)/n_county,
#            state_overallCVI = sum(overall_CVI)/n_county,
#            state_baselineAll = sum(baseline_all)/n_county,
#            state_baselineHealth = sum(baseline_health)/n_county,
#            state_baselineSocioEcon = sum(baseline_socioEcon)/n_county,
#            state_baselineInfrastucture = sum(baseline_infrastructure)/n_county,
#            state_baselineEviron = sum(baseline_environ)/n_county,
#            state_climateAll = sum(climate_all)/n_county,
#            state_climateSocioEcon = sum(climate_socioEcon)/n_county,
#            state_climateExtreme = sum(climate_extreme)/n_county) %>%
#     ungroup() %>%
#     distinct(transcript_year, state_name, .keep_all = TRUE) %>%
#     select(state_name, n_places_incounty, starts_with("state_"))

# write.csv(allData_state, "./LocalView/data/modified/LVState.csv")

allData_stateGeo <- allData_state %>%
    left_join(statesGeo, by = "state_name")

# st_write(allData_stateGeo, "./GIS/modified/geopackages/lvStates.gpkg")
















































