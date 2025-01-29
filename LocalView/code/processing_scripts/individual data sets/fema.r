### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the FEMA disaster     ##
##      declarations                                                          ##
##      Data included:                                                        ##
##          FEMA (2010-2023)                                                  ##
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
##                                                                            ##
## Output:                                                                    ##
##      /LocalView/data/modified/fema_countyYear_withNA.rdata                 ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load libraries and custom functions                                     ####

source("./LocalView/code/processing_scripts/analysis_prelims.r")
source("./LocalView/code/processing_scripts/individual data sets/geography.r")

#   ____________________________________________________________________________
#   FEMA disaster declarations                                              ####

fema_declarationLevel <- read.csv("./LocalView/data/original/2023_FEMA.csv") %>%
  rename(
    fema_id = id,
    state_fips = fipsStateCode,
    county_fips = fipsCountyCode,
    fema_declarationType = declarationType,
    fema_incidentType = incidentType
  ) %>%
  separate(declarationDate,
    into = c("fema_year", "fema_month", "date"),
    sep = "-"
  ) %>%
  mutate(fema_day = str_sub(date, 0, 2)) %>%
  filter(
    fema_year >= 2004 & fema_year < 2023, 
    fema_incidentType == "Coastal Storm" |
    fema_incidentType == "Fire" |
    fema_incidentType == "Flood" |
    fema_incidentType == "Freezing" |
    fema_incidentType == "Hurricane" |
    fema_incidentType == "Mud/Landslide" |
    fema_incidentType == "Severe Ice Storm" |
    fema_incidentType == "Severe Storm" |
    fema_incidentType == "Snowstorm" |
    fema_incidentType == "Tornado" |
    fema_incidentType == "Typhoon" |
    fema_incidentType == "Winter Storm" |
    fema_incidentType == "Tropical Storm" |
    fema_incidentType == "Tsunami"
  ) %>%
  padFips() %>%
  createFips() %>%
  group_by(fema_year, stcounty_fips) %>%
  mutate(
    fema_nDecCountyYear = n(),
    fema_nDecTypeCountyYear = n_distinct(fema_declarationType)
  ) %>%
  ungroup() %>%
  select(
    fema_id, fema_year, fema_month, fema_day, fema_declarationType,
    fema_incidentType, state_fips, county_fips, stcounty_fips,
    fema_nDecCountyYear, fema_nDecTypeCountyYear
  ) %>%
  excludeStates()

# save(fema_declarationLevel, file = "./LocalView/data/modified/individual_datasets/fema_declarationLevel.rdata")

##  ............................................................................
# create county-year level fema data                                        ####

## loop through fema and merge it with county data
f_merged <- list()
for (y in unique(fema_declarationLevel$fema_year)) {
  f <- fema_declarationLevel %>%
    distinct(stcounty_fips, .keep_all = TRUE) %>%
    filter(fema_year == y)

  if (y == 2009) {
    f_merged[[y]] <- f %>%
      select(-state_fips, -county_fips) %>%
      right_join(counties, by = "stcounty_fips") %>%
      mutate(
        fema_year = as.numeric(y),
        transcript_year = fema_year + 1,
        fema_decBinary = ifelse(is.na(fema_nDecCountyYear), 0, 1)
      )
  } else if (y >= 2010 & y <= 2014) {
    f_merged[[y]] <- f %>%
      select(-state_fips, -county_fips) %>%
      right_join(counties, by = "stcounty_fips") %>%
      mutate(
        fema_year = as.numeric(y),
        transcript_year = fema_year + 1,
        fema_decBinary = ifelse(is.na(fema_nDecCountyYear), 0, 1)
      )
  } else if (y >= 2015 & y <= 2019) {
    f_merged[[y]] <- f %>%
      select(-state_fips, -county_fips) %>%
      right_join(counties, by = "stcounty_fips") %>%
      mutate(
        fema_year = as.numeric(y),
        transcript_year = fema_year + 1,
        fema_decBinary = ifelse(is.na(fema_nDecCountyYear), 0, 1)
      )
  } else {
    f_merged[[y]] <- f %>%
      select(-state_fips, -county_fips) %>%
      right_join(counties, by = "stcounty_fips") %>%
      mutate(
        fema_year = as.numeric(y),
        transcript_year = fema_year + 1,
        fema_decBinary = ifelse(is.na(fema_nDecCountyYear), 0, 1)
      )
  }
}

## n = 43,512
fema_countyLevel <- bind_rows(f_merged) %>%
  mutate(transcript_year = as.character(transcript_year),
         fema_nDecTypeCountyYear = ifelse(is.na(fema_nDecTypeCountyYear), 0, fema_nDecTypeCountyYear),
         fema_nDecCountyYear = ifelse(is.na(fema_nDecCountyYear), 0, fema_nDecCountyYear)) %>%
  select(
    stcounty_fips, fema_year, transcript_year, fema_nDecCountyYear,
    fema_nDecTypeCountyYear, fema_decBinary
  )

# save(fema_countyLevel, file = "./LocalView/data/modified/individual_datasets/fema_countyLevel.rdata")

##  ............................................................................
##  FEMA Sample                                                             ####

sample_femaDecLevel <- fema_declarationLevel %>% sample_n(50)
# write.csv(sample_femaDecLevel, "./LocalView/data/samples/femaDecLevel_sample.csv", row.names = FALSE)

sample_femaCountyLevel <- fema_countyLevel %>% sample_n(50)
# write.csv(sample_femaCountyLevel, "./LocalView/data/samples/femaCountyLevel_sample.csv", row.names = FALSE)

