
# process counties function ####################################################
processCounties <- function(df){
    df  %>% 
    select(STATEFP10, COUNTYFP10, NAMELSAD10, GEOID10, INTPTLAT10, INTPTLON10, geometry)  %>%
    mutate(STATEFP10 = str_pad(STATEFP10, width = 2, side = "left", pad ="0"),
           GEOID10 = str_pad(GEOID10, width = 5, side = "left", pad ="0"))  %>%  
    filter(STATEFP10 < 57) 
}

# merge places with county fips ################################################
mergePC <- function(df1, df2) {
        left_join(df1, df2, by = c("STATEFP10", "county1"="NAMELSAD10"))  %>% 
        rename(county1fips = COUNTYFP10)  %>% 
        left_join(df2, by = c("STATEFP10", "county2"= "NAMELSAD10"))  %>%
        rename(county2fips = COUNTYFP10)  %>% 
        left_join(df2, by = c("STATEFP10", "county3"= "NAMELSAD10"))  %>% 
        rename(county3fips = COUNTYFP10)  %>% 
        left_join(df2, by = c("STATEFP10", "county4"= "NAMELSAD10"))  %>% 
        rename(county4fips = COUNTYFP10)  %>% 
        left_join(df2, by = c("STATEFP10", "county5"= "NAMELSAD10"))  %>% 
        rename(county5fips = COUNTYFP10)
}

oneCounty <- function(df){
        df  %>% filter_at(vars(county2fips, county3fips, county4fips, county5fips),all_vars(is.na(.)))  %>% 
        mutate(c1Full = paste(STATEFP10, county1fips, sep=""))  %>% 
        group_by(c1Full)  %>% 
        select(-county2, -county3, -county4, -county5, -county2fips, -county3fips, -county4fips, -county5fips)
}

twoCounties <- function(df){
        df  %>% 
        filter(!is.na(county2fips))  %>% 
              mutate(c2Full = paste(STATEFP10, county2fips, sep=""))  %>%  
              filter(!is.na(county1fips) & 
                    is.na(county3fips) &
                    is.na(county4fips) &
                    is.na(county5fips))  %>% 
              group_by(c2Full)  %>% 
              select(-county1, -county3, -county4, -county5, -county1fips, -county3fips, -county4fips, -county5fips)
}

threeCounties <- function(df){
        df  %>% 
        filter(!is.na(county3fips))  %>% 
            mutate(c3Full = paste(STATEFP10, county3fips, sep=""))  %>%  
            filter(!is.na(county1fips) & 
                    !is.na(county2fips) &
                    is.na(county4fips) &
                    is.na(county5fips))  %>% 
            group_by(c3Full)  %>% 
            select(-county2, -county1, -county4, -county5, -county2fips, -county1fips, -county4fips, -county5fips)
}

fourCounties <- function(df){
        df  %>% 
        filter(!is.na(county4fips))  %>% 
        mutate(c4Full = paste(STATEFP10, county4fips, sep=""))  %>%  
        filter(!is.na(county1fips) & 
                !is.na(county2fips) &
                !is.na(county4fips) &
                is.na(county5fips))  %>% 
        group_by(c4Full)  %>% 
        select(-county2, -county3, -county1, -county5, -county2fips, -county3fips, -county1fips, -county5fips)
}

fiveCounties <- function(df) {
            df  %>% 
            filter(!is.na(county5fips))  %>% 
            mutate(c5Full = paste(STATEFP10, county5fips, sep=""))  %>%  
            filter_at(vars(county2fips, county3fips, county4fips, county5fips),all_vars(!is.na(.)))  %>%  
            group_by(c5Full)  %>% 
            select(-county2, -county3, -county4, -county1, -county2fips, -county3fips, -county4fips, -county1fips)
}

e2008 <- function(df) {
   df  %>%  
   filter(year >=2010 & year <= 2012) 
}

e2012 <- function(df) {
    df  %>%      
    filter(year >=2013 & year <= 2016)
}
e2016 <- function(df) {
    df  %>%  
    filter(year >=2017 & year <= 2020)
}
e2020 <- function(df) {
    df  %>%  
    filter(year >=2021 & year >= 2024)
}


# suppress warnings & increase java heap space #################################
options(warn = -1)


## process places function #####################################################
processPlaces <- function(df){
    df  %>% 
    mutate(state_fips = str_pad(state_fips, 2, side = "left", 0),
           county2 = str_trim(county2),
           county3 = str_trim(county3),
           county4 = str_trim(county4),
           county5 = str_trim(county5), 
           county1fips = str_pad(county1fips, 3, side = "left", 0),
           county2fips = str_pad(county2fips, 3, side = "left", 0),
           county3fips = str_pad(county3fips, 3, side = "left", 0),
           county4fips = str_pad(county4fips, 3, side = "left", 0),
           county5fips = str_pad(county5fips, 3, side = "left", 0))
}

processElections <- function(df){
    df  %>% 
    st_drop_geometry()  %>% 
    rename(state_fips = STATEFP,
            county_fips = COUNTYFP,
            county_name = NAMELSAD,
            state_abbr = STUSPS,
            state_name = NAME) 
}

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
    separate(NAME, sep = ",", c("county_name", "state_name"))  %>% 
    mutate(state_fips = str_sub(GEOID, start = 1, end = 2),
            county_fips = str_sub(GEOID, start = 3, end = 5),
            state_name = trimws(state_name))  %>% 
    rename(full_fips = GEOID) 
}

mergePC <- function(df1, df2) {
        left_join(df1, df2, by = c("state_fips", "county1"="county_name"))  %>% 
        rename(county1fips = county_fips)  %>% 
        left_join(df2, by = c("state_fips", "county2"= "county_name"))  %>%
        rename(county2fips = county_fips)  %>% 
        left_join(df2, by = c("state_fips", "county3"= "county_name"))  %>% 
        rename(county3fips = county_fips)  %>% 
        left_join(df2, by = c("state_fips", "county4"= "county_name"))  %>% 
        rename(county4fips = county_fips)  %>% 
        left_join(df2, by = c("state_fips", "county5"= "county_name"))  %>% 
        rename(county5fips = county_fips)
}

oneCounty <- function(df){
        df  %>% filter_at(vars(county2fips, county3fips, county4fips, county5fips),all_vars(is.na(.)))  %>% 
        mutate(c1Full = paste(state_fips, county1fips, sep=""))  %>% 
        group_by(c1Full)  %>% 
        select(-county2, -county3, -county4, -county5, -county2fips, -county3fips, -county4fips, -county5fips)
}

twoCounties <- function(df){
        df  %>% 
        filter(!is.na(county2fips))  %>% 
              mutate(c2Full = paste(state_fips, county2fips, sep=""))  %>%  
              filter(!is.na(county1fips) & 
                    is.na(county3fips) &
                    is.na(county4fips) &
                    is.na(county5fips))  %>% 
              group_by(c2Full)  %>% 
              select(-county1, -county3, -county4, -county5, -county1fips, -county3fips, -county4fips, -county5fips)
}

threeCounties <- function(df){
        df  %>% 
        filter(!is.na(county3fips))  %>% 
            mutate(c3Full = paste(state_fips, county3fips, sep=""))  %>%  
            filter(!is.na(county1fips) & 
                    !is.na(county2fips) &
                    is.na(county4fips) &
                    is.na(county5fips))  %>% 
            group_by(c3Full)  %>% 
            select(-county2, -county1, -county4, -county5, -county2fips, -county1fips, -county4fips, -county5fips)
}

fourCounties <- function(df){
        df  %>% 
        filter(!is.na(county4fips))  %>% 
        mutate(c4Full = paste(state_fips, county4fips, sep=""))  %>%  
        filter(!is.na(county1fips) & 
                !is.na(county2fips) &
                !is.na(county4fips) &
                is.na(county5fips))  %>% 
        group_by(c4Full)  %>% 
        select(-county2, -county3, -county1, -county5, -county2fips, -county3fips, -county1fips, -county5fips)
}

fiveCounties <- function(df) {
            df  %>% 
            filter(!is.na(county5fips))  %>% 
            mutate(c5Full = paste(state_fips, county5fips, sep=""))  %>%  
            filter_at(vars(county2fips, county3fips, county4fips, county5fips),all_vars(!is.na(.)))  %>%  
            group_by(c5Full)  %>% 
            select(-county2, -county3, -county4, -county1, -county2fips, -county3fips, -county4fips, -county1fips)
}




## 

















