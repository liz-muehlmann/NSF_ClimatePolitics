
# election data ################################################################
mit <- read.csv("./Political/PoliticalOriginalData/2000_2020_County_Presidential.csv") %>%     
      select(-candidate)  %>% 
      filter(mode == "TOTAL" & year >= 2008)  %>% 
      pivot_wider(
        names_from = party,
        values_from = candidatevotes,
        values_fill = 0)  %>% 
      rename(full_fips = county_fips,
             DEM = DEMOCRAT,
             REP = REPUBLICAN,
             total_votes = totalvotes)  %>% 
      mutate(full_fips = as.character(full_fips),
             full_fips = str_pad(full_fips, 5, side = "left", pad = 0),
             DVP = (DEM/total_votes)*100,
                    RVP = (REP/total_votes)*100,
             state_fips = str_sub(full_fips, start = 1, end = 2),
             county_fips = str_sub(full_fips, start = 3, end = 5))  %>% 
      select(year, state_fips, county_fips, county_name, DEM, REP, total_votes, DVP, RVP, full_fips)  %>% 
      filter(state_fips != "02")
             
mit08 <- mit  %>% 
    filter(year == 2008)  %>% 
    add_row(year = 2008,
            state_fips = "46",
            county_fips = "102",
            county_name = "Shannon County",
            DEM = 2971,
            REP = 331,
            total_votes = 3350,
            DVP = 88.68,
            RVP = 9.88,
            full_fips = "46102")

mit1216 <- mit  %>% 
    filter(year == 2012 | year == 2016)  %>% 
    add_row(year = 2012,
            state_fips = "46",
            county_fips = "102",
            county_name = "Shannon County",
            DEM = 2937,
            REP = 188,
            total_votes = 3145,
            DVP = 93.38,
            RVP = 5.977,
            full_fips = "46102")

mit20 <- mit  %>% 
    filter(year == 2020)

upshot20 <- read.csv("./Political/PoliticalOriginalData/2020_PL_UpshotGeneral.csv")   %>% 
        mutate(county_fips = str_sub(GEOID, start = 1, end = 5))  %>% 
        mutate(state_fips = str_sub(county_fips, start = 1, end = 2))  %>% 
        group_by(county_fips)  %>% 
        mutate(DEM = sum(votes_dem),
               GOP = sum(votes_rep),
               TOTAL = sum(votes_total))  %>% 
        ungroup()   %>% 
        distinct(county_fips, state_fips, .keep_all = TRUE)  %>% 
        mutate(DVP = (DEM/TOTAL)*100,
               RVP = (GOP/TOTAL)*100)  %>% 
        rename(REP = GOP,
               total_votes = votes_total)  %>% 
        select(county_fips, state_fips, DEM, REP, DVP, RVP, total_votes)
upshot20$year <- "2020"

va20 <- read.csv("./Political/PoliticalModifiedData/2020_PL_VAPresidential.csv") %>% 
      select(-OTHER)  %>% 
      mutate(DVP = (DEM/total_votes)*100,
             RVP = (REP/total_votes)*100) 
             
ky20 <- read.csv("./Political/PoliticalModifiedData/2020_County_KYPresidential.csv") %>% 
      select(-OTHER)  %>% 
      mutate(DVP = (DEM/total_votes)*100,
             RVP = (REP/total_votes)*100) 

mumissing20 <- read.csv("./Political/PoliticalModifiedData/2020_County_MITUpshotMissingCounties.csv")  %>% 
    mutate(full_fips = str_pad(full_fips, 5, side="left", pad="0"))  %>% 
    mutate(DVP = DVP*100,
           RVP = RVP*100)


## va & ky #####################################################################
evaky20 <- rbind(va20, ky20)  %>% 
    mutate(county_fips = str_pad(county_fips, 3, side="left", "0"),
           full_fips = paste(state_fips, county_fips, sep=""),
           state_fips = as.character(state_fips)) 


## va, ky + mit ################################################################
vakymit <- rbind(evaky20, mit20) 
vakymit <- rbind(vakymit, mumissing20)

## geometry ####################################################################
cupshot20 <- left_join(upshot20, counties10, by = c("county_fips" = "GEOID"))  %>% 
    select(year, STATEFP, COUNTYFP, NAMELSAD, DEM, REP, DVP, RVP, total_votes, geometry, county_fips)

cvakymit20 <- left_join(vakymit, counties10, by = c("full_fips" = "GEOID"))  %>% 
    select(year, STATEFP, COUNTYFP, NAMELSAD, DEM, REP, DVP, RVP, total_votes, geometry, full_fips) 

ndup_cvakymit20 <- cupshot20  %>% filter(!(county_fips %in% cvakymit20$full_fips))

noak20 <- rbind(cupshot20, cvakymit20)  %>% 
    filter(STATEFP != "02")  %>% 
    mutate(full_fips = paste(STATEFP, COUNTYFP, sep=""))  

## mit geo #####################################################################
mitgeo08 <- left_join(mit08, counties10, by = c("full_fips" = "GEOID"))  %>% 
    select(year, STATEFP, COUNTYFP, NAMELSAD, DEM, REP, DVP, RVP, total_votes, geometry)  %>% 
    filter(STATEFP != "02")  %>% 
    mutate(full_fips = paste(STATEFP, COUNTYFP, sep=""))

mitgeo1216 <- left_join(mit1216, counties10, by = c("full_fips" = "GEOID"))  %>% 
    select(year, STATEFP, COUNTYFP, NAMELSAD, DEM, REP, DVP, RVP, total_votes, geometry)  %>% 
    filter(STATEFP != "02") %>% 
    mutate(full_fips = paste(STATEFP, COUNTYFP, sep=""))

## mit + ak ####################################################################
us08 <- rbind(akgeo08, mitgeo08)   %>% 
    left_join(states, by = "STATEFP")
us1216 <- rbind(akgeo1216, mitgeo1216) %>% 
    left_join(states, by = "STATEFP")
us20 <- rbind(akgeo20, noak20) %>% 
    left_join(states, by = "STATEFP")


## write shapefile #############################################################
# st_write(us08, "./Political/PoliticalModifiedData/Geopackages/us08_election.gpkg")
# st_write(us1216, "./Political/PoliticalModifiedData/Geopackages/us1216_election.gpkg")
# st_write(us20, "./Political/PoliticalModifiedData/Geopackages/us20_election.gpkg")
