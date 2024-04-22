## this was the original processing before the Algara-Sharif dataset was found
######################### FILE DESCRIPTION #####################################
##                                                                            ##
## This file handles the preliminary steps necessary for                      ##  
##      the political data                                                    ##
##                                                                            ##  
## Geographic boundaries are downloaded tigris unless otherwise specified.    ##
##                                                                            ##  
## Presidential election results (2016-2020) were downloaded                  ##
##      from MIT.                                                             ##
##      https://electionlab.mit.edu/data                                      ##
##                                                                            ##
## Alaska Election District boundaries were downloaded from Alaska's          ##
##      2020 May Interim Proclamation website. Previous year boundaries are   ##
##      only available in .pdf format.                                        ##
##      https://www.akredistrict.org/2022-may-interim-proclamation/           ##
##                                                                            ##
## 2020 election data was downloaded from The Upshot's github and converted   ##
##      to .csv using QGIS                                                    ##
##      https://github.com/TheUpshot/presidential-precinct-map-2020           ##
##                                                                            ##
## 2020 election data for Kentucky was downloaded from their election website ##
##      https://elect.ky.gov/results/2020-2029/Pages/2020.aspx                ##
##                                                                            ##
## 2020 election data for Virgina was downloaded from their election website  ##
##      https://historical.elections.virginia.gov/elections/view/144567/      ##
##                                                                            ##
## Arizona was downloaded from, total votes from azsos                        ##
##      https://www.politico.com/2020-election/results/arizona/               ##
##      https://apps.azsos.gov/election/2020/2020_resultssummary_0.xml        ##
##                                                                            ##
## Arkansas was downloaded from:                                              ##
##      https://results.enr.clarityelections.com/AR/106124/web.274956/#/detail/100 
## Georgia
##      https://results.enr.clarityelections.com/GA/105369/web.264614/#/detail/5000
##                                                                            ##
## https://www.kaggle.com/datasets/unanimad/us-election-2020?resource=download&select=president_county.csv
## Due to data limitations, several sources were combined to make a complete  ##
##      county election return map for 2020.                                  ##
##      Both MIT & Upshot were missing several counties that have been        ##
##      manually collected and added in                                       ##
##      FIPS: 05107 (Phillips County, AR), 05069 (Jefferson County, AR),      ##      
##          https://results.enr.clarityelections.com/AR/106124/web.274956/#/summary
##      15005 (Kalawao County, HI), per Hawaii elections website, Kalawao     ##
##          county is included in Maui county as the former has ~90 residents ##
##          https://elections.hawaii.gov/wp-content/uploads/2015/03/hava_stateplan.pdf           
##      19163 (Scott County, IA),                                             ##    
##          https://www3.scottcountyiowa.gov/auditor/pub/election_returns/2020/20201103_General_Election/20201103_General_Election_Summary_Official_Results.pdf 
##      40143 (Tulsa County, OK), 40109 (Oklahoma County, OK)                 ##
##          https://results.okelections.us                                    ##
##      37185 (Warren County, SC)                                             ##
##          https://www.ncsbe.gov/results-data/election-results/historical-election-results-data#by-precinct
##                                                                            ##                                               
## The 2008  & 212 election data is missing Shannon County, SD 46102          ##
##      it's added in manually in lines 98 & 113                              ##
##      ** note: Shannon County was renamed to Oglala Lakota in 2014**        ##
##                                                                            ##
################################################################################



mit20 <- mit  %>% 
  filter(year == 2020)  %>% 
  select(-county_name)  %>% 
  add_row(
    year = 2020,
    state_fips = "37",
    state_name = "North Carolina",
    state_abbr = "NC", 
    county_fips = "185",
    DEM = 184764,
    REP = 84776,
    total_votes = 279244,
    DVP = 93.38,
    RVP = 5.977,
    full_fips = "37185")

upshot20 <- read_sf("./Political/PoliticalOriginalData/2020_PL_UpshotGeneral.geojson")  %>% 
  st_drop_geometry()  %>% 
  mutate(full_fips = str_sub(GEOID, start = 1, end = 5),
         county_fips = str_sub(full_fips, start = 1, end = 3),
         state_fips = str_sub(full_fips, start = 1, end = 2))  %>% 
  filter(state_fips < 56 & state_fips != "02" & state_fips != "04" & state_fips != "05" & state_fips != "13")  %>% 
  select(-GEOID)      %>% 
  group_by(county_fips)  %>% 
  mutate(DEM = sum(votes_dem),
         GOP = sum(votes_rep),
         TOTAL = sum(votes_total))  %>% 
  ungroup()   %>% 
  distinct(county_fips, state_fips, .keep_all = TRUE)  %>% 
  mutate(DVP = (DEM/TOTAL)*100,
         RVP = (GOP/TOTAL)*100,
         year = "2020")  %>% 
  rename(REP = GOP,
         total_votes = votes_total)  %>% 
  select(year, full_fips,county_fips, state_fips, DEM, REP, DVP, RVP, total_votes)  %>% 
  left_join(states, by = "state_fips")

kaggle20 <- read.csv("./Political/PoliticalOriginalData/2020_US_KaggleGeneral.csv")

kgeo20 <- left_join(counties10, kaggle20, by = c("county_name" = "county")) %>% 
  select(-candidate, - state, - won) %>% 
  st_drop_geometry()



ar20 <- read.csv("./Political/PoliticalModifiedData/2020_AR_General.csv")  %>%
  rename(state_fips = state_fips)  %>% 
  mutate(county_fips = str_pad(county_fips, 3, side = "left", 0),
         state_fips = str_pad(state_fips, 2, side = "left", 0),
         full_fips = paste(state_fips, county_fips, sep=""))

az20 <- read.csv("./Political/PoliticalOriginalData/2020_AZ_General.csv") %>%
  rename(state_fips = state_fips)  %>% 
  mutate(county_fips = str_pad(county_fips, 3, side = "left", 0),
         state_fips = str_pad(state_fips, 2, side = "left", 0),
         full_fips = paste(state_fips, county_fips, sep=""))

ga20 <- read.csv("./Political/PoliticalModifiedData/2020_GA_General.csv")  %>% 
  mutate(county_fips = str_sub(full_fips, start = 1, end = 3),
         state_fips = str_sub(full_fips, start = 1, end = 2))

azar <- rbind(ar20, az20)  %>% 
  select(full_fips, state_fips, county_fips, county_name, DEM, REP, total_votes, DVP, RVP)  %>% 
  mutate(year = "2020")

azarga <- rbind(azar, ga20)    

va20 <- read.csv("./Political/PoliticalModifiedData/2020_PL_VAPresidential.csv") %>% 
  select(-OTHER)  %>% 
  mutate(DVP = (DEM/total_votes)*100,
         RVP = (REP/total_votes)*100) 

ky20 <- read.csv("./Political/PoliticalModifiedData/2020_County_KYPresidential.csv") %>% 
  select(-OTHER)  %>% 
  mutate(DVP = (DEM/total_votes)*100,
         RVP = (REP/total_votes)*100) 

m20 <- read.csv("./Political/PoliticalModifiedData/2020_County_MITUpshotMissingCounties.csv")  %>% 
  mutate(full_fips = str_pad(full_fips, 5, side="left", pad="0"),
         state_fips = as.character(state_fips),
         state_fips = str_pad(state_fips, 2, side="left", pad=0),
         county_fips = str_pad(county_fips, 3, side="left", pad=0))

vaky <- rbind(va20, ky20)  %>% 
  mutate(county_fips = str_pad(county_fips, 3, side = "left", 0),
         full_fips = paste(state_fips, county_fips, sep = ""),
         state_fips = as.character(state_fips))

vakyazarga <- rbind(vaky, azarga)  

missing <- rbind(vakyazarga, m20)  %>% 
  left_join(states, by = "state_fips")  %>% 
  select(-county_name)

e20 <- rbind(mit20, upshot20)
all20 <- rbind(e20, missing)  %>% 
  select(year, state_abbr, full_fips, total_votes, DEM, REP, DVP, RVP)



mit <- read.csv("./Political/PoliticalOriginalData/2020_MITGeneral.csv")  %>% 
  mutate(full_fips = as.character(full_fips),
         full_fips = str_pad(full_fips, 5, side = "left", pad = 0),
         state_fips = str_sub(full_fips, start = 1, end = 2),
         county_fips = str_sub(full_fips, start = 3, end = 5))  %>% 
  filter(state_fips != "02" & mode == "TOTAL" & year >= 2008)  %>% 
  select(-candidate, -version, -mode)  %>% 
  pivot_wider(names_from = party,
              values_from = candidatevotes,
              values_fill = 0) %>% 
  rename(DEM = DEMOCRAT,
         REP = REPUBLICAN)  %>% 
  mutate(DVP = (DEM/total_votes)*100,
         RVP = (REP/total_votes)*100,
         state_name = str_to_sentence(state_name))  %>% 
  select(-OTHER, -GREEN, -LIBERTARIAN, -office) %>% 
  add_row(year = 2012,
          state_fips = "46",
          county_fips = "102",
          county_name = "Oglala Lakota County",
          DEM = 2937,
          REP = 188,
          state_name = "South Dakota",
          state_abbr = "SD",
          total_votes = 3145,
          DVP = 93.38,
          RVP = 5.977,
          full_fips = "46102") %>%
  add_row(year = 2008,
          state_fips = "46",
          county_fips = "102",
          county_name = "Oglala Lakota County",
          DEM = 2971,
          REP = 331,
          total_votes = 3350,
          state_abbr = "SD",
          state_name = "South Dakota",
          DVP = 88.68,
          RVP = 9.88,
          full_fips = "46102") 
