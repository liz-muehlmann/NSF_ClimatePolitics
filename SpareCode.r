## separate by whether place splits over multiple counties #########################################
pc1_2010 <- oneCounty(places2010)  %>% 
    left_join(eacs0816, by = c("state_fips", "county1fips" = "county_fips"))
nrow(pc1_2010) # 39,723
pc2_2010 <- twoCounties(places2010) %>% 
    left_join(eacs0816, by = c("state_fips", "county2fips" = "county_fips"))
nrow(pc2_2010) # 1,224
pc3_2010 <- threeCounties(places2010) %>% 
    left_join(eacs0816, by = c("state_fips", "county3fips" = "county_fips"))
nrow(pc3_2010) # 95
pc4_2010 <- fourCounties(places2010) %>% 
    left_join(eacs0816, by = c("state_fips", "county4fips" = "county_fips"))
nrow(pc4_2010) # 14
pc5_2010 <- fiveCounties(places2010) %>% 
    left_join(eacs0816, by = c("state_fips", "county5fips" = "county_fips"))
nrow(pc5_2010) # 2

pc1_2020 <- oneCounty(places2020) %>% 
    left_join(eacs20 , by = c("state_fips", "county1fips" = "county_fips"))
nrow(pc1_2020) # 30,539
pc2_2020 <- twoCounties(places2020) %>% 
    left_join(eacs20 , by = c("state_fips", "county2fips" = "county_fips"))
nrow(pc2_2020) # 1,118
pc3_2020 <- threeCounties(places2020) %>% 
    left_join(eacs20 , by = c("state_fips", "county3fips" = "county_fips"))
nrow(pc3_2010) # 95
pc4_2020 <- fourCounties(places2020) %>% 
    left_join(eacs20 , by = c("state_fips", "county4fips" = "county_fips"))
nrow(pc4_2020) # 15
pc5_2020 <- fiveCounties(places2020)  %>% 
    left_join(eacs20, by = c("state_fips", "county5fips" = "county_fips"))
nrow(pc5_2020) # 3

peacs10 <- left_join(pc1_2010, pc2_2010)


## election & acs ##################################################################################
eacs0812 <- left_join(e0812, acs2010) %>% 
    mutate(state_fips = str_sub(full_fips, start = 1, end = 2),
           county_fips = str_sub(full_fips, start = 3, end = 5)) %>% 
    select(-full_fips)

eacs16 <- left_join(e16, acs2015) %>% 
    mutate(state_fips = str_sub(full_fips, start = 1, end = 2),
           county_fips = str_sub(full_fips, start = 3, end = 5)) %>% 
    select(-full_fips)

eacs20 <- left_join(e20, acs2020)%>% 
    mutate(state_fips = str_sub(full_fips, start = 1, end = 2),
           county_fips = str_sub(full_fips, start = 3, end = 5)) %>% 
    select(-full_fips)

eacs0816 <- left_join(eacs0812, eacs16, by = c("state_fips", "county_fips"))

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
        select(-NAME) %>% 
        filter(full_fips < 56999)
    assign(paste("acs", year, sep=""), df)
}
