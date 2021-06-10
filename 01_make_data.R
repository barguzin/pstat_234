library(tidycensus) 
library(tidycovid19)
library(tidyverse)
library(tigris)
library(sf)
library(classInt)
#library(RcppMsgPack)
#library(arrow)
tigris_use_cache = TRUE


# load the census data
census_api_key('41ce0e70602654543dd381afc56307c160b386d2', install=TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

usa <- get_acs(geography = "county", 
              geometry = TRUE, 
              variables = c(
                medincome = "B19013_001",
                total_pop = "B01003_001", 
                black = "B02001_003", 
                amer_indian = "B02014_001", 
                hispanic = "B03001_001", 
                public_transit = "B08101_025", 
                med_earnings = "B08121_001",
                hh_4_more = "B08202_018", 
                pop_under_18 = "B09001_001", 
                family_hh = "B11001_002", 
                hs_diploma = "B15003_017", 
                bachelor = "B15003_022", 
                poverty = "B16009_002", 
                no_insurance = "B27010_066", 
                per_capita_income = "B19301_001", 
                renter_occupied = "B25003_003", 
                median_value = "B25107_001"
              ), 
              #state = "CA", 
              year = 2019)

head(usa)

usa %>% 
  select(GEOID) %>% 
  n_distinct()

#write.csv(usa, 'data/usa.csv')
# this interface uses apache arrow which did not install properly in R on Windows
#write_feather(usa, 'data/usa.feather') 


# DO NOT RUN !!!!
# read and subset mobility data 
#mob = read_csv('data/mobility_subset.zip', 
#               col_types = cols(apple_mobility = col_double(), fips=col_character()))
# mob = read.csv('data/usa.csv')

# mob %>% 
#   select(fips, ds_m4, all_day_ratio_single_tile_users, m50, cases, deaths, 
#          apple_mobility, miles_per_person_umd, staying_home_umd, pop, over60, 
#          med_income, afram, hispar, male, pop_density, cov_death_rate, 
#          sg_sheltered, sg_avg_dist2) %>%
#   write_csv('data/mobility_subset.csv')
