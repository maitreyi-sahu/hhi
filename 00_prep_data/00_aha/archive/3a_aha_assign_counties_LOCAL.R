#install.packages("googleway")
rm(list=ls())
pacman::p_load( data.table, dplyr, tidyr, stringr)

library(tidygeocoder)
library(tigris)
library(sf)

missing_counties <- fread("Desktop/missing_counties.csv") %>% mutate(STATEFP = as.character(MLOCSTCD))
state_names <- missing_counties %>% select(STATEFP, location_name) %>% distinct() %>% rename(state = location_name)


county_names = counties() %>% st_drop_geometry %>% mutate(cnty =GEOID, county = NAMELSAD) %>% select(county, cnty, STATEFP) %>% left_join(state_names, by = "STATEFP")

missing_county_addresses <- reverse_geo(
  missing_counties$LAT,
  missing_counties$LONG,
  method = 'osm',
  address = "address",
  limit = 1,
  full_results = T,
  mode = "",
  unique_only = FALSE,
  return_coords = TRUE,
  min_time = NULL,
  quiet = getOption("tidygeocoder.quiet", FALSE),
  api_url = NULL,
  timeout = 20,
  flatten = TRUE,
  batch_limit = NULL,
  verbose = getOption("tidygeocoder.verbose", FALSE),
  no_query = FALSE,
  custom_query = list(),
  api_options = list()
) 

missing_county_counties <- missing_county_addresses %>% 
  mutate(LAT = lat, LONG = long) %>% 
  # remove digits and commas
  select(lat, long, county, state) %>% 
  left_join(county_names, by = c("county", "state"))

write.csv(missing_county_counties, "Desktop/missing_counties_mapped.csv", row.names = F)
