# Pop data from DEX for plotting
# MSahu
# 6/16/2024

# ==============================================================================

# State / County Names 

county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")[ , cnty := sprintf("%05d", cnty)][, 1:5]
state_names <- fread("/mnt/share/dex/us_county/maps/state_regions.csv") %>% mutate(state_name = State) %>% 
  left_join( fread("/mnt/share/dex/us_county/maps/states.csv") %>% rename(state_code = abbreviation, laura_id = state), by = "state_name")

# State / County Pop
# DEX has from 2000-2021 only; impute 2022 using 2021 
# TODO update to 2022 pop

county_pop <- fread("/mnt/share/dex/us_county/03_post_model/pop_denom/best/inputs/county_population_age_sex.csv") %>% 
  filter(year_id > 1999) %>% 
  group_by(mcnty, year_id) %>% summarize(pop = sum(pop)) %>% ungroup() %>% 
  merge(county_names, by = "mcnty", all.x = TRUE) %>% setDT()
pop_2022 <- county_pop[year_id == 2021,] %>% mutate(year_id = 2022) # Use the 2021 values for 2022, for now
county_pop <- rbind(county_pop, pop_2022)
rm(pop_2022)

state_pop <- county_pop %>%
  group_by(state_name, year_id) %>%
  summarize(pop = sum(pop)) %>% 
  left_join(state_names, by = "state_name") %>%
  select(year_id, state_code, state_name, State, `State Code`, Region, Division, location_id, laura_id, pop)

write.csv(county_pop, "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/pop/dex/dex_county_pop00_21.csv", row.names = F)
write.csv(state_pop, "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/pop/dex/dex_state_pop00_21.csv", row.names = F)


