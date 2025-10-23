# Get census tract-level total population estimates using the TidyCensus package
# MSahu
# April 4, 2024

# THIS IS GIVING WEIRD RESULTS - come back to it

# Steps:
# 1. For 2009-2022, use 5-year ACS estimates
# 2. For 2000, use decennial estimate
# 3. For 2001-2008, linearly interpolate between 2000 and 2010 decennial estimates

# Note:
# 5-year ACS exists for 2009 and 2010 but not used because assume that decennial is better
# 1-year ACS exists for 2005-2009 but only provides data for places with pop > 65,000

# TO DO - MAP CENSUS TRACTS TO CHECK

# ==============================================================================

# setup
rm(list=ls())
pacman::p_load(data.table, dplyr, stringr, readxl, reshape2, arrow, ggplot2,
               tidycensus, # https://walker-data.com/census-r/an-introduction-to-tidycensus.html
               purrr, furrr) # to run in parallel
census_api_key("8babc646937002e75e6671479bb31cc2541b988a", install = T, overwrite = T)

# Source postal codes 
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"
states <- fread(paste0(dir, "state_ids.csv"))

# ==============================================================================

# For 2009-2022, get 5-year ACS estimates

# Get census variable names; pull the variable name for total pop
acs_vars2022 <- load_variables(2022, paste0("acs", 5)) 
tot_pop_var <- acs_vars2022[acs_vars2022$concept == "Total Population", "name"] %>% pull()
rm(acs_vars2022)

# Extract

get_pop <- function(year, state_code) {
  
  tot_pop <- get_acs(
    
    geography = "tract",
    state = state_code,
    survey = "acs5",
    variables =  c(tot_pop = tot_pop_var),
    cache_table = TRUE, # makes this run faster the second time around
    output = "wide",
    year = year ) %>% 
    
    mutate(year_id = year)
  
  return(tot_pop)
}

years = 2009:2022
pop09_22 <- do.call(rbind, lapply(years, get_pop, state_code = states$postal_code)) %>% 
  rename(tot_pop_acs5yr = tot_popE) %>% 
  select(year_id, GEOID, tot_pop_acs5yr)

# ==============================================================================

# For 2000, use decennial

# Get census variable names; pull the variable name for total pop
census_vars00 <- load_variables(2000, "sf1") # first decennial file
tot_pop_var <- census_vars00[census_vars00$concept == "TOTAL POPULATION [1]", "name"] %>% pull()
rm(census_vars00)

# Extract

get_pop <- function(year, state_code) {
  
  tot_pop <- get_decennial(
    
    geography = "tract",
    state = state_code,
    variables =  c(tot_pop = tot_pop_var),
    cache_table = TRUE, # makes this run faster the second time around
    output = "wide",
    year = year) %>% 
    
    mutate(year_id = year)
  
  return(tot_pop)
}

years = c(2000, 2010)
pop_decennial <- do.call(rbind, lapply(years, get_pop, state_code = states$postal_code[1])) %>% 
  rename(tot_pop_decennial = tot_pop) %>% 
  select(year_id, GEOID, tot_pop_decennial)

# ==============================================================================

# Linear interpolation for 2000 to 2008

combined_pop <- expand.grid(GEOID = unique(c(pop_decennial$GEOID, pop09_22$GEOID)), year_id = 2000:2022) %>% 
  left_join(pop_decennial, by = c("GEOID", "year_id")) %>%
  left_join(pop09_22, by = c("GEOID", "year_id")) %>% 
  # interpolate 2001-2009 using pop_decennial
  group_by(GEOID) %>% 
  mutate(tot_pop_interpolated = ifelse(year_id %in% 2001:2009, 
                                       tot_pop_decennial[year_id == 2000] + (tot_pop_decennial[year_id == 2010] - tot_pop_decennial[year_id == 2000]) * (year_id - 2000) / 10, 
                                       NA)) %>% 
  ungroup() %>% 
  
  # create final variable using 2000 decennial, 2001-2008 interpolation, 2009-2022 5-year ACS
  mutate(total_pop = case_when(
    year_id %in% 2001:2008 ~ tot_pop_interpolated,
    year_id == 2000 ~ tot_pop_decennial,
    year_id %in% 2009:2022 ~ tot_pop_acs5yr)) %>%
  
  # for 2009, if ACS is NA then use the interpolated value
  mutate(total_pop = ifelse(year_id == 2009 & is.na(tot_pop_acs5yr) & !is.na(tot_pop_interpolated), tot_pop_interpolated, total_pop))

# CHECKS

# for a single tract, is the result a straight line over time?
random_geoids <- sample(combined_pop$GEOID, 10)
check <- combined_pop %>% filter(GEOID %in% random_geoids) # COME BACK TO THIS - WHY ARE SO MANY MISSING
#check <- combined_pop %>% filter(GEOID == "01001020200")
ggplot(data = check) + 
  geom_point(aes(x = year_id, y = tot_pop_interpolated), color = "blue")+
  geom_point(aes(x = year_id, y = tot_pop_acs5yr), color = "black") +
  geom_point(aes(x = year_id, y = tot_pop_decennial), color = "red") +
  facet_wrap(~GEOID)
# not ideal because ACS shows there's a lot of variation over time

# for 2009, how far off is ACS versus interpolation?
check <- combined_pop %>% filter(year_id == 2009)
ggplot(data = check, aes(x = tot_pop_acs5yr, y = tot_pop_interpolated)) + geom_point()+ geom_abline(color = "blue")
# not great but not bad

# ==============================================================================

# SAVE

write_dataset(combined_pop, format = "parquet", partitioning = "year_id", path = paste0(dir, "pop/tidycensus00_22/tract/"))
#check <- open_dataset(paste0(dir, "pop/tidycensus00_22/")) %>% collect()
