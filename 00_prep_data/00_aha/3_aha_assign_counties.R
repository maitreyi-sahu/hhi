
# ABANDONED 
# NOTE - this now essentially gets rewritten using the mcnty/cnty script..

# MSahu
# 4/6/2024; Updated 3/11/2025

# ==============================================================================

# GET THE COUNTY USING COUNTY BOUNDARIES FROM THE TIGRIS PACKAGE 
# https://rdrr.io/cran/tigris/man/counties.html
# https://walker-data.com/census-r/census-geographic-data-and-applications-in-r.html

# DO NOT RERUN - TAKES ~25 MIN
# Part of this code is run locally because 'tidygeocoder' not compatible with this environment

# ==============================================================================

# Setup
rm(list=ls())
pacman::p_load(data.table, dplyr, stringr, arrow, sf, usmap, tigris)
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"

# Read county names from DEX 
county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")[ , cnty := sprintf("%05d", cnty)][, c("cnty", "cnty_name")]

# For missing counties, get them from Google API (locally)
# install.packages("googleway", library = "~/rpackages")
# library(googleway, lib.loc = "~/rpackages")

#-------------------------------------------------------------------------------

# Load file
aha_clean_geocoded <- fread(paste0(dir, "processed/.archive/aha_clean_geocoded_2000_2019.csv"))

# Function to use county boundaries from Tigris package to assign hospitals to counties
get_counties <- function(yr, state_code) {
  
  # yr = 2017
  # state_code = "KY"
  
  print(paste0("year_id = ", yr, "; state_code = ", state_code))
  
  cnty_cols = c("year_id", "ID", "MSTATE", "cnty")
  
  # function from tigris package to get county boundaries
  counties_sf <- counties(state_code, 
                       cb = F, # can change to "T" for faster processing, but will be less detailed 
                       year = yr,
                       progress_bar = T) %>% 
    st_transform(4326) %>% # as a sphere on earth
    mutate(intersect_code = row_number())
  
  # get the AHA data for this state/year and convert to SF (spatial DF)
  aha_counties <- setDT(aha_clean_geocoded)[ year_id == yr & MSTATE == state_code, c("year_id", "ID", "MSTATE", "LAT", "LONG")]
  aha_counties_sf <- st_as_sf(aha_counties, coords = c("LONG", "LAT"), crs = 4326)
  
  # get intersections between AHA data and county boundaries
  intersects <- st_intersects(aha_counties_sf, counties_sf)
  intersects <- lapply(intersects, function(x) ifelse(is.null(x), NA_integer_, x)) # must convert null to NA
  aha_counties_intersects <- aha_counties[, intersect_code := unlist(intersects)] 
  aha_counties_intersects <-  merge(aha_counties, counties_sf %>% st_drop_geometry, by = "intersect_code", all.x = TRUE)[ ,`:=` (cnty =  paste0(STATEFP, COUNTYFP))][, ..cnty_cols]
  
  return(aha_counties_intersects)
}

# RUN
start.time = Sys.time()
yrs <- c(2010:2019)
state_codes <- unique(aha_clean_geocoded$MSTATE)
yr_state_list <- tidyr::crossing(yrs, state_codes)
aha_counties_combined <- rbindlist(lapply(1:nrow(yr_state_list), function(i) get_counties(yr_state_list$yrs[i], yr_state_list$state_codes[i])))
execution.time = Sys.time() - start.time
print(execution.time)

# Then merge; for 2000:2009 use the OLDEST YEAR AVAILABLE
aha_clean_with_county <- aha_clean_geocoded %>% left_join(aha_counties_combined, by = c("year_id", "ID", "MSTATE")) %>% setDT()
oldest_year_data <- aha_clean_with_county[!is.na(cnty), .SD[which.min(year_id)], by = ID]
aha_clean_with_county <- aha_clean_with_county[ , cnty := ifelse(year_id %in% 2000:2009, oldest_year_data$cnty[match(ID, oldest_year_data$ID)], cnty), by = ID] %>% 
  left_join(county_names, by = "cnty")

write.csv(aha_clean_with_county, paste0(dir, "processed/.archive/aha_clean_with_county_stage1.csv"))
rm(aha_clean_geocoded)
# ------------------------------------------------------------------------------
 
# Get missing values locally using "tidygeocoder" - this package is super slow and also takes ~25 min for the 3661 missing coords [gives back the current county]

missing_counties <- fread(paste0(dir, "processed/.archive/aha_clean_with_county_stage1.csv"))[is.na(cnty), -1] %>% 
  select(year_id, ID, MNAME, MLOCADDR, MLOCCITY, MSTATE, MLOCSTCD, hosp_zip, LAT, LONG) %>% 
  left_join(fread(paste0(dir, "processed/id_vars/state_ids.csv")), by = "MSTATE") 
write.csv(missing_counties, paste0(dir, "processed/.archive/missing_counties.csv"), row.names = F)

# CODE THAT'S RUN LOCALLY ...

# library(tidygeocoder)
# library(tigris)
# library(sf)
# 
# missing_counties <- fread("Desktop/missing_counties.csv") %>% mutate(STATEFP = as.character(MLOCSTCD))
# state_names <- missing_counties %>% select(STATEFP, location_name) %>% distinct() %>% rename(state = location_name)

# county_names = counties() %>% st_drop_geometry %>% mutate(cnty =GEOID, county = NAMELSAD) %>% select(county, cnty, STATEFP) %>% left_join(state_names, by = "STATEFP")

# missing_county_addresses <- reverse_geo(
#   missing_counties$LAT,
#   missing_counties$LONG,
#   method = 'osm',
#   address = "address",
#   limit = 1,
#   full_results = T,
#   mode = "",
#   unique_only = FALSE,
#   return_coords = TRUE,
#   min_time = NULL,
#   quiet = getOption("tidygeocoder.quiet", FALSE),
#   api_url = NULL,
#   timeout = 20,
#   flatten = TRUE,
#   batch_limit = NULL,
#   verbose = getOption("tidygeocoder.verbose", FALSE),
#   no_query = FALSE,
#   custom_query = list(),
#   api_options = list()
# ) 
# 
# missing_county_counties <- missing_county_addresses %>% 
#   mutate(LAT = lat, LONG = long) %>% 
#   # remove digits and commas
#   select(lat, long, county, state) %>% 
#   left_join(county_names, by = c("county", "state"))
# 
# write.csv(missing_county_counties, "Desktop/missing_counties_mapped.csv", row.names = F)

# ==============================================================================

# Read back the mapping that was run locally and clean up

county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")

# checked mapped file for missing county names
nrow( fread(paste0(dir, "processed/.archive/missing_counties_mapped.csv"))  %>% filter(is.na(county)) %>%  distinct())  # 183 missing county names / 76 distinct

# clean the missing county names using chat-GPT
missing_counties_gpt <- fread(paste0(dir, "processed/.archive/missing_county_names_chatgpt_cleaned.csv")) %>% 
  mutate(lat5 = as.character(format(lat, nsmall = 5)), long5 = as.character(format(long, nsmall = 5))) %>% 
  select(-lat, -long)

# merge and clean
missing_counties_mapped <- fread(paste0(dir, "processed/.archive/missing_counties_mapped.csv")) %>% 
  mutate(lat5 = as.character(format(lat, nsmall = 5)), long5 = as.character(format(long, nsmall = 5)))  %>% 
  rename(cnty_name = county, state_name = state) %>% 
  left_join(missing_counties_gpt, by = c("lat5", "long5")) %>% 
  mutate(cnty_name = ifelse(is.na(cnty_name), county, cnty_name),
         state_name = ifelse(is.na(state_name), state, state_name)) %>% 
  select(-state, -county) %>%
  left_join(county_names %>% select("cnty_name", "cnty", "state_name"), by = c("state_name", "cnty_name")) %>% # join fips
  setDT()

missing_counties_mapped <- missing_counties_mapped[ , CNTY := sprintf("%05d", cnty)] %>% 
  rename(CNTY_NAME = cnty_name) %>% 
  select(lat5, long5, CNTY_NAME, CNTY) %>% distinct()

rm(missing_counties_gpt, missing_counties, oldest_year_data)

# ==============================================================================

# Merge back with full AHA dataset

aha_clean_with_county_mapped <- fread(paste0(dir, "processed/.archive/aha_clean_with_county_stage1.csv"))[, -1] %>% 
  mutate(lat5 = as.character(format(LAT, nsmall = 5)), long5 = as.character(format(LONG, nsmall = 5)))  %>% 
  left_join(missing_counties_mapped, by = c("lat5", "long5")) %>% 
  mutate(cnty = ifelse(is.na(cnty), CNTY, cnty),
         cnty_name = ifelse(is.na(cnty_name), CNTY_NAME, cnty_name)) %>% 
  
  # One last missing one - Fort Campbell
  mutate(cnty = ifelse(ID == "0006510175", "21047", cnty),
         cnty_name = ifelse(ID == "0006510175", "Christian County", cnty_name)) %>% 
  
  select(-lat5, -long5, -CNTY, -CNTY_NAME) 

print(paste0("Rows with missing counties: " ,nrow( aha_clean_with_county_mapped %>% filter(is.na(cnty) | cnty == "   NA"))))

# ==============================================================================

# EXTRACT REMAINING MISSING COUNTIES TO FEED TO CHAT-GPT 
#na_cnty <- aha_clean_with_county_mapped %>% filter(cnty == "   NA" ) %>% select(year_id, ID, MNAME, MLOCADDR, MLOCCITY, MSTATE, MLOCZIP, cnty, cnty_name) #, mcnty)
#write.csv(na_cnty, paste0(dir, "processed/.archive/na_cnty.csv"), row.names = F)

# [map using GPT]

# READ GPT-MAPPED FIPS CODES
gpt_cnty <- fread(paste0(dir, "processed/.archive/na_cnty_gpt.csv")) %>% select(year_id, ID, cnty) %>% 
  mutate(cntyR =  sprintf("%05d", cnty)) %>%  select(-cnty) %>% 
  mutate(ID = str_pad(as.character(ID), width = 10, side = "left", pad = "0"))

# MERGE BACK
aha_clean_with_county_mappedR <- aha_clean_with_county_mapped %>% left_join(gpt_cnty , by = c('year_id', 'ID')) %>% 
  mutate(cnty = ifelse(is.na(cntyR), cnty, cntyR)) %>% select(-cntyR, -cnty_name) %>% 
  left_join(county_names[ , cnty := sprintf("%05d", as.integer(cnty))], by = "cnty")

print(paste0("Rows with missing counties: " ,nrow( aha_clean_with_county_mappedR %>% filter(is.na(cnty) | cnty == "   NA"))))

rm(aha_clean_with_county_mapped, missing_counties_mapped, gpt_cnty)

# ==============================================================================

# NEW ADDITION 12/11/24 - add back the missing HRRs / state-HRRs 

aha_hrrs <- read_feather(paste0(dir, "processed/.archive/aha_intermediate_2000_2019.feather")) %>% select(year_id, ID, hrrnum, hrrcity, hrrstate, state_hrr) 

aha_clean_with_county_mappedR <- aha_clean_with_county_mappedR %>% select(-hrrnum, -hrrcity, -hrrstate, -state_hrr) %>%  
  left_join(aha_hrrs, by = c("year_id", "ID")) 

print(paste0("Rows with missing HRRs: " ,nrow( aha_clean_with_county_mappedR %>% filter(is.na(hrrnum)) )))

# ==============================================================================

# SAVE

write.csv(aha_clean_with_county_mappedR, paste0(dir, "processed/.archive/aha_clean_2000_2019.csv"), row.names = F)
