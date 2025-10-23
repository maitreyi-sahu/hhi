# MSahu
# Aug 26, 2023

# Lists of AHA IDS, HRRs, etc.
# Can source this as needed

# ==============================================================================

rm(list=ls())
pacman::p_load(dplyr, data.table)

dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"
aha_clean <- fread(paste0(dir, "aha_clean_2000_2019.csv"))
aha_counties <- aha_clean %>% arrange(desc(year_id)) %>% select(ID, cnty, mcnty) %>% distinct() %>% group_by(ID) %>% arrange(mcnty) %>%  slice(1) %>% ungroup() # TODO remove after getting FCOUNTY

# read 2019-2022 IDs from Harry and drop 2019
id_cols <- c("year_id", "ID", "MCRNUM", "MSTATE", "cnty", "mcnty", "hosp_zip", "pref_ID", "hrrnum") 
harry_ids <- fread(paste0(dir, "id_vars/from_harry/AHA_ID_File.csv"))[ 
  , `:=` (year_id = 2000 + YEAR, 
          # TODO cnty = FCOUNTY,
          hrrnum = HRRCODE, 
          hosp_zip = as.numeric(sub("-.*", "", MLOCZIP)))]
harry_ids <- harry_ids %>% left_join(aha_counties, by = "ID") # TODO remove after getting FCOUNTY
harry_ids <- harry_ids[ , ..id_cols][year_id!=2019, ]

# ==============================================================================

# POSTAL CODES / LOCATION NAMES 

policy_path <- "/snfs1/Project/IRH/US_value/Data/policy_vars/"
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
locs <- get_ids("location")

state_ids <-  locs[locs$location_id>=523 & locs$location_id<=573] %>% # 51 states including DC
  left_join(fread(paste0(policy_path,"market_concentration/hcup/postal_code.csv")), by = "location_name") %>% 
  rename(postal_code = state) %>% 
  mutate(MSTATE = postal_code) %>% 
  select(location_id, location_name, postal_code, MSTATE)

write.csv(state_ids, paste0(dir, "id_vars/state_ids.csv"), row.names = F)

# ------------------------------------------------------------------------------ 

# DISTINCT VALUES 

# Distinct Hospital IDS [here I recreate state-HRR var, so that also available for Harry vals]
ids <- aha_clean[ , ..id_cols] %>% rbind(harry_ids, fill = T) # TODO remove fill = T 
ids <- ids[ , state_hrr := ifelse(!is.na(hrrnum), paste0(MSTATE, as.character(hrrnum)), NA)]  %>% 
  mutate(MCRNUM = as.integer(MCRNUM)) %>%  # Cooper data has this as integer
  arrange(year_id, MSTATE, ID, hrrnum, cnty, mcnty, hosp_zip, pref_ID, state_hrr) %>% 
  distinct()

write.csv(ids, paste0(dir, "id_vars/aha_clean_ids.csv"), row.names = F)

#check <- ids %>% filter(is.na(mcnty))
# TODO check with Harry to get county names for 2020-2022

# Distinct State-HRRs

state_hrr <- ids %>%   
  select(year_id, MSTATE, hrrnum, state_hrr) %>% 
  distinct()

write.csv(state_hrr , paste0(dir, "id_vars/aha_state_hrrs.csv"), row.names = F)
