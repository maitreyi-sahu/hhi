# MSahu
# Updated 4/6/2024

# Read in individual-level HHI from Zach Cooper; uses 30-minute travel time radius, time-invariant for 2008-2014

# NOTES:
#  1) We average over the AHA ID, but there are approximately 3-4 Medicare Numbers per AHA Hospital
#  2) The Cooper AHA ID's don't line up with our AHA ID's by year, because it's an average 2008-14

# ==============================================================================

rm(list=ls())

# Packages
pacman::p_load(tidyr, dplyr, readxl)

# Directories 
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"
in_dir <- paste0(dir, "raw/aha/Zack Cooper HHI using AHA data/")
out_dir <- paste0(dir, "processed/cooper/")

# ------------------------------------------------------------------------------

# Read Cooper estimates

cooper_data <- read_xlsx(paste0(in_dir, "aha_hosp_with_hhi_clean_1.xlsx"))[-c(1:3, 5),]

names(cooper_data) <- lapply(cooper_data[1, ], as.character)
cooper_data <- cooper_data[-1,] 

cooper_data <- cooper_data %>% 
  mutate(hhi = as.numeric(`Herfindahl–Hirschman Index (HHI)`),
         MCRNUM = as.integer(`Medicare Hospital ID`))

# ------------------------------------------------------------------------------

# Add ID var

# Cooper - add ID var instead 
ids <- fread(paste0(dir, "processed/id_vars/aha_clean_ids.csv"))[year_id == 2011, c("ID", "MCRNUM")] %>%  distinct()

cooper_data <- cooper_data %>% 
  
  left_join(ids, by = "MCRNUM") %>% 
  
  # IF MULTIPLE MEDICARE NUMBERS PER HOSPITAL, AVERAGE THE HHI - goes from 22k --> 6k
  # COME BACK TO THIS STEP
   group_by(ID) %>%  
   summarize(hhi = mean(hhi)) %>% 
   ungroup() %>% 
  
  # add other vars
  mutate(
    year_id = 2011,
    hhi_type = "cooper_drive_time_08_14",
    market_var = "HOSPBD",
    radius = 30,
    radius_units = "min",
    hosp_type = "gen_surg") %>%  # paper says it's limited to " general medical/surgical hospitals with at least 50 cases" -- not sure if this includes surgical only??
  
select(ID, year_id, hhi_type, market_var, radius, radius_units, hosp_type, hhi)


# ------------------------------------------------------------------------------

# Add HRR num for the most recent year available

ids <- fread(paste0(dir, "processed/id_vars/aha_clean_ids.csv")) %>% select(ID, year_id, hrrnum) %>%  distinct() %>% 
  group_by(ID) %>% 
  filter(year_id == max(year_id)) %>% ungroup() %>%  select(ID, hrrnum)

cooper_data <- cooper_data %>% 
  left_join(ids, by = "ID")

# ------------------------------------------------------------------------------


# Save

write.csv(cooper_data, paste0(out_dir, "cooper_hhi_formatted.csv"), row.names = F)