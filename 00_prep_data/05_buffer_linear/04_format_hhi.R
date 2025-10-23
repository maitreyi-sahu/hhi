# Maitreyi Sahu
# Aug 27, 2023

# Format the LINEAR DISTANCE hhi so it's consistent with the following vars:
# ID, YEAR, hhi_type, market_var, radius, radius_units, hosp_type, hhi

# Output file: "linear_distance_hhi_formatted.feather" 

# TO DO: check 2019 values and against haversine / other linear

# ==============================================================================

rm(list=ls())
pacman::p_load(dplyr, data.table, stringr, arrow, ggplot2)

dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"

# ------------------------------------------------------------------------------

# LOAD FILES

# 2000-2019 from MS
stamp <- "2024-04-08" # UPDATE IF RERUNNING THIS
linear_hhi <- read_feather(paste0(dir, "buffer/linear/linear_hhi_compiled_", stamp ,".feather")) %>% setDT()

# Import 2019-2022 from HK
harry_dir <- paste0(dir, "buffer/linear/from_harry/")

read_harry <- function(file) {
  hosp_type = ifelse(grepl("Gen_Surg", file), "gen_surg", "all")
  dt <- fread(paste0(harry_dir, file))
  dt <- dt[, year_id := 2000 + year_id]
  dt <- dt[, hosp_type := hosp_type]
  setnames(dt, "aha_systems", "aha_systems_n")
  return(dt)
}
harry_files <- list.files(harry_dir, pattern = ".csv")
linear_hhi_harry <- rbindlist(lapply(harry_files, read_harry)) %>% filter(year_id %in% 2020:2022) # use my values for 2019 for now (come back to this after checks)

combined_hhi <- rbind(linear_hhi, linear_hhi_harry, fill = T) # harry doesn't have "aha_hosp_n" but ok

# ------------------------------------------------------------------------------

# FORMAT / CLEANUP
formatted_hhi <- combined_hhi %>% select(ID, year_id, hhi_beds, hhi_admits, hhi_pt_days,  hhi_mcr_pt_days, radius, hosp_type) %>% 
  
  # pad the IDs [leading 0's get dropped when imported in the console]
  
  mutate(ID = str_pad(ID, width = 10, side = "left", pad = "0")) %>%  # use stringr package because "sprintf" is adding trailing spaces...

  # reshape long 
  rename(HOSPBD = hhi_beds,
         ADMTOT = hhi_admits,
         IPDTOT = hhi_pt_days,
         MCRIPD = hhi_mcr_pt_days) %>%
  reshape2::melt(id.vars = c("ID", "year_id", "radius", "hosp_type"), measure.vars = c("HOSPBD", "ADMTOT", "IPDTOT", "MCRIPD"), variable.name = "market_var") %>% 
  rename(hhi = value) %>% 
  
  # add additional vars
  mutate(hhi_type = "linear",
         radius_units = "miles") %>% 
  select(ID, year_id, hhi_type, market_var, radius, radius_units, hosp_type, hhi)

# ------------------------------------------------------------------------------

# Save

write_feather(formatted_hhi, paste0(dir, "buffer/linear/linear_distance_hhi_formatted.feather"))
