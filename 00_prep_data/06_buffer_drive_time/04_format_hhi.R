# Maitreyi Sahu
# 4/6/2024

# Format the driving distance hhi so it's consistent with the following vars:
# ID, YEAR, hhi_type, market_var, radius, radius_units, hosp_type, hhi

# ------------------------------------------------------------------------------

rm(list=ls())
pacman::p_load(dplyr, stringr, arrow, data.table)

dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"

# ------------------------------------------------------------------------------

# LOAD FILES
stamp <- "2024-03-24" # UPDATE AS NEEDED
drivetime_hhi <- read_feather(paste0(dir, "buffer/drive_time/drive_time_hhi_compiled_", stamp ,".feather")) 

# Import 2019-2022 from Harry and reformat
harry_dir <- paste0(dir, "buffer/drive_time/from_harry/")
cols_to_select <- names(drivetime_hhi)

read_harry <- function(file) {
  
  hosp_type = ifelse(grepl("Gen_Surg", file), "gen_surg", "all")
  
  dt <- fread(file)[ 
    ,`:=` (hosp_type = hosp_type,
           hhi_beds = HHI_HOSPBD,
           hhi_pt_days = HHI_IPDTOT,
           hhi_admits = HHI_ADMTOT,
           hhi_mcr_pt_days = HHI_MCRIPD,
           tot_beds = NA, tot_admits = NA, tot_pt_days = NA, tot_mcr_pt_days = NA, aha_hosp_n = NA, # not included in these files
           aha_systems_n = N_Systems,
           radius = Drivetime,
           ID = AHA_ID,
           year_id = 2000 + YEAR)][, ..cols_to_select]
  return(dt)
}

harry_files <- list.files(harry_dir, pattern = ".csv")
drivetime_hhi_harry <- rbindlist(lapply(paste0(harry_dir, harry_files), read_harry))

# Use my 2019 vals
combined_hhi <- drivetime_hhi %>% rbind(drivetime_hhi_harry %>% filter(year_id!=2019))

# ------------------------------------------------------------------------------

# FORMAT / CLEANUP
formatted_hhi <- combined_hhi %>% select(ID, year_id, hhi_beds, hhi_admits, hhi_pt_days,  hhi_mcr_pt_days, hosp_type, radius) %>% 
  
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
  mutate(hhi_type = "drive_time",
         radius_units = "minutes") %>% 
  select(ID, year_id, hhi_type, market_var, radius, radius_units, hosp_type, hhi)

# ------------------------------------------------------------------------------

# Save

write_feather(formatted_hhi, paste0(dir, "buffer/drive_time/drive_time_hhi_formatted.feather"))
