# Maitreyi Sahu
# Aug 27, 2023

# Format the linear distance hhi so it's consistent with the following vars:
# ID, YEAR, hhi_type, market_var, radius, radius_units, hhi

# Output file: "linear_distance_hhi_formatted.csv" 

# ------------------------------------------------------------------------------

rm(list=ls())

# Setup

pacman::p_load(dplyr, stringr)

dir <- "/mnt/share/resource_tracking/us_value/data/"
out_dir <- paste0(dir, "hospital_hhi/processed/")

# ------------------------------------------------------------------------------

stamp <- "2023-09-02" # UPDATE IF RERUNNING THIS
individual_hhi <- fread(paste0(out_dir, "buffer/linear_crs3857/linear_distance_hhi_compiled_", stamp ,".csv")) 

# Import 2019-2021 data from Harry Koos at Stanford
harry_linear <- fread(paste0(out_dir, "buffer/linear_crs3857/from_harry/AHA_HHI_2019_2021_Linear_buffer.csv")) %>% 
  mutate(year_id = 2000 + year_id)

# Use Harry's version for 2019 because he updated some incorrect geocodes
# ACTUALLY THIS GIVES WEIRD RESULTS =- USE OUR VERSION
combined_hhi <- individual_hhi %>% rbind(harry_linear %>% filter(year_id!=2019))

# ------------------------------------------------------------------------------

# FORMAT / CLEANUP
formatted_hhi <- combined_hhi %>% select(ID, year_id, hhi_beds, hhi_admits, hhi_pt_days,  hhi_mcr_pt_days, radius) %>% 
  
  # pad the IDs [leading 0's get dropped when imported in the console]
  
  mutate(ID = str_pad(ID, width = 10, side = "left", pad = "0")) %>%  # use stringr package because "sprintf" is adding trailing spaces...

  # reshape long 
  rename(HOSPBD = hhi_beds,
         ADMTOT = hhi_admits,
         IPDTOT = hhi_pt_days,
         MCRIPD = hhi_mcr_pt_days) %>%
  melt(id.vars = c("ID", "year_id", "radius"), measure.vars = c("HOSPBD", "ADMTOT", "IPDTOT", "MCRIPD"), variable.name = "market_var") %>% 
  rename(hhi = value) %>% 
  
  # add additional vars
  mutate(hhi_type = "linear",
         radius_units = "miles",
         hosp_type = "all") %>% 
  select(ID, year_id, hhi_type, market_var, radius, radius_units, hosp_type, hhi)

# Check - now all 10 characters
# formatted_hhi$nchar <- nchar(formatted_hhi$ID)
# table(formatted_hhi$nchar, formatted_hhi$YEAR)

# ------------------------------------------------------------------------------

# Save

write.csv(formatted_hhi, paste0(out_dir, "buffer/linear_crs3857/linear_distance_hhi_formatted.csv"), row.names = F)
#write.csv(formatted_hhi, paste0(out_dir, "buffer/linear_distance_hhi_formatted_",stamp,".csv"), row.names = F)
