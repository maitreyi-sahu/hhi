# Check for additional missing polygons
# MSahu
# March 22, 2024

# ==============================================================================

# Setup
rm(list=ls())
pacman::p_load(dplyr, stringr, tidyr, sf, sp, rgdal, ggplot2, arrow) 

# Directories
data_dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"
polygon_dir <- paste0(data_dir, "buffer/drive_time/polygons/")

# ==============================================================================

# LOOP through all radii and save missing coordinates

for (current_radius in c(15, 30, 60)) {
  
  # File names
  current_polygon_dir <- paste0(polygon_dir, "polygons_", current_radius, "m/")
  geojson_file_names_main_folder <- c(list.files(current_polygon_dir, pattern = "\\.geojson$", full.names = F))
  
  # ----------------------------------------------------------------------------
  
  # Unique LON/LAT values from AHA
  unique_lat_long <- fread(paste0(data_dir, "aha_clean_2000_2019.csv")) %>% 
    filter(!is.na(LONG) & !is.na(LAT) & LONG!="." & LAT !=".") %>% 
    mutate(LAT = round(LAT, 5), LONG = round(LONG, 5)) %>% 
    mutate(lon_string = ifelse(LONG >=0, 
                              gsub(paste0("E", sprintf("%.5f", abs(LONG))), pattern = "\\.", replacement = "_"),
                              gsub(paste0("W", sprintf("%.5f", abs(LONG))), pattern = "\\.", replacement = "_")),
           lat_string = ifelse(LAT >=0, 
                              gsub(paste0("N", sprintf("%.5f", abs(LAT))), pattern = "\\.", replacement = "_"),
                              gsub(paste0("S", sprintf("%.5f", abs(LAT))), pattern = "\\.", replacement = "_"))) %>%
    mutate(coords_pattern = paste0(lon_string, "_", lat_string)) %>% 
    select(LONG, LAT, lon_string, lat_string, coords_pattern) %>% unique()
  
  # Check if polygon file exists; if not, save to list
  unique_lat_long$matches<- sapply(unique_lat_long$coords_pattern, 
                                                function(c) {grep(pattern = c, x = geojson_file_names_main_folder, value = T)})
  unique_lat_long$missing <- ifelse(unique_lat_long$matches == "character(0)", 1, 0)
  
  # ----------------------------------------------------------------------------
  
  # Save
  missing_lat_lon_vals <- unique_lat_long %>% filter(missing == 1) %>% select(LONG, LAT, lon_string, lat_string) %>% mutate(drive_time = current_radius)
  write.csv(missing_lat_lon_vals, paste0(polygon_dir, "geocodes_for_extraction/missing_geocodes_remaining.csv"), row.names = F)
}
