# ==============================================================================

# CLEAN THE LAT AND LONG VALUES PROVIDED IN AHA DATA [TAKES A FEW min]
# MSahu
# November 16, 2023

# STEPS
# 1: get the missing geocodes [QGIS??]
# 2: plot the geocodes, by state
# 3: manually clean wrong geocodes
# 4: plot the corrected geocodes
# 5: then, map on the county and call it "cnty"

# Inputs:
#   1) aha_intermediate_2000_2019: cleaned individual hospital data with HRR and state-HRR

# Output:
#   1) aha_clean_2000_2019: cleaned individual hospital data with CORRECTED geocodes

# ==============================================================================

rm(list=ls())

# Packages 
pacman::p_load(data.table, dplyr, stringr, readxl, reshape2, arrow,
               sf, usmap)

# Directories
dir <- "/mnt/share/resource_tracking/us_value/data/"
in_dir <- paste0(dir, "hospital_hhi/processed/")
plot_dir <- paste0(dir, "hospital_hhi/plots/geocode_cleaning/")
geocoding <- paste0(dir, "hospital_hhi/raw/aha/AHA raw data/qgis_geocoding/")

# ==============================================================================

# Read data
aha_clean <- read_feather(paste0(in_dir, ".archive/aha_intermediate_2000_2019.feather"))

# MISSING GEOCODES -- DO THIS ONLY ONCE

missing_geocodes <- aha_clean %>% filter(is.na(LONG) | is.na(LAT) | LONG == "." | LAT == ".") %>% 
   
   mutate(address = paste0(MLOCADDR, ", ", MLOCCITY, ", ", MSTATE, ", ", MLOCZIP))
 
# write.csv(missing_geocodes, paste0(geocoding, "missing_geocodes.csv"), row.names = F)

missing_geocodes <- missing_geocodes %>% select(ID, year_id) 

# NOW EXTRACTED FROM QGIS (MANUAL DRAG/DROP LOCALLY) --> merge with aha_clean

geocodes_qgis <- st_read(paste0(geocoding, "missing_geocodes.shp")) %>% 
  separate(latlong, into = c("LAT.y", "LONG.y"), sep = ",") %>% 
  select(-c(result_num, status, formatted_, place_id, location_t)) %>% 
  st_drop_geometry() %>% 
  select(year_id, ID, LAT.y, LONG.y) %>% mutate(year_id = as.integer(year_id))

# merge

aha_clean_geocoded <- aha_clean %>% 
  
  left_join(geocodes_qgis, by = c("year_id", "ID")) %>% 
  
  mutate(LAT = ifelse(is.na(LAT) | LAT == ".", LAT.y, LAT),
         LONG = ifelse(is.na(LONG) | LONG == ".", LONG.y, LONG)) %>% 
  
  select(-LAT.y, -LONG.y)

rm(aha_clean, missing_geocodes)

# CLEAN TWO OBVIOUSLY WRONG GEOCODES

aha_clean_geocoded <- aha_clean_geocoded %>% 
  
  mutate(LONG = ifelse(LONG == "-114615217", "114.615217", LONG)) %>% 
  
  arrange(MSTATE, ID, year_id)

# ==============================================================================

# Convert format

aha_numeric <- aha_clean_geocoded %>% mutate(LAT = as.numeric(LAT), LONG = as.numeric(LONG))

aha_map <- usmap_transform(aha_numeric, 
                             input_names = c("LONG", "LAT"),
                             output_names = c("x", "y")) 

# PLOT GEOCODE FUNCTION

state <- us_map(regions = "states") # boundaries from usmap package

state_map <- function(state_code, point_data) {
  
  data = point_data %>% filter(MSTATE == my_state_code)
  
  state_map <- ggplot() +
    
    geom_polygon(data = state, aes(x = x, y = y, group = group), fill = NA, color = "black") +
    
    geom_point(data = data, aes(x = x, y = y, group = year_id), color = "red", size = 1) +
    
    labs(title = paste0(my_state_code)) + 
    
    theme_void() +
    
    theme(legend.position = "right",
          legend.justification = "top",
          legend.key.height=unit(1.35,"cm"),
          legend.text=element_text(size=6),
          plot.caption = element_text(hjust = 0)) 
  
  return(state_map)
  
}


# loop and output

pdf(paste0(plot_dir, "/byState_map_hospitals_prior_to_cleaning.pdf"), width = 12, height = 8, onefile = TRUE) 

for(i in 1:length(unique(aha_clean_geocoded$MSTATE))){
  
  my_state_code <- unique(aha_clean_geocoded$MSTATE)[i]
  
  print(state_map(my_state_code, aha_map))
  
}

dev.off()

# ==============================================================================

# MANUAL CLEANING -- Create list of points that fall outside the state that it's supposed to be in

# Convert dataframe to an sf object
aha_map_sf <- st_as_sf(aha_numeric, coords = c("LONG", "LAT"), crs = 4269)

# Read state boundaries (downloaded from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html)
state_boundaries <- st_read(paste0(geocoding, "state_boundaries/cb_2018_us_state_500k.shp")) %>% rename(MSTATE = STUSPS)

# Set up DF
outside_boundary_addresses <- data.frame(
  ID = as.character(),
  MSTATE = as.character(),
  year_id = as.integer(),
  MLOCADDR = as.character(),
  MLOCCITY = as.character(),
  MLOCZIP = as.character()
)

# ==============================================================================

# Loop through, join data frame, and map

pdf(paste0(plot_dir, "/byState_map_hospitals_outside_boundaries.pdf"), width = 12, height = 8, onefile = TRUE) 

for(i in 1:length(unique(aha_clean_geocoded$MSTATE))){
  
  my_state_code <- unique(aha_clean_geocoded$MSTATE)[i]
  
  state_boundary <- state_boundaries %>% filter(MSTATE == my_state_code) %>% pull(geometry)
  
  state_sf <- aha_map_sf %>% filter(MSTATE == my_state_code)
  
  # Perform a spatial join based on "MSTATE"
  state_intersection <- st_intersection(state_sf, state_boundary)
  
  outside_boundary <- setdiff(state_sf %>% select(ID, MSTATE, year_id) %>% st_drop_geometry(), 
                              state_intersection %>% select(ID, MSTATE, year_id) %>% st_drop_geometry()) %>% 
    left_join(aha_map, by = c("ID", "MSTATE", "year_id"))
  
  # MAP
  print(state_map(my_state_code, outside_boundary))
  
  # Bind 
  outside_boundary_addresses <- rbind(outside_boundary_addresses, 
                                      outside_boundary %>% select(ID, MSTATE, year_id, MLOCADDR, MLOCCITY, MLOCZIP))
  
}

dev.off()

# Create CSV to drop into QGIS

write.csv(outside_boundary_addresses, file = paste0(geocoding, "points_outside_state.csv"), row.names = F)

# ==============================================================================

# Import correct coordinates and merge

geocodes_qgis2 <- st_read(paste0(geocoding, "points_outside_qgis.shp")) %>% 
  separate(latlong, into = c("LAT.y", "LONG.y"), sep = ",") %>% 
  select(-c(result_num, status, formatted_, place_id, location_t)) %>% 
  st_drop_geometry() %>% 
  select(year_id, ID, LAT.y, LONG.y) %>% mutate(year_id = as.integer(year_id))

# merge

aha_clean_geocoded <- aha_clean_geocoded %>% 
  
  left_join(geocodes_qgis2, by = c("year_id", "ID")) %>% 
  
  mutate(LAT = ifelse(!is.na(LAT.y), LAT.y, LAT),
         LONG = ifelse(!is.na(LONG.y), LONG.y, LONG)) %>% 
  
  select(-LAT.y, -LONG.y) %>% setDT()

# ==============================================================================

# Map updated version

# Convert format

aha_numeric <- aha_clean_geocoded %>% mutate(LAT = as.numeric(LAT), LONG = as.numeric(LONG))

aha_map <- usmap_transform(aha_numeric, 
                           input_names = c("LONG", "LAT"),
                           output_names = c("x", "y")) 

# PLOT

# loop and output

pdf(paste0(plot_dir, "/byState_map_hospitals_cleaned.pdf"), width = 12, height = 8, onefile = TRUE) 

for(i in 1:length(unique(aha_clean_geocoded$MSTATE))){
  
  my_state_code <- unique(aha_clean_geocoded$MSTATE)[i]
  
  print(state_map(my_state_code, aha_map))
  
}

dev.off()

# ==============================================================================

options(digits = 5) 

# Update file

write.csv(aha_clean_geocoded, paste0(in_dir, ".archive/aha_clean_geocoded_2000_2019.csv"), row.names = F)

# Write list of updated geocodes (for Harry Koos)

write.csv(geocodes_qgis, paste0(geocoding, "missing_geocodes_updated_for_harry.csv"), row.names = F)
write.csv(geocodes_qgis2, paste0(geocoding, "incorrect_geocodes_updated_for_harry.csv"), row.names = F)

# Write unique geocodes for 30-minute drive time

aha_unique_geocodes <- aha_clean_geocoded %>% select(LAT, LONG) %>% 
  mutate(LAT = round(as.numeric(LAT), 5),
             LONG = round(as.numeric(LONG), 5)) %>% distinct()
write.csv(aha_unique_geocodes, file = paste0(in_dir, "buffer/drive_time/unique_geocodes.csv"))