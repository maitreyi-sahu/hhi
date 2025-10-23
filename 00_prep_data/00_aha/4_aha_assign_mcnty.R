# Assign mcnty and clean cnty's which were assigned incorrectly
# msahu
# March 9, 2025

# ==============================================================================

# SETUP

rm(list=ls())
pacman::p_load(data.table, dplyr, sf)
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"

# Read county names from DEX 
county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")
state_codes <- fread('/mnt/share/dex/us_county/maps/states1.csv') %>% select(state_name, abbreviation)

# Read AHA data
aha_clean <- read.csv(paste0(dir, "processed/.archive/aha_clean_2000_2019.csv"))

# ==============================================================================

# LOAD DATA AND SET COORDINATE SYSTEM, separately for cont US, AK, HI

# Use the CRS 4326, for global 
us_crs = 4326

# AHA data
aha_clean_sf <- aha_clean %>% 
  st_as_sf(coords = c('LONG', "LAT"), crs = us_crs) 

# Read mcnty shapefile [WITH 'TRUE' BOUNDARIES - NOT THE ONE FOR MAPPING]
cnty_shp <- st_as_sf(readRDS('/snfs1/Project/us_counties/locations/counties/prepped_shp/cnty_shape_file.rds')) %>% 
  st_transform(crs = us_crs) 
# mcnty_shp <- st_as_sf(readRDS('/snfs1/Project/us_counties/locations/counties/prepped_shp/mcnty_shape_file.rds')) %>% 
#   st_transform(crs = us_crs) 

# ==============================================================================

# PLOT

ggplot() +
  geom_sf(data = cnty_shp, fill = NA, color = 'black') +  # County boundaries
  geom_sf(data = aha_clean_sf, color = "red", alpha = 0.5)  # Hospital locations
# ggplot() +
#   geom_sf(data = mcnty_shp, fill = NA, color = 'black') +  # County boundaries
#   geom_sf(data = aha_clean_sf, color = "red", alpha = 0.5)  # Hospital locations


# ==============================================================================

# ASSIGN POINTS TO MCNTY AND CNTY

aha_joined_sf <- st_join(aha_clean_sf, cnty_shp, join = st_within) %>% st_drop_geometry()
#aha_mcnty_sf <- st_join(aha_clean_sf, mcnty_shp, join = st_within) %>% st_drop_geometry()

rm(aha_clean_sf)

# ==============================================================================

# CLEAN (X is original, Y is new)

# check if mcnty or cnty is NA
missing_counts <- aha_joined_sf %>%
  summarize(
    missing_mcnty = sum(is.na(mcnty.y)),
    missing_cnty = sum(is.na(cnty.y))
  )

print(paste0("Number of counties with missing mcnty: ", missing_counts$missing_mcnty))
print(paste0("Number of counties with missing cnty: ", missing_counts$missing_cnty))

rm(missing_counts)

# check when cnty state doesn't match mcnty state --> there are lots!
aha_joined_sf %>% filter( mcnty.x!=mcnty.y ) %>% nrow()
aha_joined_sf %>% filter( cnty.x!=cnty.y ) %>% nrow()
aha_joined_sf %>% filter( state_name.x!=state_name.y) %>% nrow()

aha_joined_sf <- aha_joined_sf %>% 
  mutate(mcnty = mcnty.y,
         state = state.y,
         state_name = state_name.y,
         cnty = cnty.y,
         cnty_name = cnty_name.y) %>% 
  select(-matches("\\.x$|\\.y$")) %>% 
  
  # MANUAL CLEANING 
  
  # Kentucky
  mutate(cnty = ifelse(ID == '0006510175' & year_id > 2001, 21047, cnty ),
         cnty_name = ifelse(ID == '0006510175' & year_id > 2001, 'Christian County', cnty_name)) %>% 
  
  # Las Vegas
  mutate(cnty = ifelse(ID == '0006880085' & year_id > 2001, 32003, cnty),
         cnty_name = ifelse(ID == '0006880085' & year_id > 2001, 'Clark County', cnty_name),
         MLOCZIP = ifelse(ID == '0006880085' & year_id > 2001, '89086-4400', MLOCZIP))
  

# ==============================================================================

# Convert back to DT

coords <- aha_clean %>% select(year_id, ID, LONG, LAT)
aha_clean_mapped <- aha_joined_sf %>% st_drop_geometry() %>% left_join(coords, by = c('ID', 'year_id'))

# ==============================================================================

# SAVE

write.csv(aha_clean_mapped, paste0(dir, "processed/aha_clean_2000_2019.csv"), row.names = F)

rm(coords, aha_joined_sf, mcnty_shp)