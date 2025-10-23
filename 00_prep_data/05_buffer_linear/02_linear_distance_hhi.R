# JOBMON TASK SCRIPT: Calculate individual-level HHI using linear distance
# Maitreyi Sahu
# August 26, 2023

# REFERENCES
# Overview of CRS: https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf

# ==============================================================================

rm(list=ls())

# Setup

data_dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"

pacman::p_load(dplyr, data.table, stringr, tidyr, sf, sp, rgdal, ggplot2, arrow) 
# install.packages("swfscMisc", lib = "~/rpackages")
library(swfscMisc, lib.loc = "~/rpackages")

# Source HHI aggregation function
source("~/repos/us_value/1_data_compilation/4_hospital_hhi/00_prep_data/hhi_functions.R") 

# ------------------------------------------------------------------------------

# Jobmon arguments input
args <- commandArgs(trailingOnly = TRUE)
radius <- as.integer(args[1])
year_id <- as.integer(args[2])
hosp_type <- as.character(args[3])

current_radius <- radius 
current_year <- year_id
current_hosp_type <- hosp_type

# FOR TESTING ONLY
# current_radius <- 60
# current_year <- 2019
# current_id = "0006940058"
# current_hosp_type <- "all"

# ------------------------------------------------------------------------------

# Convert AHA data to SF object 

aha_clean_1yr <- fread(paste0(data_dir, "aha_clean_2000_2019.csv"))[
  year_id == current_year, ][
    !is.na(LONG) & !is.na(LAT) & LONG!="." & LAT !=".",
    c("ID", "MNAME", "MSTATE", "LAT", "LONG", "HOSPBD", "ADMTOT", "IPDTOT", "MCRIPD", "SYSID", "in_system", "pref_ID", "gen_surg")] 

# Subset to gen_surg hospitals only if hosp_type == "gen_surg"
if (current_hosp_type == "gen_surg") {
  aha_clean_1yr <- aha_clean_1yr[gen_surg == 1, ]
} else {
  aha_clean_1yr <- aha_clean_1yr
}

# Create SF object
aha_1yr_SF <- st_as_sf(aha_clean_1yr, 
                   coords = c("LONG", "LAT"), 
                   crs = 4326) # tells R that these coordinates should be interpreted as degrees on a sphere (WGS84)    

# ------------------------------------------------------------------------------

# FUNCTION TO GET HOSPITAL HHI USING LINEAR DISTANCE

get_hospital_hhi <- function(current_id, current_hosp_type) { 

  current_aha_1yr_SF <- aha_1yr_SF %>% filter(ID == current_id)

  current_lon = aha_clean_1yr[ID == current_id, LONG]
  current_lat = aha_clean_1yr[ID == current_id, LAT]

  # Define the buffers in meters
  current_radius_m <-swfscMisc::convert.distance(current_radius, from = c("mi"), to = c("km")) * 1000 # convert to meters

  # Function to retrieve local UTM zone based on longitude
  find_utm_zone <- function(lon) { (floor((lon + 180)/6) %% 60) + 1}
  
  # Function to retrieve EPSG code given the UTM zone [only works for northern hemisphere]
  find_epsg <- function(utm_zone) {
    if (lat > 0) {as.numeric(paste0('326', utm_zone))}
    else {NA}
    }

  # Get correct espg code given the coordinates
  lon =  st_coordinates(current_aha_1yr_SF)[,1]
  lat =  st_coordinates(current_aha_1yr_SF)[,2]
  utm_zone = find_utm_zone(lon)
  espg = find_epsg(utm_zone)
  
  current_buffer <- st_transform(current_aha_1yr_SF, 
                                 crs = espg) %>%  # transform to projection that uses meters as unit of distance, by UTM zone
    # Create buffer
  st_buffer(dist = current_radius_m) %>% 
  mutate(accessradius = current_radius)
  
  current_buffer <- st_transform(current_buffer, crs = 4326) # transform back to original projection
  
  # ----------------------------------------------------------------------------
  
  # INTERSECTIONS (using st_intersects)
  
  # Retrieve all hospital ids for that year and within ~100 miles of the primary hospital
  # LAT: 1 degree in latitude is ~69.4 miles. 100/69 = 1.45. Use 3 degrees to be safe.
  # LONG: 1 degree in longitude is cosine(latitude in radians) * length of degree at (miles) at equator. Alaska is up to 71N latitude... 100/18 = 5.55. Use 7 degrees to be safe.
  # https://www.sco.wisc.edu/2022/01/21/how-big-is-a-degree/
  
  hospitals_within_100miles <- aha_clean_1yr[between(LAT, current_lat -3, current_lat +3) & between(LONG, current_lon -7, current_lon + 7), 
                                         # filter to coords within 100 miles
                                         .(ID, MNAME, pref_ID, HOSPBD, ADMTOT, IPDTOT, MCRIPD)][, x := .I] # subset to cols of interest and add index
  
  # What hospitals intersect with that buffer?
  
  intersections <- st_intersects(x = current_buffer, 
                                 y = aha_1yr_SF %>% filter(ID %in% hospitals_within_100miles$ID)) %>% unlist()
  
  # ----------------------------------------------------------------------------
  
  # CALCULATE INDIVIDUAL HHI for each hospital 
  
  intersecting_hospitals <- hospitals_within_100miles %>%  filter(x %in% intersections) 
  
  buffer_hhi <- intersecting_hospitals %>% aggregate_hhi_buffer(current_id = current_id, current_year = current_year, current_radius = current_radius, current_hosp_type = current_hosp_type)
      
}

# ------------------------------------------------------------------------------

# RUN ACROSS ALL HOSPITALS AND BIND

start_time <- Sys.time()
buffer_hhi_combined <- bind_rows(lapply(aha_clean_1yr$ID, get_hospital_hhi, current_hosp_type = current_hosp_type))
end_time <- Sys.time() 
execution_time = end_time - start_time
print(execution_time)

# ------------------------------------------------------------------------------

# SAVE

# If directory doesn't exist, create it
mainDir <- paste0(data_dir, "buffer/linear/")
subDir <- paste0("radius=", current_radius)
dir.create(file.path(mainDir, subDir)) # doesn't overwrite if it already exists

write_feather(buffer_hhi_combined, paste0(mainDir, subDir, "/linear_hhi_", current_hosp_type, "_", current_year,  ".feather"))
