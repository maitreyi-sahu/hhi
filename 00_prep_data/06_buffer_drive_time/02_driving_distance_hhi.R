# JOBMON TASK SCRIPT: Calculate individual-level HHI using driving distance
# Maitreyi Sahu
# March 22, 2023

# ==============================================================================

rm(list=ls())

# Setup
pacman::p_load(dplyr, data.table, stringr, tidyr, sf, sp, rgdal, ggplot2, arrow) 

data_dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"
polygon_dir <- paste0(data_dir, "buffer/drive_time/polygons/")

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

# Read AHA data and format
aha_cols <- c("ID", "MNAME", "MSTATE", "LAT", "LONG", "SERV", "HOSPBD", "ADMTOT", "IPDTOT", "MCRIPD", "SYSID", "in_system", "pref_ID", "gen_surg")
aha_clean_rounded_1yr <- fread(paste0(data_dir, "aha_clean_2000_2019.csv"))[year_id == current_year, ][, ..aha_cols][, `:=` (LAT = round(LAT, 5), LONG = round(LONG, 5))] 
  
# Subset to gen_surg hospitals only if hosp_type == "gen_surg"
if (current_hosp_type == "gen_surg") {
   aha_clean_rounded_1yr <- aha_clean_rounded_1yr[gen_surg == 1, ]
} else {
   aha_clean_rounded_1yr <- aha_clean_rounded_1yr
}

# Create SF object
aha_sf_1yr <- st_as_sf(aha_clean_rounded_1yr, coords = c("LONG", "LAT"), crs = 4326) # WGS84 / spherical    

# Polygon file names
current_polygon_dir <- paste0(polygon_dir, "polygons_", current_radius, "m/")
geojson_file_names <- c(list.files(current_polygon_dir, pattern = "\\.geojson$", full.names = F))

# ==============================================================================
#
# FUNCTION TO GET HHI FOR EACH HOSPITAL USING DRIVE-TIME POLYGONS FROM MAPBOX
#
#  1. get drive-time polygon 
#  2. add 200m buffer (to catch nearby points that may be missed by the polygon)
#  3. add bounding box of ~100 miles (to save run time)
#  3. intersect with hospital coordinates within bounding box and find overlap
#  4. for overlapping hospitals, calculate HHI [using HHI function]
#
#  Inputs: 
#    - One year of hospital data (to get coords for Hospital ID)
#    - Drive time polygon
#  
#  Output: Individual hospital-level HHI for the specified hospital-year-radius  
#
#===============================================================================

get_hospital_hhi <- function(current_id, current_hosp_type) {
  
   ###########################################
   # 1. READ CORRESPONDING DRIVE-TIME POLYGON
   ###########################################
  
   # Get LON/LAT values for hospid == aha_id and pad string to 5 characters
   current_lon = aha_clean_rounded_1yr[ID == current_id, LONG]
   current_lat = aha_clean_rounded_1yr[ID == current_id, LAT]
   
   print(paste0("LONG = ", current_lon, ", LAT = ", current_lat)) # for error traceback
   
   current_lon_string = ifelse(current_lon >=0, 
                                  paste0("E", sprintf("%.5f", abs(current_lon))),
                                  paste0("W", sprintf("%.5f", abs(current_lon))))
   current_lat_string = ifelse(current_lat >=0, 
                                  paste0("N", sprintf("%.5f", abs(current_lat))),
                                  paste0("S", sprintf("%.5f", abs(current_lat))))
   
   # Get corresponding pattern for isochrone file names
   current_pattern_coords = paste0(gsub(current_lon_string, pattern = "\\.", replacement = "_"),
                                   "_",
                                   gsub(current_lat_string, pattern = "\\.", replacement = "_"))
   matching_files <- grep(current_pattern_coords, geojson_file_names, value = TRUE)
   
   # Read the isochrone file
   if (any(length(matching_files) > 0)) {
     
     drive_time_polygon <- st_read(paste0(current_polygon_dir, matching_files[1]), quiet = T) # this is dirty - if multiple matching isochrones, we just select the first one (happens because I extracted isochrones multiple times and they have diff filenames)  
  
   } else { print("No matching polygon found for this hospital ID.")}
  
   
   ### NEXT STEPS ARE ONLY RUN IF POLYGON IS FOUND ###
   
   try({
   
       #############################################
       # 2. ADD 200M BUFFER (to catch nearby points)
       #############################################
        
       add_buffer <- function(polygon, buffer_size) {
          
         polygon_in_3857 <- st_transform(polygon, crs = 3857) # transform to different projection that uses meters as unit of distance
         polygon_with_buffer <- st_buffer(polygon_in_3857, buffer_size)
         polygon_with_buffer <- st_transform(polygon_with_buffer, crs = 4326) # transform back to WGS84 projection
       }     
    
       drive_time_polygon_with_buffer <- add_buffer(drive_time_polygon, 200)
       
       #######################################
       # 3. ADD BOUNDING BOX (to save runtime)
       #######################################
       
       # Get all hospital IDs within ~100 miles of the primary hospital
       # LAT: 1 degree in latitude is ~69.4 miles. 100/69 = 1.45. Use 3 degrees to be safe.
       # LONG: 1 degree in longitude is cosine(latitude in radians) * length of degree at (miles) at equator. Alaska is up to 71N latitude... 100/18 = 5.55. Use 7 degrees to be safe.
       # https://www.sco.wisc.edu/2022/01/21/how-big-is-a-degree/
       
       hosp_ids_within_100miles <- aha_clean_rounded_1yr[between(LAT, current_lat -3, current_lat +3) & between(LONG, current_lon -7, current_lon + 7), # filter to coords within 100 miles
                                                         .(ID, MNAME, pref_ID, HOSPBD, ADMTOT, IPDTOT, MCRIPD)][, x := .I] # subset to cols of interest and add index
       
       ###############################################
       # 4. FIND HOSPITALS WHICH OVERLAP WITH POLYGON
       ###############################################
       
       intersections <- st_intersects(x = drive_time_polygon_with_buffer, y = aha_sf_1yr %>% filter(ID %in% hosp_ids_within_100miles$ID)) %>% unlist()
       
       hosp_ids_within_100miles <- hosp_ids_within_100miles %>% mutate(intersects = ifelse(x %in% intersections, T, F))
       
      #################################################
      # 5. CALCULATE HHI  [using sourced HHI function]
      #################################################
       
       intersecting_hospitals <- hosp_ids_within_100miles %>%  filter(x %in% intersections) 
       
       buffer_hhi <- intersecting_hospitals %>% aggregate_hhi_buffer(current_id = current_id, current_year = current_year, current_radius = current_radius, current_hosp_type = current_hosp_type)
  
   })
   
   if(exists("buffer_hhi")) {
     
     buffer_hhi <- buffer_hhi
     
   } else{
    
    # If no polygon is found or polygon is wonky, return NA
    buffer_hhi <- data.table(year_id = current_year, ID = current_id, 
                             hhi_beds = NA, hhi_admits = NA, hhi_pt_days = NA, hhi_mcr_pt_days = NA,
                             tot_beds = NA, tot_admits = NA, tot_pt_days = NA, tot_mcr_pt_days = NA,
                             aha_hosp_n = NA,
                             aha_systems_n = NA, 
                             radius = current_radius,
                             hosp_type = current_hosp_type)
    }
} 

# ------------------------------------------------------------------------------

# RUN ACROSS ALL HOSPITALS AND BIND

start_time <- Sys.time()
buffer_hhi_combined <- bind_rows(lapply(aha_clean_rounded_1yr$ID, get_hospital_hhi, current_hosp_type = current_hosp_type))
end_time <- Sys.time() 
execution_time = end_time - start_time
print(execution_time)

# ------------------------------------------------------------------------------

# SAVE

# If directory doesn't exist, create it
mainDir <- paste0(data_dir, "buffer/drive_time/")
subDir <- paste0("radius=", current_radius)
dir.create(file.path(mainDir, subDir)) # doesn't overwrite if it already exists

write_feather(buffer_hhi_combined, paste0(mainDir, subDir, "/drive_time_hhi_", current_hosp_type, "_", current_year,  ".feather"))
