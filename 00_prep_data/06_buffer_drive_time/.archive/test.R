rm(list=ls())

library(geojsonsf)
library(sf)

polygon_dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/buffer/drive_time/polygons/polygons_"
drive_times <- c(15, 30, 60)  

# test a couple isochrone files
test1 <- geojson_sf(paste0(polygon_dir, drive_times[1], "m/isochrone_mapbox_050584_W80_83000_N38_29250_15.geojson"))
test2 <- geojson_sf(paste0(polygon_dir, drive_times[1], "m/isochrone_mapbox_025566_W87_54820_N33_20910_15.geojson"))
object.size(test)

length(geojson_files_15m)

# bind all and convert to shape file - should take around 20 min per file
geojson_files_15m <- list.files(paste0(polygon_dir, "15m/"))[1:100]

# read_geojson(geojson_files_15m[1], 15)
start_time <- Sys.time()
combined_sf_15m <- lapply(geojson_files_15m, function(f){
  sf <- geojson_sf(paste0(polygon_dir, "15m/", f))
  return(sf)
})
combined_sf_15m <- rbindlist(combined_sf_15m)



# Convert lat and lon degrees to miles - only check files that are within 100 miles of each other


geojson_files_15m <- list.files(paste0(polygon_dir, "15m/"))
object.size(combined_sf_15m)


# Workflow

# First, check that all hospid's have an associated file

# 
# 2: TASK SCRIPT
  #  for each hospital,
  #  get lat/lon for hosp
  #  open geojson file with: that hospid and any hospid's within 100 miles 


# Restrict the bounding box to ~100 miles of the hospid
# LATITUDE: 1 degree in latitude is ~ 69 miles. Use 3 degrees difference to be safe
# LONGITUDE: 1 degree in longitude is cosine(latitude in radians) * length of degree at (miles) at equator. Use 5 degrees to be safe

# intersect the buffers 
# calculate HHI