# Get isochrones for missing geocodes
# MSahu
# March 23, 2024

# THIS IS RUN LOCALLY BECAUSE DIFFICULTY INSTALLING MAPBOX API  
# https://docs.traveltime.com/api/sdks/r
# traveltimeR (and probably also mapbox api) uses rprotobuf as a dependency. 
# If package installation fails, please make sure you have the system requirements covered for rprotobuf

# ==============================================================================

# install.packages("mapboxapi")
pacman::p_load(mapboxapi, tidyverse, data.table, sf)

mb_token = "pk.eyJ1IjoibWFpdHJleWk5MTQ3IiwiYSI6ImNsbWxhZHN2eTA4eHYyb3Bia3MzaGZ1bHoifQ.iXruYTlCiB5v5fHQKMUx0g"

dir <- "C:/Users/msahu/Desktop/"

# ==============================================================================

# read files
read_geocode_files <- function (drive_time) {

  temp = fread(paste0(dir, "missing_geocodes_for_mapbox", drive_time, "m.csv"))[ , drive_time := drive_time]

  return(temp)
}

drive_times = c(15, 30, 60)
#drive_times = 60
missing_geocodes =  do.call(bind_rows, lapply(drive_times, read_geocode_files))

# read file from Harry
# missing_geocodes <- fread(paste0(dir, "Missing_Coordinates.csv")) %>% 
#   rename(drive_time = Drivetime)

# ==============================================================================

# loop through missing geocodes and get isochrones
# NOTE: this needs to be done in a loop - can't pass the vector of coords because this function can't submit batch jobs

for (i in 1:nrow(missing_geocodes)) {
  
  print(paste0("LONG: ", missing_geocodes$LONG[i], ", LAT: ", missing_geocodes$LAT[i], ", TIME: " , missing_geocodes$drive_time[i]))
  
  suppressWarnings(rm(temp, isochrone_sf_row))
  
  tryCatch( # keep going if there are errors
    
    temp <- mb_isochrone(
      location = c(missing_geocodes$LONG[i], missing_geocodes$LAT[i]),
      profile = "driving",
      time = missing_geocodes$drive_time[i],
      distance = NULL,
      depart_at = NULL,
      access_token = mb_token,
      denoise = 1,
      generalize = NULL,
      geometry = "polygon",
      output = "sf",
      rate_limit = 500,
      keep_color_cols = FALSE,
      id_column = NULL), error = function(e){})
  
  if (exists("temp")) {
    
    isochrone_sf_row = st_sf(
                        index = i,
                        LAT = missing_geocodes$LAT[i],
                        LONG = missing_geocodes$LONG[i],
                        drive_time = missing_geocodes$drive_time[i],
                        geometry = temp$geometry, crs = 4326)
    
    st_write(isochrone_sf_row, paste0(dir, 
                                      "missing_polygons/isochrone_mapbox_",
                                      sprintf("%05d", as.integer(i)), "_",
                                      "W", gsub(sprintf("%.5f", abs(missing_geocodes$LONG[i])), pattern = "\\.", replacement = "_"), 
                                      "_",
                                      "N", gsub(sprintf("%.5f", abs(missing_geocodes$LAT[i])), pattern = "\\.", replacement = "_"), 
                                      "_",
                                      missing_geocodes$drive_time[i],
                                      ".geojson"), driver = "GeoJSON", append = F)
  }
}

#check <- st_read(paste0(dir, "missing_polygons/isochrone_mapbox_W149_87896_N61_17723_60.geojson"))
