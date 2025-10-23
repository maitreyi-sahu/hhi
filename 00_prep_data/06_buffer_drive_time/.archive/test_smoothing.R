

rm(list=ls())
pacman::p_load(tidyverse, sf, maps, stringr) 
install.packages("smoothr", lib = "~/rpackages")
library(smoothr, lib.loc = "~/rpackages")

data_dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"
polygon_dir <- paste0(data_dir, "buffer/drive_time/polygons/polygons_15m/")

# read test geojson file

file1 <- "isochrone_mapbox_049967_W88_43100_N43_77780_15.geojson"
small_file1 <- "isochrone_mapbox_041760_W74_00022_N40_73678_15.geojson"

gj <- st_read(paste0(polygon_dir, file1))
gj_small <- st_read(paste0(polygon_dir, small_file1))
smoothed_gj_small <- st_simplify(gj_small, preserveTopology = TRUE, dTolerance = 0.1)

st_crs(gj_small)
st_crs(smoothed_gj_small)

usa = st_as_sf(map('usa', plot = FALSE, fill = TRUE))

ggplot() + geom_sf(data = usa) + geom_sf(data = gj, color = "red") 
ggplot() + 
  #geom_sf(data = usa) + 
  geom_sf(data = gj_small, color = "red") 

ggplot() + 
  #geom_sf(data = usa) + 
  geom_sf(data = smoothed_gj_small, color = "red") 


# read abbreviated lat/lon values and merge

#lat_lon_vals <- fread(paste0(data_dir, "buffer/drive_time/aha_geocodes_for_mapbox.csv"))
aha_clean <- fread(paste0(data_dir, "aha_clean_2000_2019.csv"))
aha_clean_rounded <- fread(paste0(data_dir, "aha_clean_2000_2019.csv")) %>% 
  mutate(LAT = str_pad(as.character(round(LAT, 5)), 5, "right"),
         LONG = as.character(round(LONG, 5)))

# PLOT - check how hospitals intersect

aha_points_check <- aha_clean %>% 
  filter(year_id ==2019) %>% 
  filter(LAT > 40.70 & LAT < 40.77 & LONG < -73.96 & LONG > -74.05) %>% 
  st_as_sf(coords = c("LONG", "LAT"), 
           crs = 4326)

ggplot() + geom_sf(data = gj_small, color = "red") + geom_sf(data = aha_points_check, color = "blue") 

ggplot() + geom_sf(data = gj_small, color = "red") + geom_sf(data = aha_points_check, color = "blue") 
ggplot() + geom_sf(data = smoothed_gj_small, color = "red") + geom_sf(data = aha_points_check, color = "blue") 


# count the intersections

intersections <- length(st_intersects(x = gj_small, y = aha_points_check) %>% unlist())
intersections

intersections <- length(st_intersects(x = smoothed_gj_small, y = aha_points_check) %>% unlist())
intersections

# DON'T SMOOTH - get the same number of intersections. actually we should add a buffer of 100-200m


# check if polygon exists for each of the values in the AHA file - create missing file if not

missing_geocodes <- aha_clean_rounded %>% select(year_id, ID, LAT, LONG) 

# 

# Function to extract LONG and LAT from filename
extract_coordinates <- function(filename) {
  pattern <- "isochrone_mapbox_\\d+_(W|E)(-?\\d+\\.\\d+)_(N|S)(-?\\d+\\.\\d+)_\\d+\\.geojson"
  match <- regmatches(filename, regexpr(pattern, filename, perl = TRUE))
  
  if (length(match) > 0) {
    parts <- strsplit(match[[1]], "_")[[1]]
    direction_lon <- parts[3]
    lon <- as.numeric(parts[4]) * ifelse(direction_lon == "W", -1, 1)
    
    direction_lat <- parts[5]
    lat <- as.numeric(parts[6]) * ifelse(direction_lat == "S", -1, 1)
    
    return(c(lon = lon, lat = lat))
  } else {
    return(NULL)
  }
}

# Iterate through files in the directory
files <- list.files(path = polygon_dir, pattern = "\\.geojson$", full.names = TRUE)
for (file in files) {
  coordinates <- extract_coordinates(file)
  if (!is.null(coordinates)) {
    lon <- coordinates["lon"]
    lat <- coordinates["lat"]
    cat("File:", basename(file), "LONG:", lon, "LAT:", lat, "\n")
  } else {
    cat("Invalid filename format:", basename(file), "\n")
  }
}



# aggregate  


ggplot()