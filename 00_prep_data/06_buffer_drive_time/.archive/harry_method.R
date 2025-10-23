# MSahu
# March 20, 2024

# ABONDONED because it is 2.9 million coordinates - too many to query API

# Steps:
# Filter to hospitals within 100 miles of each other and calculate the travel times
# Then would need to use Google API queries:
# To calculate driving or public transport distance between each other: https://cran.r-project.org/web/packages/gmapsdistance/gmapsdistance.pdf

# ------------------------------------------------------------------------------

# Setup
rm(list=ls())
library(geosphere)

dir <- "/mnt/share/resource_tracking/us_value/data/"
data_dir <- paste0(dir, "hospital_hhi/processed/")
out_dir <- paste0(data_dir, "buffer/drive_time/coords_within_100miles/")

# ------------------------------------------------------------------------------

aha_clean <- fread(paste0(data_dir, "aha_clean_2000_2019.csv"))

hospital_coords_within100miles <- data.frame(
  year_id = integer(),
  LONG.x = numeric(),
  LAT.x = numeric(),
  LONG.y = numeric(),
  LAT.y = numeric()
)

for (y in 2000:2019) {

  aha_temp <- filter(aha_clean, !is.na(LONG) & !is.na(LAT) & LONG!="." & LAT !=".", year_id == y) %>% 
    select(year_id, ID, LAT, LONG) %>% mutate(distance = NA)
  
  # A full join by the distance column (which is all NAs), creates every combination of hospital in a given year
  aha_join <- full_join(aha_temp, aha_temp, by = c("year_id", "distance")) 
  
  # Identify the column numbers corresponding to the Longitude and Latitude coordinates (in that order)
  # distHaversine will calculate the meters between the points, which can be converted to miles
  miles_to_meters <- 1609.344
  aha_join$haversine_dist <- distHaversine(aha_join[4:3],aha_join[8:7])/miles_to_meters
  
  # At this point you can filter the data down to hospitals that are within a certain distance
  # of each other (say 100 miles) and use the LON/LAT coordinates to calculate drive times,
  # which can then be further filtered 
  aha_coords_yr <- filter(aha_join, haversine_dist <=100, haversine_dist!=0) %>% 
    arrange(desc(haversine_dist)) %>% 
    select(LONG.x, LAT.x, LONG.y, LAT.y) %>% unique()
  
  # Bind
  hospital_coords_within100miles <- rbind(hospital_coords_within100miles, aha_coords_yr)
  
  # Clear
  rm(aha_join, aha_temp, aha_coords_yr)
}

# ==============================================================================

# Read Harry's coordinates for 2019 to 2022
harry_coords19_22 <- fread(paste0(out_dir, "Hospital_Coordinates_Within_100_Miles_19_22.csv")) %>% 
  select(LONG.x, LAT.x, LONG.y, LAT.y)

# Bind all unique values
unique_coords_within_100_miles <- hospital_coords_within100miles %>% rbind(harry_coords19_22) %>% unique()

# Save
write.csv(unique_coords_within_100_miles, paste0(out_dir, "hospital_coords_within_100_miles_2000_2022.csv"), row.names = F)


