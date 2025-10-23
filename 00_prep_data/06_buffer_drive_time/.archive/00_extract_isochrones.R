# Extract AHA geocodes for mapbox 
# Sep 15, 2023
# MSahu

rm(list=ls())

# Directories 
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"


# annoying debugging for mapbox
detach(package:tidyr,unload=TRUE) 
#install.packages("tidyr", lib = "~/rpackages")
library(tidyr, lib.loc = "~/rpackages")

# ------------------------------------------------------------------------------

# Get list of AHA ID's and their geocodes

aha_geocodes <- fread(paste0(dir, "aha_clean_2000_2019.csv")) %>% 
  select(ID, LAT, LONG) %>%  distinct() %>% 
  mutate(coordinates = paste0("(", LAT, ",", LONG, ")")) %>%  select(-LAT, -LONG)

write.csv(aha_geocodes, paste0(dir, "buffer/drive_time/aha_geocodes_for_mapbox.csv"), row.names = F)

# ------------------------------------------------------------------------------

# MAPBOX - get isochrones (https://rdrr.io/cran/mapboxapi/man/mb_isochrone.html)



# install.packages("mapboxapi", lib = "~/rpackages")
library(mapboxapi, lib.loc = "~/rpackages")

# install.packages("mapdeck", lib = "~/rpackages")
library(mapdeck, lib.loc = "~/rpackages")

msahu_token <- "pk.eyJ1IjoibWFpdHJleWk5MTQ3IiwiYSI6ImNsbWxhZHN2eTA4eHYyb3Bia3MzaGZ1bHoifQ.iXruYTlCiB5v5fHQKMUx0g"

mb_isochrone(
  location,
  profile = "driving",
  time = c(15, 30, 60),
  distance = NULL,
  depart_at = NULL, # will reflect the driving conditions at the time this is run
  access_token = msahu_token,
  denoise = 1,
  generalize = NULL,
  geometry = "polygon",
  output = "sf",
  rate_limit = 300,
  keep_color_cols = FALSE,
  id_column = ID
)

mapdeck(style = mapdeck_style("light")) %>%
  add_polygon(
    data = isochrones,
    fill_colour = "time",
    fill_opacity = 0.5,
    legend = TRUE
  )