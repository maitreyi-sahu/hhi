rm(list=ls())

#install.packages("tidyr")
#library(Rtools)
library(tidyr)
library(dplyr)

#install.packages("mapboxapi")
library(mapboxapi)

# ------------------------------------------------------------------------------

# set token (not sure which one worked)

#usethis::edit_r_environ()

#set_mapbox_api('pk.eyJ1IjoibWFpdHJleWk5MTQ3IiwiYSI6ImNsbWxhZHN2eTA4eHYyb3Bia3MzaGZ1bHoifQ.iXruYTlCiB5v5fHQKMUx0g')

msahu_token <- "pk.eyJ1IjoibWFpdHJleWk5MTQ3IiwiYSI6ImNsbWxhZHN2eTA4eHYyb3Bia3MzaGZ1bHoifQ.iXruYTlCiB5v5fHQKMUx0g"
mb_access_token(msahu_token, install = TRUE, overwrite = T)
Sys.getenv('MAPBOX_PUBLIC_TOKEN')

# ------------------------------------------------------------------------------

# import

geocodes <- fread("C:/Users/msahu/Desktop/aha_geocodes_for_mapbox.csv")  %>% 
  mutate(coordinates = paste0(LONG, ",", LAT)) 

# First Value

isochrones <- mb_isochrone(
  geocodes$coordinates[1],
  profile = "driving",
  time = 1,
  distance = NULL,
  depart_at = NULL, # doesn't account for traffic
  denoise = 1,
  generalize = NULL,
  geometry = "polygon",
  output = "sf",
  rate_limit = 300,
  keep_color_cols = FALSE,
  id_column = "ID" )

isochrones$ID = NA
isochrones$LONG = NA
isochrones$LAT = NA

# Now loop through and bind

for (i in 1:nrow(geocodes)) {

isochrones_temp <- mb_isochrone(
  geocodes$coordinates[i],
  profile = "driving",
  time = c(15, 30, 60),
  distance = NULL,
  depart_at = NULL, # doesn't account for traffic
  denoise = 1,
  generalize = NULL,
  geometry = "polygon",
  output = "sf",
  rate_limit = 300,
  keep_color_cols = FALSE,
  id_column = "ID" )

  isochrones_temp$ID = geocodes$ID[i] 
  isochrones_temp$LONG =  geocodes$LONG[i] 
  isochrones_temp$LAT =  geocodes$LAT[i] 
  
  isochrones <- rbind(isochrones, isochrones_temp)
  
}

# Delete top value

isochrones <- isochrones %>%  filter(time != 1)

# Still need to save!

st_write(isochrones, "C:/Users/msahu/Desktop/isochrones.R")
