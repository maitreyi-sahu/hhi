# Plot the linear distance buffers
# MSahu
# August 26, 2023

# =================================================================================================

# Make sure these packages load correctly! Sometimes need to clean the environment first, as follows:

# 1. close all your rstudio sessions 
# 2. Run /ihme/singularity-images/rstudio/shells/r_env_cleaning.sh
# 3. Get a new session.

# Setup
rm(list=ls())

# Directories
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"
out_dir <- paste0(dir, "plots/buffers/")

# Packages
# install.packages("swfscMisc", lib = "~/rpackages")
pacman::p_load(dplyr, stringr, tidyr, sf, sp, rgdal, ggplot2) 
library(swfscMisc, lib.loc = "~/rpackages")

# ====================================================================================================

aha_clean <- fread(paste0(dir, "processed/aha_clean_2000_2019.csv")) %>% 
  filter(!is.na(LONG) & !is.na(LAT) & LONG!="." & LAT !=".") %>% 
  select(year_id, ID, MNAME, MSTATE, LAT, LONG, 
         HOSPBD, ADMTOT, IPDTOT, 
         SYSID, in_system, pref_ID)

# Create SF object
aha_SF <- st_as_sf(aha_clean, 
                   coords = c("LONG", "LAT"), 
                   crs = 4326) # tells R that these coordinates should be interpreted as degrees on a sphere (WGS84)    
aha_SF <- st_transform(aha_SF, crs = 3082) # now set to equal area lambert projection (use st_crs to double check)

# -----------------------------------------------------------------------------

# LINEAR DISTANCE  (Adapted from Rachel W's code)

# Use the Lambert projection (EPSG 3082)
# This conformal projection preserves angles between locations in small areas at the expense of distorting area.
# https://michaelminn.net/tutorials/r-projections/index.html

# Define the buffers (15, 30, and 50 mile radius)

radius_m <-swfscMisc::convert.distance(c(15, 30, 50), from = c("mi"), to = c("km")) * 1000 # convert to meters

buffers <- st_transform(aha_SF, crs = 3857) # transform to different projection that uses meters as unit of distance

buffers_15mi <- buffers %>%  st_buffer(dist = radius_m[1]) %>% mutate(accessradius = 15) # 24140.16 is 15 mi in meters
buffers_30mi <- buffers %>%  st_buffer(dist = radius_m[2]) %>% mutate(accessradius = 30) # 48280.32 is 30 mi in meters
buffers_50mi <- buffers %>%  st_buffer(dist = radius_m[3]) %>% mutate(accessradius = 50)# 80467.20 is 50 mi in meters

#transform back to Lambert projection (ESPG 3082)
buffers_15mi <- st_transform(buffers_15mi, crs = 3082) 
buffers_30mi <- st_transform(buffers_30mi, crs = 3082)
buffers_50mi <- st_transform(buffers_50mi, crs = 3082)

# ----------------------------------------------------------------------------------------

# Plotting to check the buffer locations - use map that includes actual location of Hawaii/AK

library(maps)
library(plotly)
usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

buffers_15mi_map <- st_transform(buffers_15mi, crs = usmap_crs()) %>%  filter(year_id == 2019)
buffers_30mi_map <- st_transform(buffers_30mi, crs = usmap_crs()) %>%  filter(year_id == 2019)
buffers_50mi_map <- st_transform(buffers_50mi, crs = usmap_crs()) %>%  filter(year_id == 2019)
aha_SF_map <- st_transform(aha_SF, crs = usmap_crs()) %>%  filter(year_id == 2019) 

p <- ggplot(usa) +
  geom_sf(color = "#2b2b2b", fill = "white", size=0.125) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) + # Lambert equal area projection
  ggthemes::theme_map() +
  geom_sf(data = buffers_15mi_map, fill = "paleturquoise3", alpha = 0.6) +
  geom_sf(data = aha_SF_map, color = "darkred", size = .05) +
  ggtitle("Hospital locations and 15 mile buffers, 2019")

ggsave(p, file = paste0(out_dir , "linear_buffer_15_mile.pdf"))

p <- ggplot(usa) +
  geom_sf(color = "#2b2b2b", fill = "white", size=0.125) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) + # Lambert equal area projection
  ggthemes::theme_map() +
  geom_sf(data = buffers_30mi_map, fill = "paleturquoise3", alpha = 0.6) +
  geom_sf(data = aha_SF_map, color = "darkred", size = .05) +
  ggtitle("Hospital locations and 30 mile buffers, 2019")

ggsave(p, file = paste0(out_dir , "linear_buffer_30_mile.pdf"))


p <- ggplot(usa) +
  geom_sf(color = "#2b2b2b", fill = "white", size=0.125) +
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) + # Lambert equal area projection
  ggthemes::theme_map() +
  geom_sf(data = buffers_50mi_map, fill = "paleturquoise3", alpha = 0.6) +
  geom_sf(data = aha_SF_map, color = "darkred", size = .05) +
  ggtitle("Hospital locations and 50 mile buffers, 2019")

ggsave(p, file = paste0(out_dir , "linear_buffer_50_mile.pdf"))


