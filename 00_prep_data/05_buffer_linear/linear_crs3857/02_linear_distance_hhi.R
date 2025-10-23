# JOBMON TASK SCRIPT: Calculate individual-level HHI using linear distance
# Maitreyi Sahu
# August 26, 2023

# REFERENCES
# Zach Cooper paper: https://www.nber.org/system/files/working_papers/w29809/w29809.pdf
# Overview of CRS: https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf
# How to calculate drive time: http://www.hrecht.com/r-drive-time-analysis-tutorial/tutorial.html

# ==============================================================================

rm(list=ls())

# Setup

dir <- "/mnt/share/resource_tracking/us_value/data/"
in_dir <- paste0(dir, "hospital_hhi/raw/aha/AHA raw data/")
out_dir <- paste0(dir, "hospital_hhi/processed/")

pacman::p_load(dplyr, stringr, tidyr, sf, sp, rgdal, ggplot2, arrow) 
# install.packages("swfscMisc", lib = "~/rpackages")
library(swfscMisc, lib.loc = "~/rpackages")

# ------------------------------------------------------------------------------

# Jobmon arguments input

args <- commandArgs(trailingOnly = TRUE)
radius <- as.integer(args[1])
hospital_id <- as.character(args[2])
year <- as.integer(args[3])

current_radius <- radius 
current_id <- hospital_id
current_year <- year

# ------------------------------------------------------------------------------

# Convert AHA data to SF object (MAY NEED TO DO ADDITIONAL CLEANING)

aha_clean <- fread(paste0(out_dir, "aha_clean_2000_2019.csv")) %>% 
  filter(!is.na(LONG) & !is.na(LAT) & LONG!="." & LAT !=".") %>% 
  select(YEAR, ID, MNAME, MSTATE, LAT, LONG, 
         HOSPBD, ADMTOT, IPDTOT, 
         SYSID, in_system, pref_ID)

# Create SF object
aha_SF <- st_as_sf(aha_clean, 
                   coords = c("LONG", "LAT"), 
                   crs = 4326) # tells R that these coordinates should be interpreted as degrees on a sphere (WGS84)    
aha_SF <- st_transform(aha_SF, crs = 3082) # now set to equal area lambert projection (use st_crs to double check)

current_aha_SF <- aha_SF %>% filter(YEAR == current_year) %>% filter(ID == current_id)

# ------------------------------------------------------------------------------

# CREATE BUFFER USING LINEAR DISTANCE (Adapted from Rachel W's code)

# Use the Lambert projection (EPSG 3082)
# This conformal projection preserves angles between locations in small areas at the expense of distorting area.
# https://michaelminn.net/tutorials/r-projections/index.html

# Define the buffers in meters

current_radius_m <-swfscMisc::convert.distance(current_radius, from = c("mi"), to = c("km")) * 1000 # convert to meters

current_buffer <- st_transform(current_aha_SF, crs = 3857) %>%  # transform to different projection that uses meters as unit of distance
  
  # Create buffer
  st_buffer(dist = current_radius_m) %>% 
  mutate(accessradius = current_radius)

current_buffer <- st_transform(current_buffer, crs = 3082) #transform back to Lambert projection (ESPG 3082)

# ------------------------------------------------------------------------------

# INTERSECTIONS (using st_intersects)

# Retrieve all hospital ids for that year

hosp_ids <-  aha_SF %>% 
  filter(YEAR == current_year ) %>% 
  select(ID, MNAME, pref_ID, IPDTOT, HOSPBD) %>% 
  mutate(x = row_number()) %>% 
  st_drop_geometry()

# What hospitals intersect with that buffer?

intersections <- st_intersects(x = current_buffer, y = aha_SF %>% filter(YEAR == current_year)) %>% unlist()

# ------------------------------------------------------------------------------

# CALCULATE INDIVIDUAL HHI for each hospital using hospital beds for market share (Cooper's method)

buffer_hhi <- hosp_ids %>%  filter(x %in% intersections) %>% 
  
  # Total hospital beds and patient days
  mutate(tot_beds = sum(HOSPBD),
         tot_pt_days = sum(IPDTOT)) %>% 
  
  # Hospital beds for the system
  group_by(pref_ID) %>% 
  summarize(system_beds = sum(HOSPBD),
            system_pt_days = sum(IPDTOT),
            tot_beds = mean(tot_beds),
            tot_pt_days = mean(tot_pt_days)) %>% 
  
  # Market share as a readable percentage
  mutate(market_share_beds = system_beds / tot_beds * 100,
         market_share_pt_days = system_pt_days / tot_pt_days * 100) %>% data.frame() %>% 
  
  # collapse again to calculate HHI
  summarize(hhi_beds = sum(market_share_beds^2),
            hhi_pt_days = sum(market_share_pt_days^2),
            tot_beds = mean(tot_beds),
            tot_pt_days = mean(tot_pt_days),
            aha_systems = n()) %>% 
  
  # add ID and year columns
  mutate(ID = current_id,
         YEAR = current_year,
         radius = current_radius) %>% 
  select(ID, YEAR, everything()) #reorder

# ------------------------------------------------------------------------------

# SAVE

# If directory doesn't exist, create it

mainDir <- paste0(out_dir, "buffer/linear/")
subDir <- paste0("radius=", current_radius, "/year=", current_year)
dir.create(file.path(mainDir, subDir)) # doesn't overwrite if it already exists

write_feather(buffer_hhi, paste0(mainDir, subDir, "/hhi_", current_id, ".feather"))