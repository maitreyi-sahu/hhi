

### NEED TO REVISE THIS SCRIPT -- CURRENTLY THIS IS JUNK ###



# Re-create Cooper estimates using 30 minute travel time
# MSahu
# August 7, 2023

# Currently, we do the following with the Cooper estimates:
#  - (i) Take the (unweighted) mean of the HHI of the hospitals within an HRR. --> Joe wants to weight by hospital beds??
# -	(ii) Take the weighted mean across HRRs using state-HRR population weight.

# =================================================================================================

# REFERENCES

# Zach Cooper paper: https://www.nber.org/system/files/working_papers/w29809/w29809.pdf

# How to calculate drive time: http://www.hrecht.com/r-drive-time-analysis-tutorial/tutorial.html

# Overview of CRS: https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf

# ==================================================================================================

# Setup
rm(list=ls())

# Set up directories
dir <- "/mnt/share/resource_tracking/us_value/data/"
in_dir <- paste0(dir, "hospital_hhi/raw/aha/AHA raw data/")
out_dir <- paste0(dir, "hospital_hhi/processed/")

# Packages
# install.packages("hereR", lib = "~/rpackages")
# install.packages("swfscMisc", lib = "~/rpackages")

library(pacman)
pacman::p_load(dplyr, stringr, tidyr, sf, sp, rgdal, ggplot2) 
library(hereR, lib.loc = "~/rpackages")
library(swfscMisc, lib.loc = "~/rpackages")

# Make sure these packages load correctly! Sometimes need to clean the environment first, as follows:

# 1. close all your rstudio sessions 
# 2. Run /ihme/singularity-images/rstudio/shells/r_env_cleaning.sh
# 3. Get a new session.

# ====================================================================================================

# DRIVING DISTANCE

# Use hereR package to get the isochrones
# This allows it to keep running even if one hospital fails

tryLocation <- function(location) {
  out <- tryCatch({
    temp <- isoline(
      poi = location,
      # range is in seconds - we want 30 minutes, so multiply by 60
      range = 30 * 60,
      range_type = "time",
      transport_mode = "car",
      url_only = F,
      optimize = "quality",
      traffic = F,
      aggregate = F
    )
    temp <- temp %>%
      mutate(hospital_id = point_id)
    return(temp)},

    error = function(cond) {
      message(paste("Hospital ID failed: ", point_id))
      message(paste(cond))
      # Choose a return value in case of error
      return(NULL)},

    warning = function(cond) {
      message(paste("Hospital ID caused a warning:", point_id))
      message(paste(cond))
      # Choose a return value in case of warning
      return(NULL)
    })
  return(out)
}


# Loop over points to make isochrones file
# Using a timer to avoid rate limit errors (Status 429)
isochrones <- NULL
error_rows <- NULL
for (i in 1:nrow(aha_subset_SF)) {
  print(i)
  # Get isochrones for that point
  # Lately the rate limiting has been more aggressive, so do an aggressive second delay
  # In the past I've used 0.15 seconds without issue but not right now
  Sys.sleep(0.4)
  # Filter to ith point
  point_temp <- aha_subset_SF %>% filter(row_number() == i)
  point_id <- point_temp$ID

  isochrones_temp <- tryLocation(point_temp)

  # If the point errored out save it
  if (is.null(isochrones_temp)) {
    error_rows <- bind_rows(error_rows, point_temp)
  } else {
    isochrones <- bind_rows(isochrones, isochrones_temp)
  }
}
rm(isochrones_temp, point_temp)
