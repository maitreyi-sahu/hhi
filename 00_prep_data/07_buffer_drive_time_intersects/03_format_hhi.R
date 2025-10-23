# Maitreyi Sahu
# Aug 27, 2023

# Format the driving distance INTERSECTS hhi so it's consistent with the following vars:
# ID, YEAR, hhi_type, market_var, radius, radius_units, hhi

# Output file: "linear_distance_hhi_formatted.csv" 

# ------------------------------------------------------------------------------

rm(list=ls())
pacman::p_load(dplyr, stringr)

dir <- "/mnt/share/resource_tracking/us_value/data/"
out_dir <- paste0(dir, "hospital_hhi/processed/")

# ------------------------------------------------------------------------------
