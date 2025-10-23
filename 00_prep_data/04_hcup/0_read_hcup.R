#===============================================================================

# Link HCUP hospitals to AHA ID [individual level]

# Author: MSahu
# Date: Aug 27, 2023

# Redo from an old script ... US_value/1_data_compilation/3_extract_covars_and_spending/5_policy.R

# NOTES on the HCUP Hospital Market Structure Files:
#   -- AVAILABLE FILES: 1997, 2000, 2003, 2006, 2009 
#   -- ONLY COVERS 40 STATES!
#   -- REFS: https://hcup-us.ahrq.gov/toolssoftware/hms/hms.jsp; https://hcup-us.ahrq.gov/toolssoftware/hms/HMSUserGuide2009.pdf 

# NOTE: I RE-DOWNLOADED 2006 and 2009 files, which had an error in the patient flow variables (emailed HCUP on 9/1/23)

#===============================================================================

rm(list=ls())

# Packages
pacman::p_load(dplyr, haven, reshape2, stringr)

# Directories for processed data
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"
out_dir <- paste0(dir, "processed/hcup/")

# HCUP Market Structure File Directories
hcup_dir <- "/ihme/limited_use/LIMITED_USE/PROJECT_FOLDERS/USA/"
policy_dir <- "/snfs1/Project/IRH/US_value/Data/policy_vars/" # HCUP Hospital Market Data was requested and saved here in 2020 --> should probably move to limited use

#===============================================================================

# READ HCUP HHI; BIND; SAVE

hcup_years <- c(1997, 2000, 2003, 2006, 2009)

hcup_hhi_1997 <- fread(paste0(hcup_dir, "HCUP_SIDS/1997/USA_HCUP_SID_1997_HOSP_MARKET_STRUCTURE.CSV"))
hcup_hhi_2000 <- fread(paste0(hcup_dir, "HCUP_SIDS/2000/USA_HCUP_SID_2000_HOSP_MARKET_STRUCTURE.CSV"))
hcup_hhi_2003 <- fread(paste0(hcup_dir, "HCUP_SIDS/2003/USA_HCUP_SID_2003_HOSP_MARKET_STRUCTURE.CSV"))
hcup_hhi_2006 <- fread(paste0(hcup_dir, "HCUP_SIDS/2006/HMS_2006/HMS_2006.csv")) # updated with the corrected files shared by HCUP
hcup_hhi_2009 <- fread(paste0(hcup_dir, "HCUP_SIDS/2009/HMS_2009/HMS_2009_cd.csv")) # updated with the corrected files shared by HCUP

hhi_cols <- c("HOSPID", "year", "state",
              "fixedradius_hhi", "patflow_hhi", "radius_75pct_hhi", "radius_90pct_hhi")
hcup_hhi <- plyr::rbind.fill(hcup_hhi_1997, hcup_hhi_2000, hcup_hhi_2003, hcup_hhi_2006, hcup_hhi_2009)[ , hhi_cols] 
colnames(hcup_hhi)[colnames(hcup_hhi) == "year"] <- "year_id"
colnames(hcup_hhi)[colnames(hcup_hhi) == "state"] <- "HOSPST"

rm(hcup_hhi_1997, hcup_hhi_2000, hcup_hhi_2003, hcup_hhi_2006, hcup_hhi_2009)

# SAVE

write.csv(hcup_hhi, paste0(out_dir, "hcup_hhi_raw.csv"), row.names = F)

