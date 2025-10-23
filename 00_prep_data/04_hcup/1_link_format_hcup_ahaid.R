#===============================================================================

# Link HCUP hospitals to AHA ID [individual level]

# Author: MSahu
# Date: Aug 27, 2023

# NOTES on the HCUP Hospital Market Structure Files:
#   -- AVAILABLE FILES: 1997, 2000, 2003, 2006, 2009 
#   -- ONLY COVERS 40 STATES!
#   -- REFS: https://hcup-us.ahrq.gov/toolssoftware/hms/hms.jsp; https://hcup-us.ahrq.gov/toolssoftware/hms/HMSUserGuide2009.pdf 

# TO DO: 
#   -- Currently have a 65% match rate with AHAID, can try to improve with Stata DO files from 2006/2009 with other states?
#   -- check what hospital types are included in the dataset

# NOTE: if there anre multiple HOSPIDs per AHAID (~12 obs), just take the mean

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

# READ HCUP HHI

hcup_hhi <- fread(paste0(out_dir, "hcup_hhi_raw.csv"))

# ==============================================================================

# LINK TO AHA ID's (https://hcup-us.ahrq.gov/ahalinkage/ahalinkage_search.jsp)

# Function to parse asc files
read_parse_asc <- function (file) {
  
  lines <- readLines(file)
  AHAID <-  substring(lines, start_AHAID, end_AHAID)
  HOSPID <- substring(lines, start_HOSPID, end_HOSPID)
  HOSPST <- substring(lines, start_HOSPST, end_HOSPST)
  
  df <- data.frame(AHAID = as.integer(AHAID), HOSPID = as.integer(HOSPID), HOSPST = as.character(HOSPST), YEAR = as.integer(YEAR))
  return(df)  
}

parse_vars <- c("AHAID", "HOSPID", "HOSPST","YEAR")

################
##### NIS ######
################

## 1997 ## (documentation: USA_HCUP_NIS_1997_L_WEIGHT_Y2014M02D19.PDF)

YEAR = 1997
start_AHAID = 1 
end_AHAID = 7
start_HOSPID = 88 
end_HOSPID = 92
start_HOSPST = 123 
end_HOSPST = 124 

nis_97_file <- paste0(hcup_dir, "HCUP_NIS/1997_NEW/USA_HCUP_NIS_1997_HOSP_WTS_Y2014M02D19.ASC")
nis_97_list <- lapply(nis_97_file, read_parse_asc)  

## 2000 ##
nis_00 <- read_dta(paste0(hcup_dir, "HCUP_NIS/2000_NEW/NIS_2000_HOSPITAL.DTA"))[, parse_vars]  

## 2003 ##
nis_03 <- read_dta(paste0(hcup_dir, "HCUP_NIS/2003_NEW/NIS_2003_HOSPITAL.DTA"))[, parse_vars]  

## 2006 ##
nis_06 <- read_dta(paste0(hcup_dir, "HCUP_NIS/2006/NIS_2006_Hospital.dta"))[, parse_vars]   

## 2009 ##
nis_09 <- read_dta(paste0(hcup_dir, "HCUP_NIS/2009/NIS_2009_Hospital.dta"))[, parse_vars]  
# Check <- ahal_nis_09 %>% filter(HOSPST == "WY")  
# Wyoming is missing AHA ID's!

# Bind; Clean
nis <- rbind(nis_97_list[[1]], nis_00, nis_03, nis_06, nis_09) %>% 
  filter(AHAID!= "")

rm(nis_97_list, nis_97, nis_00, nis_03, nis_06, nis_09)

# ------------------------------------------------------------------------------

################
##### SIDS #####
################

# 1997 and 2000 are not available at state level!

# 2003 only has a few states in limited_use (double check if more available from HCUP?)

sids_03_dir <- paste0(hcup_dir, "HCUP_SIDS/2003/")

sids_03_files <- list.files(sids_03_dir, pattern = "AHALINK.DTA", full.names=T)
sids_03_list <- lapply(sids_03_files, read_dta)  
sids_03_combined <- do.call(rbind, sids_03_list)[ , c("ahaid", "hospid", "hospst", "year")]
colnames(sids_03_combined) <- toupper(colnames(sids_03_combined))

AZ_03 <- read_sav(paste0(hcup_dir, "HCUP_SIDS/2003/USA_AZ_HCUP_SID_2003_AHAL.SAV"))[ , parse_vars]
sids_03 <- rbind(sids_03_combined, AZ_03)

# 2006 (start/end values are given in the .loc files)

sids_06_dir <- paste0(hcup_dir, "HCUP_SIDS/2006") 
sids_06_files <- list.files(sids_06_dir, pattern = "AHAL.asc", full.names = T)

YEAR = 2006
start_AHAID = 1 
end_AHAID = 7
start_HOSPID = 21
end_HOSPID = 25
start_HOSPST = 30 
end_HOSPST = 31 

sids_06_list <- lapply(sids_06_files, read_parse_asc)  
sids_06 <- do.call(rbind, sids_06_list)

# 2009 (start/end values are given in the .loc files)

sids_09_dir <- paste0(hcup_dir, "HCUP_SIDS/2009") 
sids_09_files <- list.files(sids_09_dir, pattern = "AHAL.asc", full.names = TRUE)

YEAR = 2009
start_AHAID = 1 
end_AHAID = 7
start_HOSPID = 25
end_HOSPID = 29
start_HOSPST = 30 
end_HOSPST = 31 

sids_09_list <- lapply(sids_09_files, read_parse_asc)  
sids_09 <- do.call(rbind, sids_09_list)

# Bind; clean
sids <- rbind(sids_03, sids_06, sids_09) %>% 
  filter(AHAID!= "") 
  
rm(AZ_03, sids_03_files, sids_03_list, sids_03_combined, sids_03,
   sids_06_list, sids_09_list, sids_06, sids_09)

# ------------------------------------------------------------------------------

# CHECKS

# ID lists

hcup_ids <- rbind(nis, sids) %>%  rename(year_id = YEAR) %>% 
  mutate(ID = str_pad(AHAID, width = 10, side = "left", pad = "0")) %>% 
  distinct() 

hcup_ids_state <- hcup_ids[ , -which(names(hcup_ids) %in% c("year_id"))] %>% distinct()

hcup_ids_year <- hcup_ids[ , -which(names(hcup_ids) %in% c("HOSPST"))] %>% distinct()

hcup_ids_distinct <- hcup_ids[ , -which(names(hcup_ids) %in% c("HOSPST", "year_id"))] %>% distinct()

# Missing table and % match function

missing_checks <- function (x) {
  
  pct_match <- sum(!is.na(x$AHAID)) / nrow(x)
  print(paste0( round(pct_match * 100, 1), "% of HCUP hospitals matched with an ID"))
  
  missing_table_state <- table(x$HOSPST, is.na(x$AHAID))
  colnames(missing_table_state) <- c("Not Missing", "Missing")
  print(missing_table_state)
  
  missing_table_yr <- table(x$year_id, is.na(x$AHAID))
  colnames(missing_table_yr) <- c("Not Missing", "Missing")
  print(missing_table_yr)
}

# Join on HOSPID, YEAR, and STATE
hcup_hhi_merged <- hcup_hhi %>% left_join(hcup_ids, by = c("HOSPID", "year_id", "HOSPST"))
missing_checks(hcup_hhi_merged) # only 37.4% match
# many blank states! GA, HI, IN, KS, LA, MI, NE, NM, OH, OK, SC, SD, TN, TX, WY

# Join on HOSPID and STATE
hcup_hhi_merged <- hcup_hhi %>% left_join(hcup_ids_state, by = c("HOSPID", "HOSPST"))
missing_checks(hcup_hhi_merged) # 64.7% match; still many blank states

# JOIN ON HOSPID and YEAR
hcup_hhi_merged <- hcup_hhi %>% left_join(hcup_ids_year, by = c("HOSPID", "year_id"))
missing_checks(hcup_hhi_merged) # 46% match; many black states

# JOIN ONLY HOSPID -- BEST PERFORMING MATCH
hcup_hhi_merged <- hcup_hhi %>% left_join(hcup_ids_distinct, by = c("HOSPID"))
missing_checks(hcup_hhi_merged) # 64.8% match; many blank states and worse for 2006/2009

rm(hcup_hhi, hcup_ids, hcup_ids_state, hcup_ids_year, nis, sids)

# ------------------------------------------------------------------------------

# SAVE "BEST PERFORMING" MATCH 

hcup_hhi_merged_final <- hcup_hhi_merged %>%  
  select(ID, HOSPID, year_id, fixedradius_hhi, patflow_hhi, radius_75pct_hhi, radius_90pct_hhi)

# SAVE IDS
write.csv(hcup_ids_distinct[ , c("HOSPID", "ID")], file = paste0(out_dir, "hcup_aha_ids.csv"), row.names = F)

# SAVE LONG FORMAT - ONLY THE HOSPITALS WHICH MATCHED

hhi_vars <- c("fixed_radius", "patient_flow", "radius_75pct", "radius_90pct")

hcup_hhi_long <- hcup_hhi_merged_final %>%  
  
  # If there are multiple HCUP IDs per AHA ID, need to average (~ 12 obs)
  group_by(ID, year_id) %>% 
  summarize(fixed_radius = mean(as.numeric(fixedradius_hhi)),
            patient_flow = mean(as.numeric(patflow_hhi)),
            radius_75pct = mean(as.numeric(radius_75pct_hhi)),
            radius_90pct = mean(as.numeric(radius_90pct_hhi))) %>% 
  ungroup() %>% 
  
  # Clean up
  filter(!is.na(ID)) %>% 
  tidyr::pivot_longer(cols = c(all_of(hhi_vars)),
               names_to = "hhi_details",
               values_to = "hhi") %>% 
  mutate(hhi_type = case_when(hhi_details == "fixed_radius" ~ "hcup_fixed",
                              hhi_details == "patient_flow" ~ "hcup_pt_flow",
                              T ~ "hcup_variable"),
         market_var = "HOSPDIS",
         radius = case_when(hhi_details == "fixed_radius" ~ "15",
                            hhi_details == "radius_75pct" ~ "75",
                            hhi_details == "radius_90pct" ~ "90",
                            T ~ "75-95"),
         radius_units = ifelse(hhi_type == "fixed_radius", "miles", "pct"),
         hosp_type = "hcup") %>% # NOT SURE WHAT THIS INCLUDES
  select(ID, year_id, hhi_type, market_var, radius, radius_units, hosp_type, hhi) 

write.csv(hcup_hhi_long, file = paste0(out_dir, "hcup_hhi_individ_linked_aha.csv"), row.names = F) 