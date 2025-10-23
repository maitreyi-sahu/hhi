# ASHE ABSTRACT 2024
# MSahu
# Dec 11, 2023

# adapted from "07_compile_hhi"

rm(list=ls())

# Packages
pacman::p_load(tidyr, dplyr) 

# Directories 
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"

#===============================================================================

# 1) INDIVIDUAL

# Load files 
hanson_hrr <- fread(paste0(dir, "hanson/hhi_individ_hanson_long.csv"))
cooper_data <- fread(paste0(dir, "cooper/cooper_hhi_formatted.csv"))
buffer_linear <- fread(paste0(dir, "buffer/linear_crs3857/linear_distance_hhi_formatted.csv"))

ids <- fread(paste0(dir, "aha_clean_ids.csv")) %>% select(ID, year_id, hrrnum) %>% distinct()

# Append rows and join the HRR 
hhi_individ <- rbind(hanson_hrr, 
                     # hcup_data, 
                     buffer_linear)  %>% 
  left_join(ids, by = c("ID", "year_id")) %>%  # join the HRR numbers
  rbind(cooper_data) %>%  # Cooper data already has the hrrnum
  mutate(hhi = as.numeric(hhi)) %>% 
  select(ID, hrrnum, year_id, hhi_type, market_var, radius, radius_units, hhi, hosp_type) %>% distinct()

rm(hanson_hrr, cooper_data, 
   #hcup_data, 
   buffer_linear
   #, hcup, hanson, drive time
)  

#-------------------------------------------------------------------------------

# 2) HRR [unweighted mean of individual]

hhi_hrr <- hhi_individ %>% 
  group_by(hrrnum, year_id, hhi_type, market_var, radius, radius_units, hosp_type) %>% 
  summarize(hrr_hhi = mean(hhi, na.rm = T))

#-------------------------------------------------------------------------------

# 3) Create summary table - HRR level - for table / percentages

hhi_hrr_summary <- hhi_hrr %>% 
  filter(year_id %in% c(2000, 2019)) %>% 
  mutate(highly_concentrated = ifelse(hrr_hhi > 2500, 1, 0),
         mod_concentrated = ifelse(hrr_hhi>1500, 1, 0)) %>% 
  group_by(year_id, hhi_type, market_var, radius, radius_units, hosp_type) %>% 
  summarize(pct_hi_concentrated = round(mean(highly_concentrated)*100, 1),
            pct_mod_or_hi_concentrated =  round(mean(mod_concentrated)*100, 1))

table <- hhi_hrr_summary %>% 
  filter(hosp_type == "all", market_var == "IPDTOT")

# 4) Create summary table - individual level - for market share

# Calculate coefficient of variation by group (hrr, 15, 30, or 50)
cv_by_market_size <- hhi_individ %>%
  filter(hhi_type!= "cooper_drive_time_08_14") %>% 
  group_by(year_id, market_var, hosp_type) %>%
  summarize(CV = (sd(hhi) / mean(hhi)) * 100)  %>% 
  filter(market_var == "IPDTOT")

# Calculate coefficient of variation by group (market share type)
cv_by_market_share <- hhi_individ %>%
  filter(hhi_type!= "cooper_drive_time_08_14") %>% 
  group_by(year_id, radius, hosp_type) %>%
  summarize(CV = (sd(hhi, na.rm = T) / mean(hhi, na.rm = T)) * 100)  %>% 
  filter(radius == "30")