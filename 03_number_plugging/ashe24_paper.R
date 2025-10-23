# ASHE PAPER 2024
# MSahu
# June 16, 2024

rm(list=ls())

# Packages
pacman::p_load(tidyr, dplyr) 

# Directories 
compiled_dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/compiled_hhi/"


# source columns of interest and labels
source("~/repos/us_value/1_data_compilation/4_hospital_hhi/02_plots/plot_functions.R")

cols_of_interest <- c(custom_vars$cols, 
                      "hcup_variable_HOSPDIS_75_pct_hcup", "hcup_variable_HOSPDIS_90_pct_hcup", "hcup_pt_flow_HOSPDIS_75-95_pct_hcup")



#===============================================================================

# 1) STATE + summary percentages

hhi_state_wide <- read_feather(paste0(compiled_dir, "compiled_hhi_state_wide.feather")) %>%  select(-location_id, -location_name) %>% setDT() %>%  
  select(postal_code, year_id, all_of(cols_of_interest))

hhi_state_summary <- hhi_state_wide %>% filter(year_id %in% c(2000, 2009, 2019, 2022)) %>% 
  melt(id.vars = c("postal_code", "year_id"), measure.vars = cols_of_interest, variable.name = "hhi_type", value.name = "hhi") %>%
  mutate(highly_concentrated = ifelse(hhi > 2500, 1, 0),
         mod_concentrated = ifelse(hhi>1500, 1, 0)) %>% 
  group_by(year_id, hhi_type) %>% 
  summarize(pct_hi_concentrated = mean(highly_concentrated),
            pct_mod_or_hi_concentrated =  mean(mod_concentrated))
write.csv(hhi_state_summary, paste0(plot_dir, "time_trends/state_summary_table_ashe.csv"), row.names = F)
  
#-------------------------------------------------------------------------------

# 2) HRR + summary percentages

hhi_hrr_wide <- read_feather(paste0(compiled_dir, "compiled_hhi_hrr_wide.feather")) %>% setDT()

hhi_hrr_summary <- hhi_hrr_wide %>% filter(year_id %in% c(2000, 2009, 2019, 2022)) %>% 
  melt(id.vars = c("hrrnum", "year_id"), measure.vars = cols_of_interest, variable.name = "hhi_type", value.name = "hhi") %>%
  mutate(highly_concentrated = ifelse(hhi > 2500, 1, 0),
         mod_concentrated = ifelse(hhi>1500, 1, 0)) %>% 
  group_by(year_id, hhi_type) %>% 
  summarize(pct_hi_concentrated = mean(highly_concentrated),
            pct_mod_or_hi_concentrated =  mean(mod_concentrated))
write.csv(hhi_hrr_summary, paste0(plot_dir, "time_trends/hrr_summary_table_ashe.csv"), row.names = F)

#-------------------------------------------------------------------------------

# 3) Create summary table - HRR level - for table / percentages

table <- hhi_hrr_summary %>% 
  filter(hosp_type == "all", market_var == "HOSPBD")

# ------------------------------------------------------------------------------

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