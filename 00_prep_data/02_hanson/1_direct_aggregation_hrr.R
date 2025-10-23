# Directly calculate HHI at the HRR level using AHA data
# Maitreyi Sahu
# Aug 27, 2023

# Output files: "hhi_hrr_hanson_wide.feather", "hhi_individ_hanson_long.feather" 

# ==============================================================================

rm(list=ls())

# Packages
pacman::p_load(tidyr, data.table, ggplot2, arrow) 

# Directories 
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"
harry_dir <- paste0(dir, "hanson/from_harry/")
aha_clean <- fread(paste0(dir, "aha_clean_2000_2019.csv"))

# Source HHI aggregation function
source("~/repos/us_value/1_data_compilation/4_hospital_hhi/00_prep_data/hhi_functions.R")

#===============================================================================

# AGGREGATE HHI across both dataframes

hhi_hrr_hanson_wide <- aha_clean %>% aggregate_hhi_hrr() %>% mutate(hosp_type = "all")

hhi_hrr_hanson_wide_gen_surg <- aha_clean %>% filter(gen_surg == 1) %>% aggregate_hhi_hrr() %>% mutate(hosp_type = "gen_surg")
  
# Scatter all hosp vs gen/surg only - somewhat correlated

# combined_hanson_wide <- hhi_hrr_hanson_wide %>% select(year_id, hrrnum, hhi_pt_days) %>% 
#   left_join(hhi_hrr_hanson_wide_gen_surg, by = c("year_id", "hrrnum")) 
# 
# ggplot(data = combined_hanson_wide, aes(x = hhi_pt_days.x, y = hhi_pt_days.y, group = year_id)) +
#   geom_point() + theme_bw() +
#   ylab("HHI - gen surg only") + xlab("HHI - all hospitals") +
#   facet_wrap(~as.factor(year_id))
# 
# rm(combined_hanson_wide)

#===============================================================================

# STEP 2: add 2019-2022 data from Harry and check that 2019 is the same

# var lists
hhi_vars <- c("hhi_beds", "hhi_admits", "hhi_pt_days", "hhi_mcr_pt_days")
tot_market_vars <- c("tot_beds", "tot_admits", "tot_pt_days", "tot_mcr_pt_days", "aha_hosp_n", "aha_systems_n")

# function to format harry's data
format_harry <- function(df) {
  
  formatted_df <- df %>% 
    
    mutate(year_id = 2000 + YEAR, 
           aha_hosp_n = NA) %>% 
    rename(hrrnum = HRRCODE, aha_systems_n = aha_systems) %>% 
    select(year_id, hrrnum, all_of(hhi_vars), all_of(tot_market_vars))
    
  return(formatted_df)
}

harry_hanson_wide <- fread(paste0(harry_dir, "hhi_hrr_hanson_wide_All_Hospitals_19_22.csv")) %>% 
  format_harry() %>% 
  mutate(hosp_type = "all") 

harry_hanson_wide_gen_surg <- fread(paste0(harry_dir, "hhi_hrr_hanson_wide_Gen_Surg_19_22.csv")) %>% 
  format_harry() %>% 
  mutate(hosp_type = "gen_surg")  
  
# check that Harry and Maitreyi's 2019 vals are the same

check <- hhi_hrr_hanson_wide %>% mutate(version = "MS") %>% 
  rbind(harry_hanson_wide %>% mutate(version = "HK")) %>% 
  filter(year_id == 2019) %>% 
  pivot_wider(id_cols = hrrnum, names_from = version, values_from = hhi_pt_days)

ggplot(data = check, aes(x = MS, y = HK)) + geom_point() + geom_abline(color = "blue")

mean_diff = hhi_hrr_hanson_wide_gen_surg %>% mutate(version = "MS")  %>% 
  rbind(harry_hanson_wide_gen_surg %>% mutate(version = "HK")) %>% 
  filter(year_id == 2019) %>% 
  pivot_wider(id_cols = hrrnum, names_from = version, values_from = hhi_pt_days) %>% 
  mutate(diff = MS - HK) %>% 
  summarise(mean_diff = mean(diff, na.rm = T)) %>% pull()
print(paste0("Average difference between Harry and Maitreyi 2019 values of: ", mean_diff))

# bind

hhi_hrr_hanson_wide_combined <- rbind(
  
  hhi_hrr_hanson_wide, 
  hhi_hrr_hanson_wide_gen_surg,
  harry_hanson_wide %>%  filter(year_id != 2019), 
  harry_hanson_wide_gen_surg %>%  filter(year_id != 2019)
) 
rm(hhi_hrr_hanson_wide, hhi_hrr_hanson_wide_gen_surg,
   harry_hanson_wide, harry_hanson_wide_gen_surg)

#===============================================================================

# STEP 3: merge back to the individual level and format

ids <- fread(paste0(dir, "id_vars/aha_clean_ids.csv"))[, c("year_id", "ID", "hrrnum")]

hanson_vars <- c("year_id", "hosp_type", hhi_vars)

hhi_individ_hanson_wide <- ids %>% 
  left_join(hhi_hrr_hanson_wide_combined) %>% # TODO BY WHAT? check that this works as expected! 
  select(ID, all_of(hanson_vars))

print(paste0("# of duplicated rows: ", nrow(hhi_individ_hanson_wide[duplicated(hhi_individ_hanson_wide), ])))

# Format / Cleanup [same process as for the buffers]

hhi_individ_hanson_long <- hhi_individ_hanson_wide %>% 
  
  select(ID, all_of(hanson_vars)) %>% 
  
  # reshape long 
  rename(HOSPBD = hhi_beds,
         ADMTOT = hhi_admits,
         IPDTOT = hhi_pt_days,
         MCRIPD = hhi_mcr_pt_days) %>%
  reshape2::melt(id.vars = c("ID", "year_id", "hosp_type"), measure.vars = c("HOSPBD", "ADMTOT", "IPDTOT", "MCRIPD"), variable.name = "market_var") %>% 
  rename(hhi = value, 
         year_id = year_id) %>% 
  
  # add additional vars
  mutate(hhi_type = "hanson",
         radius = "hrr",
         radius_units = "agg") %>% 
  select(ID, year_id, hhi_type, market_var, radius, radius_units, hosp_type, hhi) 

#===============================================================================

# SAVE

write_feather(hhi_hrr_hanson_wide_combined, paste0(dir, "hanson/hhi_hrr_hanson_wide.feather"))
write_feather(hhi_individ_hanson_long, paste0(dir, "hanson/hhi_individ_hanson_long.feather"))
