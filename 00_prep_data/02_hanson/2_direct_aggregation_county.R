# Directly calculate HHI at the COUNTY level using AHA data
# Maitreyi Sahu
# April 6, 2024

# Output files: "hhi_county_wide.feather", "hhi_individ_county_long.feather" 

# ==============================================================================

# Setup
rm(list=ls())
pacman::p_load(tidyr, data.table, ggplot2, arrow) 
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"
harry_dir <- paste0(dir, "hanson/from_harry/")
aha_clean <- fread(paste0(dir, "aha_clean_2000_2019.csv"))

# Source HHI aggregation function
source("~/repos/us_value/1_data_compilation/4_hospital_hhi/00_prep_data/hhi_functions.R")

# ==============================================================================

# MARKET SHARE VARIABLES - missingness
print(paste0('Missing Hosp BD: ', nrow(aha_clean[HOSPBD==0,])))
print(paste0('Missing Admits: ', nrow(aha_clean[ADMTOT==0,]))) 
print(paste0('Missing Ipt Days: ', nrow(aha_clean[IPDTOT==0,]))) 
print(paste0('Missing Medicare Pt Days: ', nrow(aha_clean[MCRIPD==0,]))) 

# =============================================================================+

# Aggregate HHI at county level for all hospitals and general surgery hospitals

hhi_county_wide <- aha_clean %>% aggregate_hhi_county() %>% 
  mutate(hosp_type = "all")

hhi_county_wide_gen_surg <- aha_clean %>% filter(gen_surg == 1) %>% aggregate_hhi_county() %>% 
  mutate(hosp_type = "gen_surg")

# ------------------------------------------------------------------------------

# STEP 2: add 2019-2022 data from Harry and check that 2019 is the same

# var lists
hhi_vars <- c("hhi_beds", "hhi_admits", "hhi_pt_days", "hhi_mcr_pt_days")
tot_market_vars <- c("tot_beds", "tot_admits", "tot_pt_days", "tot_mcr_pt_days", "aha_hosp_n", "aha_systems_n")

# function to format harry's data
format_harry <- function(df) {
  
  formatted_df <- df %>% 
    
    mutate(year_id = 2000 + YEAR, 
           aha_hosp_n = NA) %>% 
    mutate(cnty = sprintf("%05d", FCOUNTY)) %>% 
    rename(aha_systems_n = aha_systems) %>% 
    select(year_id, cnty, all_of(hhi_vars), all_of(tot_market_vars))
    
  return(formatted_df)
}

harry_county_wide <- fread(paste0(harry_dir, "hhi_cnty_hanson_wide_All_Hospitals_19_22.csv")) %>% 
  format_harry() %>% 
  mutate(hosp_type = "all")

harry_county_wide_gen_surg <- fread(paste0(harry_dir, "hhi_cnty_hanson_wide_Gen_Surg_19_22.csv")) %>% 
  format_harry() %>% 
  mutate(hosp_type = "gen_surg")  

# Some issues with the 'FCOUNTY' variable from Harry's data assigning to nearby counties. Replace with the 

  
# check that Harry and Maitreyi's 2019 vals are the same

check <- hhi_county_wide_gen_surg %>% mutate(version = "MS") %>% 
  rbind(harry_county_wide_gen_surg %>% mutate(version = "HK")) %>% 
  filter(year_id == 2019) %>% 
  pivot_wider(id_cols = cnty, names_from = version, values_from = hhi_pt_days)

ggplot(data = check, aes(x = MS, y = HK)) + geom_point() + geom_abline(color = "blue") # TODO check discrepancies

mean_diff = hhi_county_wide_gen_surg %>% mutate(version = "MS")  %>% 
  rbind(harry_county_wide_gen_surg %>% mutate(version = "HK")) %>% 
  filter(year_id == 2019) %>% 
  pivot_wider(id_cols = cnty, names_from = version, values_from = hhi_pt_days) %>% 
  mutate(diff = MS - HK) %>% 
  
#high_mean_diff <- mean_diff[mean_diff$diff > 1, ]
#low_mean_diff <- mean_diff[mean_diff$diff < -1, ]  
  summarise(mean_diff = mean(diff, na.rm = T)) %>% pull()

print(paste0("Average difference between Harry and Maitreyi 2019 values of: ", mean_diff))

# bind HK's 2020+ vals [use MS 2019 vals!]

hhi_county_wide_combined <- rbind(
  
  hhi_county_wide, 
  hhi_county_wide_gen_surg,
  harry_county_wide %>% filter(year_id >=2020), 
  harry_county_wide_gen_surg %>% filter(year_id >=2020)
) %>% 
  filter(cnty!="   NA") %>% # TODO remove this earlier
  mutate(cnty = as.integer(cnty))

rm(hhi_county_wide, 
   hhi_county_wide_gen_surg,
   harry_county_wide, 
   harry_county_wide_gen_surg)

#===============================================================================

# STEP 3: merge back to the individual level and format

ids <- fread(paste0(dir, "id_vars/aha_clean_ids.csv"))

hanson_vars <- c("year_id", "hosp_type", hhi_vars)

hhi_individ_county_wide <- ids %>% select("ID", "year_id", "cnty") %>% distinct() %>% 
  left_join(hhi_county_wide_combined, by = c("year_id", "cnty")) %>% 
  select(ID, all_of(hanson_vars)) %>% 
  filter(!is.na(hosp_type))  # TODO : remove this line after getting the FCOUNTY ID??

print(paste0("# of duplicated rows: ", nrow(hhi_individ_county_wide[duplicated(hhi_individ_county_wide), ])))

# Format / Cleanup [same process as for the buffers]

hhi_individ_county_long <- hhi_individ_county_wide %>% 
  
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
         radius = "county",
         radius_units = "agg") %>% 
  select(ID, year_id, hhi_type, market_var, radius, radius_units, hosp_type, hhi)

# ==============================================================================

# STEP 4: Drop prior to 2002! Counties in 2000 + 2001 are weird

hhi_county_wide_combined <- hhi_county_wide_combined %>% filter(year_id >= 2002)
hhi_individ_county_long <- hhi_individ_county_long %>% filter(year_id >= 2002)

#===============================================================================

# SAVE

write_feather(hhi_county_wide_combined, paste0(dir, "hanson/hhi_county_wide.feather"))
write_feather(hhi_individ_county_long, paste0(dir, "hanson/hhi_individ_county_long.feather"))
