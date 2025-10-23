# Check what's going on with the US census population data, which has a lot of NA's
# MSahu
# Update 6/15/2024 --> at hrr / state-hrr levels no missing data

# ---------------------------------------------------------------------------------------

rm(list=ls())
library(naniar)

# Set up directories
dir <- "/mnt/share/resource_tracking/us_value/data/"
in_dir <- paste0(dir, "hospital_hhi/raw/aha/AHA raw data/")
out_dir <- paste0(dir, "hospital_hhi/processed/")

state_hrr_pop <- fread( paste0(out_dir, "pop/pop_state_hrr.csv")) %>% rename(state_hrr_pop = pop)
hrr_pop <- fread(paste0(out_dir, "pop/pop_hrr.csv")) %>% rename(hrr_pop = pop)

# ---------------------------------------------------------------------------------------

# How many hospitals don't have a population?

# Load AHA ID's

aha_ids <- fread(paste0(out_dir,"id_vars/aha_clean_ids.csv"))

# Merge with pop

hosp_pop_df <- aha_ids %>% 
  left_join(state_hrr_pop, by = c("year_id", "state_hrr", "hrrnum")) %>% 
  left_join(hrr_pop, by = c("year_id", "hrrnum")) 

gg_miss_fct(x = hosp_pop_df, fct = year_id)
# missingness doesn't get that much better over time

gg_miss_fct(x = hosp_pop_df, fct = MSTATE)

# How many zip codes are missing populations? 
total_hospyears <- aha_clean %>% nrow()
missing_pop <- hosp_pop_df %>% filter(is.na(state_hrr_pop)) %>% nrow()
print(paste0("% of AHA hospital-years missing state-hrr populations: ", round(100 * missing_pop / total_hospyears, 1) , "")) #10.7%

# Which hrr-years are missing populations?
missing_zips_2019 <- hosp_pop_df %>% filter(is.na(hrr_pop)) 
  
nrow(missing_zips_2019) / (nrow(aha_ids)/20) # 1.1%

# ------------------------------------------------------------------------------

# for a single state_hrr / HRR, is the result a straight line over time?
random_hrrs <- sample(hosp_pop_df$hrrnum, 10)
check <- hosp_pop_df %>% filter(hrrnum %in% random_hrrs) 
#check <- combined_pop %>% filter(GEOID == "01001")
ggplot(data = check) + 
  geom_point(aes(x = year_id, y = hrr_pop), color = "blue")+
  geom_point(aes(x = year_id, y = state_hrr_pop), color = "black") +
  facet_wrap(~state_hrr) 
