#===============================================================================
  
# Aggregate population at HRR + state-HRR levels
  
# MSahu + some ZIPFIX code (zip_residence_map.R)
# Last edited: June 15, 2024

# STEPS:
# 1: read county pop data
# 2: read HUD crosswalk files
# 3: in HUD, interpolate "res_ratio" for 2000-2009 
# 4: crosswalk county pop to zip-level pop
# 5: aggregate population at state-HRR and HRR levels

# NEED TO CHECK STEPS 3 to 5! 
# Also note that need to adapt aggregation code

# ==============================================================================

rm(list=ls())
pacman::p_load(data.table, dplyr, stringr, readxl, reshape2, arrow, zoo)
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"
out_dir <- paste0(dir, "processed/pop/")

# list of state-HRRs and their zip codes from AHA data
#aha_zips <- fread(paste0(dir, "processed/aha_clean_2000_2019.csv"))[ , c("year_id", "hosp_zip", "state_hrr", "hrrnum")] %>% distinct()
aha_zips <- fread(paste0(dir, "processed/id_vars/aha_clean_ids.csv"))[ , c("year_id", "hosp_zip", "state_hrr", "hrrnum")] %>% distinct()

# ------------------------------------------------------------------------------

# STEP 1: READ CENSUS [note, processing/interpolation already occurs in "0_get_pop_county.R"]

# read and format census county pop through 2022 [DEX has through 2021]
county_pop <- read_feather(paste0(out_dir, "tidycensus00_22/county/total_county_pop.feather"))[ , c("county_fips", "year_id", "total_pop")]

# county_names <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")

# ------------------------------------------------------------------------------

# STEP 2: READ HUD crosswalk files; use Q1 only

hud_dir <- paste0(dir, "raw/hud_crosswalk/county_to_zip/")

hud_data <- rbindlist(
  lapply(
    list.files(hud_dir, pattern = "xlsx"),
    function(x){
      tmp <- data.table(readxl::read_xlsx(paste0(hud_dir, x)))
      tmp[, year_id := as.numeric(str_sub(x, start = -9, end = -6))]
      tmp[, quarter := "Q1"]
      setnames(tmp, tolower(names(tmp)))
      return(tmp)
    }
  ), 
  use.names = T, fill = T
)[ , c("county", "zip", "year_id", "res_ratio")]
setnames(hud_data, "county", "county_fips")

# ------------------------------------------------------------------------------

# INTERPOLATION GIVES NEGATIVE VALUES - ABANDON AND USE 2010 values for 2000-9

# STEP 3: For HUD data, linear interpolation for 2000-9
# Run models separately by county for faster processing 

# interpolate_res_ratio <- function(current_county) {
#   
#   try({ 
#     
#   hud_subset <- hud_data[county_fips == current_county]
#   
#   lm_model <- lm(res_ratio ~ year_id + as.factor(zip),  # treat time as linear
#                  data = hud_subset)
#   
#   hud_subset_predict <- expand.grid(county_fips = current_county, zip = unique(hud_subset$zip), year_id = 2000:2009) 
#   predicted_vals <- predict(lm_model, newdata = hud_subset_predict)
#   hud_subset_predict$res_ratio <- predicted_vals
#   
#   }, silent = T)
#   
#   if(exists("hud_subset_predict")) {
#     
#     hud_subset_predict <- hud_subset_predict
#     
#   } else{
#     
#     # if model fails, use the average over the years
#     hud_subset_average <- hud_subset[, .(res_ratio = mean(res_ratio)), by = zip][ , county_fips := current_county]
#     hud_subset_predict <- crossing(year_id = 2000:2009, hud_subset_average)[, c("county_fips", "zip", "year_id", "res_ratio")]
#   }
#  
#   return(setDT(hud_subset_predict))
# }

# Interpolate backwards

# hud_interpolated_combined <- rbindlist(lapply(unique(hud_data$county), interpolate_res_ratio))
# hud_final <- rbind(hud_interpolated_combined, hud_data)

# Use the 2010 value for 2000:2009
hud_data00_09 <- hud_data[year_id == 2010, .(county_fips, zip, res_ratio)] %>% 
  cross_join(data.table(year_id = 2000:2009))

hud_final <- rbind(hud_data00_09, hud_data) %>% 
  select(year_id, everything())

rm(hud_data00_09, hud_data)

# CHECK - PLOT
random_ids <- sample(hud_final$county_fips, 12)
check <- hud_final %>% filter(county_fips %in% random_ids) 
ggplot(data = check) + 
  geom_line(aes(x = year_id, y = res_ratio, group = zip))+
  facet_wrap(~county_fips )
# some dips / changes around 2015... 
# TODO consider smoothing these / interpolating over time

# ------------------------------------------------------------------------------

# STEP 4: use "res_ratio" to assign counties to zip codes

pop_by_zip <- county_pop %>% left_join(hud_final, by = c("county_fips", "year_id")) %>% 
  mutate(pop = total_pop * res_ratio,
         zip = as.integer(zip))

# CHECK - PLOT
random_ids <- sample(pop_by_zip$zip, 12)
check <- pop_by_zip %>% filter(zip %in% random_ids) 
ggplot(data = check) + 
  geom_point(aes(x = year_id, y = res_ratio))+
  facet_wrap(~zip )

# =======================================================================================================================================

# STEP 5: aggregate population at state-HRR and HRR levels and forward fill in Zero's 

zip_pop <- aha_zips %>% mutate(zip = hosp_zip) %>% left_join(pop_by_zip, by = c("year_id", "zip")) %>% select(-zip) # merge pop

# state-HRR level agg + forward fill missing pops
state_hrr_pop <- zip_pop[, .(pop = sum(pop, na.rm = TRUE)), by = c("year_id", "hrrnum", "state_hrr")] %>% filter(!is.na(state_hrr))
setorder(state_hrr_pop, state_hrr, year_id)
misspop_hrrs <- state_hrr_pop[pop == 0, unique(hrrnum)] # HRRs with missing pops
state_hrr_pop <- state_hrr_pop[pop == 0, pop := NA][, pop := na.locf(pop, na.rm = F)] # forward fill
print(paste0("Number of state-HRRs with 0 pop: ", nrow(state_hrr_pop[hrrnum %in% misspop_hrrs & pop == 0]) ))

# HRR-level agg + forward fill missing pops
hrr_pop <- zip_pop[, .(pop = sum(pop, na.rm = TRUE)), by = c("year_id", "hrrnum")] %>% filter(!is.na(hrrnum))
setorder(hrr_pop, hrrnum, year_id)
hrr_pop <- hrr_pop[pop == 0, pop := NA][, pop := na.locf(pop, na.rm = F)] # forward fill
print(paste0("Number of HRRs with 0 pop: ", nrow(hrr_pop[hrrnum %in% misspop_hrrs & pop == 0]) ))

# =======================================================================================================================================

# Save

write.csv(state_hrr_pop, file = paste0(out_dir, "pop_state_hrr.csv"), row.names = F)
write.csv(hrr_pop, file = paste0(out_dir, "pop_hrr.csv"), row.names = F)