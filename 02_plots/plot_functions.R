
# Save
plot_dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/plots/"

# ==============================================================================

# Which variables to select?

cols_of_interest <- c("hanson_HOSPBD_county_agg_gen_surg", "hanson_HOSPBD_hrr_agg_gen_surg", 
                      "linear_HOSPBD_15_miles_gen_surg", "linear_HOSPBD_30_miles_gen_surg", "linear_HOSPBD_50_miles_gen_surg",
                      "drive_time_HOSPBD_15_minutes_gen_surg", "drive_time_HOSPBD_30_minutes_gen_surg", "drive_time_HOSPBD_60_minutes_gen_surg")

cols_of_interest_labels <- factor(
  c("County", "Hospital Referral Region",
    "Linear radius, 15 mile", "Linear radius, 30 mile", "Linear radius, 50 mile",
    "Drive time radius, 15 min", "Drive time radius, 30 min", "Drive time radius, 60 min"), 
  levels = c("County", "Hospital Referral Region",
                "Linear radius, 15 mile", "Linear radius, 30 mile", "Linear radius, 50 mile",
                "Drive time radius, 15 min", "Drive time radius, 30 min", "Drive time radius, 60 min"))

plot_colors <- c( "goldenrod1", "goldenrod3", 
                  "darkred",  "firebrick3", "darksalmon", 
                  #"darkgreen", "seagreen4","darkseagreen1",
                  "darkblue", "deepskyblue4", "paleturquoise3")
                                  
custom_vars <- data.table(cols = cols_of_interest, col_labels = cols_of_interest_labels, plot_colors = plot_colors)

cols_subset <- c("hanson_HOSPBD_county_agg_gen_surg", "hanson_HOSPBD_hrr_agg_gen_surg", 
                 "linear_HOSPBD_15_miles_gen_surg",  "drive_time_HOSPBD_30_minutes_gen_surg")
custom_vars_subset <- custom_vars[cols %in% cols_subset]

# ------------------------------------------------------------------------------

# State / County Names and Pop
# DEX has from 2000-2021 only; impute 2022 using 2021 
# TODO update to 2022 pop [created in 00_prep_data/01_pop/2_dex_pop] 

county_pop <- fread("/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/pop/dex/dex_county_pop00_21.csv")
state_pop <- fread("/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/pop/dex/dex_state_pop00_21.csv") %>% mutate(postal_code = state_code)

county_names <- county_pop %>% select(-year_id, -pop) %>% unique()
state_names <- state_pop %>% select(-year_id, -pop) %>% unique()  %>% mutate(postal_code = state_code)

# ------------------------------------------------------------------------------


