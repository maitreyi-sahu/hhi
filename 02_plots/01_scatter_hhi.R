# Compare the various HHIs - SCATTERS
# Updated 4/8/2024
# Maitreyi Sahu

# ==============================================================================

pacman::p_load('RColorBrewer', 'GGally')

source('init.R')
source("plot_functions.R")

# Load data
hhi_individ_wide <- read_feather(paste0(data_dir, "processed/compiled_hhi/compiled_hhi_individ_wide.feather")) %>% select(-hrrnum) %>% setDT()
hhi_hrr_wide <- read_feather(paste0(data_dir, "processed/compiled_hhi/compiled_hhi_hrr_wide.feather")) %>% setDT()
hhi_state_wide <- read_feather(paste0(data_dir, "processed/compiled_hhi/compiled_hhi_state_wide.feather")) %>%  select(-location_id, -location_name) %>% setDT()

# ==============================================================================

# # Check individual-level Cooper 2008-14 vs our 30-min drive time Cooper 2008-14 - gen/surg
# TODO move this to another file

# cooper <- hhi_individ_wide[year_id %in% 2008:2014, c("ID", "year_id", "cooper_drive_time_08_14_HOSPBD_30_min_gen_surg", "drive_time_HOSPBD_30_minutes_gen_surg")][ 
#   ,.(cooper = mean(cooper_drive_time_08_14_HOSPBD_30_min_gen_surg, na.rm = T), drive_30_min = mean(drive_time_HOSPBD_30_minutes_gen_surg, na.rm = T)) , by = c("ID")]
# 
# ggplot(cooper, aes(cooper, drive_30_min)) +
#   geom_point() +
#   xlab("Cooper, 30-min drive time HHI for 2008-14 [HOSPBD]") +
#   ylab("Our 30-min drive time HHI for 2008-14 [HOSPBD]") +
#   geom_abline(color = "blue")
# 
# summary(lm(drive_30_min ~ cooper, data = cooper))
# 
# cooper <- hhi_individ_wide[year_id %in% 2008:2014, c("ID", "year_id", "cooper_drive_time_08_14_HOSPBD_30_min_gen_surg", "linear_HOSPBD_15_miles_gen_surg")][ 
#   ,.(cooper = mean(cooper_drive_time_08_14_HOSPBD_30_min_gen_surg, na.rm = T), linear_15 = mean(linear_HOSPBD_15_miles_gen_surg, na.rm = T)) , by = c("ID")]
# 
# ggplot(cooper, aes(cooper, linear_15)) +
#   geom_point() +
#   xlab("Cooper, 30-min drive time HHI for 2008-14 [HOSPBD]") +
#   ylab("Our 15-mi linear HHI for 2008-14 [HOSPBD]") +
#   geom_abline(color = "blue")

# ==============================================================================

df <- hhi_state_wide # arbitrarily choose one of the 3 levels

# Create lists of variable names
all <- names(df)[-c(1, 2)] 
hanson <- all[grep("^hanson", all)]
linear <- all[grep("^linear", all)]
drive_time <- all[grep("^drive_time_HOSPBD.*_gen_surg", all)]
linear_15 <- all[grep("^linear.*15_miles_gen_surg$", all)]
linear_30 <- all[grep("^linear.*30_miles_gen_surg$", all)]
linear_50 <- all[grep("^linear.*50_miles_gen_surg$", all)]
drive_time_15 <- all[grep("^drive_time.*15_minutes_gen_surg$", all)]
drive_time_30 <- all[grep("^drive_time.*30_minutes_gen_surg$", all)]
drive_time_60 <- all[grep("^drive_time.*60_minutes_gen_surg$", all)]
hcup <- all[grep("^hcup", all)]
hosp_type <- c("drive_time_HOSPBD_30_minutes_gen_surg", "drive_time_HOSPBD_30_minutes_all")
# custom <- c("hanson_HOSPBD_hrr_agg_gen_surg",
#             #"hcup_variable_HOSPDIS_75_pct_hcup", "hcup_pt_flow_HOSPDIS_75-95_pct_hcup", 
#             #"cooper_drive_time_08_14_HOSPBD_30_min",
#             "linear_HOSPBD_15_miles_gen_surg", "linear_HOSPBD_30_miles_gen_surg", "linear_HOSPBD_50_miles_gen_surg",
#             "drive_time_HOSPBD_15_minutes_gen_surg", "drive_time_HOSPBD_30_minutes_gen_surg", "drive_time_HOSPBD_60_minutes_gen_surg")

# Plot functions

# vars<- custom_subset$cols
# df <- get(paste0("hhi_state_wide")) %>% select(all_of(vars))
# rename_vector <- setNames(custom_vars$col_labels, custom_vars$cols)
# setnames(df, old = names(rename_vector), new = as.character(unname(rename_vector)), skip_absent = T)
# 
# p <- ggpairs(df, 
#              lower=list(continuous="smooth"), 
#              method = c("pairwise", "pearson")) +
#   theme_void() 
# 
# p_size <- length(vars)*2


  
plot_correlation <- function(vars, custom = F, vector_name, lvl) {
  
  df <- get(paste0("hhi_", lvl, "_wide")) %>% select(all_of(vars))
  
  if (custom == T) {
    
    rename_vector <- setNames(custom_vars$col_labels, custom_vars$cols)
    setnames(df, old = names(rename_vector), new = as.character(unname(rename_vector)), skip_absent = T)
    
  }
  
  p <- ggpairs(df, 
               lower=list(continuous="smooth") #, method = c("pairwise", "pearson")
               ) +
    theme_minimal() +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = .6),
      plot.background = element_rect(colour = "black", fill = NA, size = .6)
    )
  
  p_size <- length(vars)*2
  
  ggsave(p, file = paste0(plot_dir, "scatter/correlation_", vector_name,"_",lvl,".pdf"), width = p_size, height = p_size)
}

# ==============================================================================

# PLOTS 

# Custom - Joe's suggestion
custom <- custom_vars$cols
plot_correlation(custom, T, "", "state")
plot_correlation(custom, T, "", "hrr")
plot_correlation(custom, T, "", "individ")

# Custom subset
subset <- custom_vars_subset$cols
plot_correlation(subset, T, "subset", "state")
plot_correlation(subset, T, "subset", "hrr")
plot_correlation(subset, T, "subset", "individ")

# Custom subset
subset_linear <- grep("^linear", custom_vars$cols, value = T)
plot_correlation(subset_linear, T, "subset_linear", "state")
plot_correlation(subset_linear, T, "subset_linear", "hrr")
plot_correlation(subset_linear, T, "subset_linear", "individ")


# All - full size (takes a few min to run)
# plot_correlation(all, "all", "state")
# plot_correlation(all, "all", "hrr")
# plot_correlation(all, "all", "individ")

# Within group - at the individual level
plot_correlation(hanson, F, "hanson", "state")
plot_correlation(linear, F, "linear", "state")
plot_correlation(drive_time, F, "drive_time", "state")
plot_correlation(linear_15, F, "linear_15", "state")
plot_correlation(linear_30, F, "linear_30", "state")
plot_correlation(linear_50, F, "linear_50", "state")
plot_correlation(drive_time_30, F, "drive_time_30", "state")
plot_correlation(drive_time_30, F, "drive_time_30", "hrr")
plot_correlation(hcup, F, "hcup", "state")

plot_correlation(hosp_type, F, "hosp_type", "state")
plot_correlation(hosp_type, F, "hosp_type", "hrr")
