# Summary stats and line graphs for HHI
# Maitreyi Sahu
# Aug 26, 2023; Updated 6/10/24

# setup
rm(list=ls())
pacman::p_load(ggplot2, arrow, dplyr, ggrepel, scales, data.table)
library("writexl", lib.loc = "~/rpackages")

# directories 
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"
out_dir <- paste0(dir, "plots/time_trends/")

# load data
hhi_hrr_wide <- read_feather(paste0(dir, "processed/compiled_hhi/compiled_hhi_hrr_wide.feather")) %>% setDT()
hhi_state_wide <- read_feather(paste0(dir, "processed/compiled_hhi/compiled_hhi_state_wide.feather")) %>%  select(-location_id, -location_name) %>% setDT()

# source columns of interest and labels
source("~/repos/us_value/1_data_compilation/4_hospital_hhi/02_plots/plot_functions.R")

cols_of_interest <- c(custom_vars$cols, 
                      "hcup_variable_HOSPDIS_75_pct_hcup", "hcup_variable_HOSPDIS_90_pct_hcup", "hcup_pt_flow_HOSPDIS_75-95_pct_hcup")

# ==============================================================================

# HEATMAPS: Trends from 2000-2019 

# STATES: average increase in concentration from 2000-19

# function to calculate the difference in HHI values 

calc_diff <- function(level, start_year, end_year, pct = F, cols_of_interest = cols_of_interest) {
  
  # get the corresponding dt
  dt <- switch(level,
                   "state" = hhi_state_wide,
                   "hrr" = hhi_hrr_wide)
  
  # set the by_var
  by_var <- switch(level,
                    "state" = c("postal_code"),
                    "hrr" = c("hrrnum"))
  
  # aggregate
  diffDT <- dt[year_id %in% c(start_year, end_year), ][
    , lapply(.SD, function(x) {
      hhi_end <- x[year_id == end_year]
      hhi_start <- x[year_id == start_year]
      if (length(hhi_end) == 0 || length(hhi_start) == 0) {
        return(NA)  # Handle missing values for either year
      }
      if (pct == F) {hhi_end - hhi_start} else if (pct == T) {100 *(hhi_end - hhi_start) / hhi_start} }), 
    by = by_var, 
    .SDcols = cols_of_interest
  ]
  
  # set column names
  if (pct == F) {
    setnames(diffDT, old = cols_of_interest, new = paste0(cols_of_interest, "_diff_", start_year, "_", end_year))
  } else if (pct == T) { 
    setnames(diffDT, old = cols_of_interest, new = paste0(cols_of_interest, "_pct_diff_", start_year, "_", end_year))
  }
  
  return(diffDT)
}

# run 

hhi_state_raw_diff_00_22 <- calc_diff("state", 2000, 2022, pct = F, cols_of_interest)
hhi_state_pct_diff_00_22 <-calc_diff("state", 2000, 2022, pct = T, cols_of_interest)
hhi_state_raw_diff_10_19 <- calc_diff("state", 2010, 2019, pct = F, cols_of_interest)
hhi_state_pct_diff_10_19 <-calc_diff("state", 2010, 2019, pct = T, cols_of_interest)

write_xlsx(list("pct_diff_00_22" = hhi_state_pct_diff_00_22, 
                "raw_diff_00_22" = hhi_state_raw_diff_00_22,
                "pct_diff_10_19" = hhi_state_pct_diff_10_19, 
                "raw_diff_10_19" = hhi_state_raw_diff_10_19), 
           paste0(out_dir, "state_hhi_time_trends.xlsx"))

hhi_hrr_raw_diff_00_22 <- calc_diff("hrr", 2000, 2022, pct = F, cols_of_interest)
hhi_hrr_pct_diff_00_22 <-calc_diff("hrr", 2000, 2022, pct = T, cols_of_interest)

write_xlsx(list("pct_diff_00_22" = hhi_hrr_pct_diff_00_22, 
                "raw_diff_00_22" = hhi_hrr_raw_diff_00_22), 
           paste0(out_dir, "hrr_hhi_time_trends.xlsx"))

# ------------------------------------------------------------------------------

# LINE GRAPHS OVER TIME (change to functions)

# State level

hhi_state <- hhi_state_wide %>% 
  melt(id.vars = c("postal_code", "year_id"), 
       measure.vars = c(custom_vars$cols)) %>% 
  setnames(old = "variable", new = "method") %>% 
  setnames(old = "value", new = "HHI") %>% 
  left_join(custom_vars, by = c("method" = "cols")) %>% 
  left_join(state_pop, by = c("year_id", "postal_code")) %>% 
  setDT() 

# NATIONAL LEVEL PLOT OVER TIME

# Pop weighted average
hhi_national <- hhi_state[ , .(HHI = weighted.mean(HHI, weight = pop)), by = c("year_id", "col_labels")] 
  
threshold_color <- "grey30"
threshold_size <- .5
national_plot <-  ggplot(data = hhi_national, aes(x = year_id, y = HHI, group = col_labels)) +
    geom_hline(yintercept = 1500, size = threshold_size, #linetype = "dashed", 
               color = threshold_color) +
    geom_hline(yintercept = 2500, size = threshold_size, color = threshold_color) +
    geom_hline(yintercept = 5000, size = threshold_size, color = threshold_color) +
  geom_line(aes(color = col_labels), size = 1) +
  geom_point(aes(color = col_labels)) +
  scale_color_manual(values = plot_colors, name = "Geographic boundary") +
  theme_bw() +
  xlab("Year") +
  ylab("Herfindahl-Hirschman Index, population-weighted mean across US States ") +
  ylim(0, 10000)  + 
  ggtitle("Hospital market concentration in the United States across methods, 2000-2022") +
  labs(caption = "*All measures use a market share of hospital beds and include general/surgical hospitals")

# TODO caption : all are hospital beds and gen/surg only

pdf("/mnt/share/resource_tracking/us_value/data/hospital_hhi/plots/time_trends/hhi_over_time_national.pdf",
    width = 10, height = 6)
print(national_plot)
dev.off()

p <- 
  ggplot(data = hhi_state, aes(x = year_id, y = HHI, group = (postal_code))) +
  geom_line(aes(color = postal_code)) +
  theme_bw() +
  xlab("Year") +
  ylab("Herfindahl-Hirschman Index") + 
  facet_wrap(~label)

pdf("/mnt/share/resource_tracking/us_value/data/hospital_hhi/plots/time_trends/hhi_over_time_state.pdf",
    width = 10, height = 6)
print(p)
dev.off()


# ------------------------------------------------------------------------------

# State; scattered against Cooper

ggplot(data = hhi_state_wide, aes(x = year_id, y = linear_HOSPBD_50_mile)) +
  geom_line(aes(color = postal_code)) +
  geom_point(data = hhi_state, aes(x = year_id, 
                                   y = cooper_drive_time_08_14_HOSPBD_30_min,
                                   color = postal_code), size = 2) +
  geom_hline(yintercept = 2500, size = 1,  color = "navyblue") +
  geom_hline(yintercept = 1500, size = 1, color = "navyblue") +
  annotate(geom="text", x=2005, y=2600, label="Highly Concentrated",
           color="navyblue", size = 5.5) +
  annotate(geom="text", x=2005, y=1600, label="Moderately Concentrated",
           color="navyblue", size = 5.5) +
  geom_label_repel(aes(label = ifelse(year_id== 2019, postal_code, "")), nudge_x = 0.35, size = 4) +
  xlab ("Year") + ylab("Hospital HHI") +
  ggtitle("State HHI using hospital beds for market share") +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15)) +
  labs(color = "State")

m1 <- lm(data = hhi_state, linear_HOSPBD_15_miles ~ cooper_drive_time_08_14_HOSPBD_30_min)
summary(m1)

m2 <- lm(data = hhi_state, linear_HOSPBD_30_miles ~ cooper_drive_time_08_14_HOSPBD_30_min)
summary(m2)

m3 <- lm(data = hhi_state, linear_HOSPBD_50_miles ~ cooper_drive_time_08_14_HOSPBD_30_min)
summary(m3)



# ------------------------------------------------------------------------------

# OLD  
# ggsave(filename = paste0(out_dir,"AHA_hospital_hhi.pdf"), height = 6, width = 8)
# 
# 
# # State (no labels)
# 
# ggplot(data = HHI_state_new %>% filter(!is.na(MSTATE)),
#        aes(x = YEAR, y = hhi_hospitals)) +
#   geom_line(aes(color = MSTATE)) +
#   geom_label_repel(aes(label = ifelse(YEAR == 2019, MSTATE, "")), nudge_x = 0.35, size = 4) +
#   xlab ("Year") + ylab("Hospital HHI") +
#   ggtitle("State Hospital Herfindahl-Hirschman Index (HHI) using AHA data, 2000-2019") +
#   scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020)) +
#   theme_bw() +
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=15)) +
#   labs(color = "State")
# 
# ggsave(filename = paste0(out_dir,"AHA_hospital_hhi_no_labels.pdf"), height = 6, width = 8)
# 
# 
# 
# # HRR
# 
# ggplot(data = HHI_hrr,
#        aes(x = YEAR, y = hhi, group = hrrnum)) +
#   geom_line(size = .1) +
#   geom_hline(yintercept = 2500, size = 1,  color = "darkred") +
#   geom_hline(yintercept = 1500, size = 1, color = "darkred") +
#   geom_label_repel(aes(label = ifelse(YEAR == 2019 & ( hhi > 6000 | hhi < 500), paste0(hrrcity, ", ", hrrstate), "")), nudge_x = 0.5, size = 4) +
#   xlab ("Year") + ylab("Hospital HHI") +
#   ggtitle("HRR-level Hospital Herfindahl-Hirschman Index (HHI) using AHA data, 2000-2019") +
#   scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2020)) +
#   theme_bw() +
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=15)) 
# 
# ggsave(filename = paste0(out_dir,"hrr_hospital_hhi.pdf"), height = 6, width = 8)
