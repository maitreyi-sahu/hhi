# HHI time trends - summary stats and line graphs
# Maitreyi Sahu

# ============================================================================

# setup
source('init.R')
source('plot_functions.R')
pacman::p_load(ggrepel, scales, writexl)

# save dir
out_dir <- paste0(plot_dir, "time_trends/")

# load data
hhi_hrr_wide <- read_feather(paste0(data_dir, "processed/compiled_hhi/compiled_hhi_hrr_wide.feather")) %>% setDT()
hhi_state_wide <- read_feather(paste0(data_dir, "processed/compiled_hhi/compiled_hhi_state_wide.feather")) %>%  select(-location_id, -location_name) %>% setDT()

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

pdf(paste0(plot_dir, "/time_trends/hhi_over_time_national.pdf"),
    width = 10, height = 6)
print(national_plot)
dev.off()

p <- 
  ggplot(data = hhi_state, aes(x = year_id, y = HHI, group = (postal_code))) +
  geom_line(aes(color = postal_code)) +
  theme_bw() +
  xlab("Year") +
  ylab("Herfindahl-Hirschman Index") + 
  facet_wrap(~col_labels)

pdf(paste0(plot_dir, "/time_trends/hhi_over_time_state.pdf"),
    width = 10, height = 6)
print(p)
dev.off()

