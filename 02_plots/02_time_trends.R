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

hhi_state_raw_diff_02_22 <- calc_diff("state", 2002, 2022, pct = F, cols_of_interest)
hhi_state_pct_diff_02_22 <-calc_diff("state", 2002, 2022, pct = T, cols_of_interest)
hhi_state_raw_diff_10_19 <- calc_diff("state", 2010, 2019, pct = F, cols_of_interest)
hhi_state_pct_diff_10_19 <-calc_diff("state", 2010, 2019, pct = T, cols_of_interest)

write_xlsx(list("pct_diff_02_22" = hhi_state_pct_diff_02_22, 
                "raw_diff_02_22" = hhi_state_raw_diff_02_22,
                "pct_diff_10_19" = hhi_state_pct_diff_10_19, 
                "raw_diff_10_19" = hhi_state_raw_diff_10_19), 
           paste0(out_dir, "state_hhi_time_trends.xlsx"))

hhi_hrr_raw_diff_02_22 <- calc_diff("hrr", 2002, 2022, pct = F, cols_of_interest)
hhi_hrr_pct_diff_02_22 <-calc_diff("hrr", 2002, 2022, pct = T, cols_of_interest)

write_xlsx(list("pct_diff_02_22" = hhi_hrr_pct_diff_02_22, 
                "raw_diff_02_22" = hhi_hrr_raw_diff_02_22), 
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

pdf(paste0(out_dir, "/hhi_over_time_national.pdf"),
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

# ------------------------------------------------------------------------------------

# Option 1: Small multiples (faceted by method) - CLEAREST
fig_small_multiples <- ggplot(hhi_national, aes(x = year_id, y = HHI)) +
  geom_area(fill = "steelblue", alpha = 0.6) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 2500, linetype = "dashed", color = "grey30", size = 0.5) +
  geom_hline(yintercept = 5000, linetype = "dashed", color = "grey30", size = 0.5) +
  facet_wrap(~col_labels, ncol = 2, scales = "free_y") +
  theme_bw() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "grey90")) +
  labs(x = "Year",
       y = "HHI (population-weighted mean)",
       title = "Hospital Market Concentration by Method, 2000-2022",
       caption = "*All measures use hospital beds market share, general/surgical hospitals")

pdf(paste0(out_dir, "hhi_national_small_multiples.pdf"), width = 10, height = 8)
print(fig_small_multiples)
dev.off()

# Option 2: Grouped by market definition type (geographic vs radius)
hhi_national_grouped <- hhi_national %>%
  mutate(group = case_when(
    grepl("County|Hospital Referral", col_labels) ~ "Geographic boundary",
    grepl("Linear", col_labels) ~ "Linear radius",
    grepl("Drive", col_labels) ~ "Drive-time radius"
  ))

fig_grouped <- ggplot(hhi_national_grouped, aes(x = year_id, y = HHI, 
                                                  color = col_labels, group = col_labels)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 2500, linetype = "dashed", color = "grey30") +
  geom_hline(yintercept = 5000, linetype = "dashed", color = "grey30") +
  facet_wrap(~group, ncol = 1, scales = "free_y") +
  scale_color_manual(values = plot_colors, name = "") +
  theme_bw() +
  theme(legend.position = "right",
        strip.text = element_text(size = 11, face = "bold"),
        strip.background = element_rect(fill = "grey90")) +
  labs(x = "Year",
       y = "HHI (population-weighted mean)",
       title = "Hospital Market Concentration in the United States, 2000-2022")

pdf(paste0(out_dir, "hhi_national_grouped.pdf"), width = 12, height = 10)
print(fig_grouped)
dev.off()

# Option 3: Slope graph (2000 vs 2022 only) - CLEANEST
hhi_slope <- hhi_national %>%
  filter(year_id %in% c(2000, 2022)) %>%
  mutate(col_labels = factor(col_labels, levels = rev(levels(col_labels))))

fig_slope <- ggplot(hhi_slope, aes(x = factor(year_id), y = HHI, 
                                    group = col_labels, color = col_labels)) +
  geom_line(size = 1.5, alpha = 0.8) +
  geom_point(size = 4) +
  geom_text(data = filter(hhi_slope, year_id == 2000),
            aes(label = paste0(col_labels, ": ", round(HHI, 0))),
            hjust = 1.1, size = 3.5) +
  geom_text(data = filter(hhi_slope, year_id == 2022),
            aes(label = round(HHI, 0)),
            hjust = -0.3, size = 3.5) +
  scale_color_manual(values = plot_colors) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold")) +
  labs(x = "", 
       y = "HHI (population-weighted mean)",
       title = "Change in Hospital Market Concentration, 2000 to 2022")

pdf(paste0(out_dir, "hhi_national_slope.pdf"), width = 10, height = 8)
print(fig_slope)
dev.off()

# Option 4: Heatmap showing all years
hhi_heatmap_data <- hhi_national %>%
  mutate(col_labels = factor(col_labels, levels = rev(levels(col_labels))))

fig_heatmap <- ggplot(hhi_heatmap_data, aes(x = year_id, y = col_labels, fill = HHI)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(HHI, 0)), color = "white", fontface = "bold", size = 3) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred",
                       midpoint = 2500, limits = c(0, 10000),
                       name = "HHI") +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold")) +
  labs(x = "Year",
       y = "",
       title = "Hospital Market Concentration by Method, 2000-2022",
       caption = "*All measures use hospital beds market share, general/surgical hospitals")

pdf(paste0(out_dir, "hhi_national_heatmap.pdf"), width = 12, height = 6)
print(fig_heatmap)
dev.off()

# Option 5: Bar chart for just 2022 (current state)
library(forcats)
hhi_2022 <- hhi_national %>%
  filter(year_id == 2022) %>%
  mutate(col_labels = fct_reorder(col_labels, HHI))

fig_bar <- ggplot(hhi_2022, aes(x = HHI, y = col_labels, fill = col_labels)) +
  geom_col() +
  geom_vline(xintercept = 2500, linetype = "dashed", color = "grey30", size = 1) +
  geom_vline(xintercept = 5000, linetype = "dashed", color = "grey30", size = 1) +
  geom_text(aes(label = round(HHI, 0)), hjust = -0.2, fontface = "bold", size = 4) +
  scale_fill_manual(values = plot_colors) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 11),
        plot.title = element_text(face = "bold")) +
  labs(x = "HHI (population-weighted mean)",
       y = "",
       title = "Hospital Market Concentration in 2022 by Geographic Market Size")

pdf(paste0(out_dir, "hhi_national_bar_2022.pdf"), width = 10, height = 6)
print(fig_bar)
dev.off()

# ------------------------------------------------------------------------------

# HEATMAPS: State-level HHI over time

# Create heatmap for each method
heatmap_plot <- ggplot(data = hhi_state[year_id %in% 2002:2023, ], aes(x = year_id, y = postal_code, fill = HHI)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       midpoint = 2500, 
                       limits = c(0, 10000),
                       name = "HHI") +
  facet_wrap(~col_labels, ncol = 3) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10, face = "bold")) +
  xlab("Year") +
  ylab("State") +
  ggtitle("Hospital Market Concentration by State and Method, 2000-2022")

pdf(paste0(out_dir, "heatmaps/hhi_heatmap_by_method_state.pdf"),
    width = 12, height = 16)
print(heatmap_plot)
dev.off()

# Individual heatmaps for each method (cleaner view)
for (method_label in unique(hhi_state$col_labels)) {
  
  method_data <- hhi_state[col_labels == method_label]
  
  p_heatmap <- ggplot(data = method_data, aes(x = year_id, y = postal_code, fill = HHI)) +
    geom_tile(color = "white", size = 0.2) +
    scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                         midpoint = 2500, 
                         limits = c(0, 10000),
                         name = "HHI") +
    geom_vline(xintercept = c(2010, 2015, 2020), linetype = "dashed", 
               color = "grey50", alpha = 0.5) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size = 10),
          plot.title = element_text(size = 12, face = "bold")) +
    scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
    xlab("Year") +
    ylab("State") +
    ggtitle(paste0("Hospital Market Concentration: ", method_label))
  
  # Save with clean filename
  filename <- gsub("[^[:alnum:]]", "_", as.character(method_label))
  
  pdf(paste0(out_dir, "heatmaps/heatmap_", filename, ".pdf"),
      width = 10, height = 12)
  print(p_heatmap)
  dev.off()
}

# Change over time heatmap (using the diff data)
change_heatmap <- hhi_state_pct_diff_02_22 %>%
  melt(id.vars = "postal_code") %>%
  mutate(method = gsub("_pct_diff_2002_2022", "", variable)) %>%
  filter(method %in% custom_vars$cols) %>%
  left_join(custom_vars, by = c("method" = "cols")) %>%
  mutate(postal_code = factor(postal_code, levels = rev(sort(unique(postal_code))))) %>%  # Add this line
  ggplot(aes(x = col_labels, y = postal_code, fill = value)) +  # Remove rev() here
  geom_tile(color = "white", size = 0.2) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", 
                       midpoint = 0,
                       name = "% Change\nin HHI") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        plot.title = element_text(size = 12, face = "bold")) +
  xlab("Method") +
  ylab("State") +
  ggtitle("Percent Change in Hospital Market Concentration by State, 2002-2022")

pdf(paste0(out_dir, "heatmaps/heatmap_percent_change_2002_2022.pdf"),
    width = 10, height = 12)
print(change_heatmap)
dev.off()