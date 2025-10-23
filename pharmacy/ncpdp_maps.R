# Pharmacy maps
# 2/3/2025
# msahu

# ==============================================================================

# SETUP 

rm(list=ls())
pacman::p_load(data.table, dplyr, sf,
               ggplot2, usmap, gridExtra, viridis, scales)

dir <- '/snfs1/Project/IRH/dex_us_county/pharma/ncpdp/'
data_dir <- paste0(dir, 'data/intermediate/')
plot_dir <- paste0(dir, 'plots/descriptives/pharmacy_density_maps/')
options(scipen = 999)

# =============================================================================

# LOAD / FORMAT DATA 

# map boundaries
mcnty_shapefile <- readRDS('/mnt/share/csu/diversity_usa/_general_code/mapping/mcnty_sf_shapefile.rds')
state_shapefile <- readRDS('/mnt/share/csu/diversity_usa/_general_code/mapping/state_sf_shapefile.rds')
state_names <- fread("/mnt/share/dex/us_county/maps/states.csv")[ , .( state_name, state_code = abbreviation)] # be careful, the state IDs are out of date in this file

# ncpdp pharmacies 
state_pharmacies <- fread(paste0(data_dir, 'state_pharmacies.csv')) %>% left_join(state_names, by = 'state_code')
mcnty_pharmacies <- fread(paste0(data_dir, 'mcnty_pharmacies.csv')) 

# ncpdp pharmacies - drive time [STATE]
state_pharm_drive <- fread(paste0(dir, 'data/driving_distance/state_results.csv')) %>% rename(state_code = state)
prop_cols <- grep("_prop$", names(state_pharm_drive), value = T)
state_pharm_drive_wide <- dcast(
  state_pharm_drive,
  state_code + year_id ~ radius,
  value.var = prop_cols
)
setnames(
  state_pharm_drive_wide,
  old = names(state_pharm_drive_wide)[-(1:2)],
  new = sub("^(.*)_([0-9]+)$", "\\1_drive\\2", names(state_pharm_drive_wide)[-(1:2)])
) 

# Process mcnty-level
mcnty_pharm_drive <- fread(paste0(dir, 'data/driving_distance/mcnty_results.csv')) 
prop_cols <- grep("_prop$", names(mcnty_pharm_drive), value = T)
mcnty_pharm_drive_wide <- dcast(
  mcnty_pharm_drive,
  mcnty + year_id ~ radius,
  value.var = prop_cols
)
setnames(
  mcnty_pharm_drive_wide,
  old = names(mcnty_pharm_drive_wide)[-(1:2)],
  new = sub("^(.*)_([0-9]+)$", "\\1_drive\\2", names(mcnty_pharm_drive_wide)[-(1:2)])
) 

# merge 
state_pharmacies <- state_pharmacies %>% left_join(state_pharm_drive_wide, by = c('year_id', 'state_code')) %>% setDT()
mcnty_pharmacies <- mcnty_pharmacies %>% left_join(mcnty_pharm_drive_wide, by = c('year_id', 'mcnty')) %>% setDT()

rm(state_pharm_drive, mcnty_pharm_drive)

# ==============================================================================

# MAP FUNCTION

# vars
pharmacy_vars <- c('community_pharmacies', 'clinic_pharmacies', 'community_or_clinic_pharmacies', 'total_pharmacies')
pharmacy_pc_vars <- paste0(pharmacy_vars, '_p10k')
drive_vars <- c("TOT_prop_drive10", "TOT_prop_drive20",
                "AIAN_prop_drive10", "AIAN_prop_drive20",
                "API_prop_drive10", "API_prop_drive20", 
                "BLCK_prop_drive10", "BLCK_prop_drive20", 
                "HISP_prop_drive10", "HISP_prop_drive20", 
                "OTH_prop_drive10", "OTH_prop_drive20",
                "WHT_prop_drive10", "WHT_prop_drive20")
final_vars <- c(drive_vars, pharmacy_vars, pharmacy_pc_vars)

# labels 
var_labels <- list('total_pharmacies' = 'Total NCPDP pharmacies',
                   'total_pharmacies_p10k' = 'Total NCPDP pharmacies per 10,000 population',
                   'community_pharmacies' = 'Community retail pharmacies',
                   'community_pharmacies_p10k' = 'Community retail pharmacies per 10,000 population',
                   'clinic_pharmacies' = 'Clinic retail pharmacies',
                   'clinic_pharmacies_p10k' = 'Clinic pharmacies per 10,000 population',
                   'community_or_clinic_pharmacies' = 'Community retail or clinic pharmacies',
                   'community_or_clinic_pharmacies_p10k' = 'Community retail or clinic pharmacies per 10,000 population',
                   "TOT_prop_drive10" = "Proportion of the total population living within 10 minutes drive of a pharmacy",
                   "TOT_prop_drive20" = "Proportion of the total population living within 20 minutes drive of a pharmacy",
                   "AIAN_prop_drive10" = "Proportion of the American Indian/Alaska Native population living within 10 minutes drive of a pharmacy",
                   "AIAN_prop_drive20" = "Proportion of the American Indian/Alaska Native population living within 20 minutes drive of a pharmacy",
                   "API_prop_drive10" = "Proportion of the Asian/Pacific Islander population living within 10 minutes drive of a pharmacy",
                   "API_prop_drive20" = "Proportion of the Asian/Pacific Islander population living within 20 minutes drive of a pharmacy",
                   "BLCK_prop_drive10" = "Proportion of the Black population living within 10 minutes drive of a pharmacy",
                   "BLCK_prop_drive20" = "Proportion of the Black population living within 20 minutes drive of a pharmacy",
                   "HISP_prop_drive10" = "Proportion of the Hispanic population living within 10 minutes drive of a pharmacy",
                   "HISP_prop_drive20" = "Proportion of the Hispanic population living within 20 minutes drive of a pharmacy",
                   "OTH_prop_drive10" = "Proportion of the population of other races living within 10 minutes drive of a pharmacy",
                   "OTH_prop_drive20" = "Proportion of the population of other races living within 20 minutes drive of a pharmacy",
                   "WHT_prop_drive10" = "Proportion of the White population living within 10 minutes drive of a pharmacy",
                   "WHT_prop_drive20" = "Proportion of the White population living within 20 minutes drive of a pharmacy"
                   )

make_map <- function(lvl, var, year) {
  
  # lvl = 'state'
  # var = 'TOT_prop_drive20'
  # y = 2019
  
  # data
  mergeVar = switch(lvl,
                    'mcnty' = 'mcnty',
                    'state' = 'state_name')
  plot_data <- get(paste0(lvl, "_pharmacies"))[, .SD, .SDcols = c('year_id', var, mergeVar)] %>%  filter(!is.na(.data[[var]]))
  plot_data <- merge(get(paste0(lvl, "_shapefile")), plot_data, by = mergeVar)
 
  # colors / breaks
  
  if (grepl("p10k$", var)) {
    color_data <- rbind(state_pharmacies[ , ..var], mcnty_pharmacies[, ..var])
    round_no = 2
  } else {
    color_data <- get(paste0(lvl, "_pharmacies"))[ , ..var]
    round_no = 1
  }
  
  brks_raw <- sapply(seq(0, 1, by = 1/7), function(x) quantile(color_data[[var]], x, na.rm = T))
  brks <- unique(brks_raw)
  
  # if you end up with fewer than 8 breaks, you can't make 7 bins
  # if (length(brks) < 8) {
  #   stop("Not enough unique quantiles to create 7 bins")
  # }
  # 
  # create labels
  round_no <- 2  # or whatever number of decimals you want
  labs <- paste0(round(brks[-length(brks)], round_no), " - ", round(brks[-1], round_no))
  
  cols = c(#"#fbfbfb", 
    "#f3e7f4", "#e0b5e4", "#d696de", "#cb77d7", "#b12bc9", "#6a0096", "#3f0061")

  
  # cut the data
  plot_data$plot_val <- cut(plot_data[[var]],
                            breaks = brks,
                            labels = labs,
                            include.lowest = T)

  # map!
  p <- ggplot(data = plot_data %>% filter(year_id == y)) +
    geom_sf(aes(fill = plot_val, geometry = geometry), color = NA) + # counties w/o border
    geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
    labs(title =  paste0(var_labels[[var]],", ", y),
         fill = "") +
    scale_fill_manual(values = cols,
                      breaks = levels(factor(plot_data$plot_val))[levels(factor(plot_data$plot_val)) != "NA"],
                      na.value = "grey70") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          title = element_text(size = 12),
          text = element_text(size = 10),
          plot.margin = margin(0.5, 0, 1, 0, "cm"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank()) +
    guides(fill = guide_legend(nrow = 1))
  
  return(p)
}

make_diff_map <- function(lvl, var, pct = T, start_year, end_year) {
  
  # lvl = 'state'
  # var = 'clinic_pharmacies_p10k'
  # start_year = 2010
  # end_year = 2019
  # pct = F
  
  # data
  mergeVar = switch(lvl,
                    'mcnty' = 'mcnty',
                    'state' = 'state_name')
  if (pct == T) {
    plotVar = 'pct_diff'
  } else if (pct == F) {
    plotVar = 'diff'
  }
  
  # values for start and end years
  plot_data <- merge(
    get(paste0(lvl, "_pharmacies"))[year_id == start_year, setNames(.(
      get(var), get(mergeVar)), c("start_val", mergeVar))],
    get(paste0(lvl, "_pharmacies"))[year_id == end_year, setNames(.(
      get(var), get(mergeVar)), c("end_val", mergeVar))],
    by = mergeVar, all = T
  )
  
  # Compute differences
  plot_data <- plot_data[, `:=`(
    diff = end_val - start_val,
    prop_diff = fifelse(start_val == 0, 
                        fifelse(end_val == 0, 0, NA_real_), 
                        (end_val - start_val) / start_val)
  )][, pct_diff := prop_diff * 100]
  
  plot_data <- merge(get(paste0(lvl, "_shapefile")), plot_data, by = mergeVar)
  
  # breaks
  num_bins <- 14  # Total number of bins (must be even to ensure symmetry)
  abs_brks <- quantile(abs(plot_data[[plotVar]]), probs = seq(0, 1, length.out = num_bins/2), na.rm = T)
  neg_brks <- -rev(abs_brks[abs_brks != 0])  # Mirror negative values without 0
  if (pct == F & min(abs_brks) < .01 ) { 
    brks <- c(neg_brks, abs_brks[abs_brks != 0])
    round_no <- 3
  } else{ 
    brks <- c(neg_brks, -0.01, 0, 0.01, abs_brks[abs_brks != 0])
    round_no <- 2
    }
  brks <- unique(brks)
  
  # labels
  if (pct == T) { 
    labs <- paste0(round(brks[-length(brks)], round_no), "% - ", round(brks[-1], round_no), "%")
    title_lab <- 'Percent change in '
  } else if (pct == F) { 
    labs <- paste0(round(brks[-length(brks)], round_no), " - ", round(brks[-1], round_no))
    title_lab <- 'Change in '
      }
  
  # define colors
  pos_colors <- rev(c("#B12BC9", "#BC4FCF", "#C76DD5", "#D188DB", "#DAA3E0", "#E2BDE6"))  
  white_middle <- c("#FbFbFb", "#FcFcFc")  # White shades in the middle
  neg_colors <- c("#ff9809", "#FFA431", "#FFB059", "#FFBD7A", "#FFCA97", "#FDD7B4")  #
  
  # Dynamically generate color ramp ensuring white stays in the middle
  if (length(brks) <= num_bins + 1) {
    neg_colors <- colorRampPalette(neg_colors)(length(neg_brks))
    pos_colors <- colorRampPalette(pos_colors)(length(neg_brks))
  } else {
    neg_colors <- colorRampPalette(neg_colors)(floor((num_bins - length(white_middle)) / 2))
    pos_colors <- colorRampPalette(pos_colors)(floor((num_bins - length(white_middle)) / 2))
  }
  
  # Final color ramp
  color_ramp <- c(neg_colors, white_middle, pos_colors)
  cols <- colorRampPalette(color_ramp)(length(brks)-1)
  
  # Cut data into bins
  plot_data$plot_val <- factor(cut(plot_data[[plotVar]], breaks = brks, labels = labs, include.lowest = T), levels = labs)

  # Map!
  p <- ggplot(data = plot_data) +
    geom_sf(aes(fill = plot_val, geometry = geometry), color = NA, show.legend = T) + # counties w/o border
    geom_sf(data = state_shapefile, fill = NA, linewidth = .4) +
    labs(title =  paste0(title_lab, tolower(var_labels[[var]]),", ", start_year, " - ", end_year),
         fill = "") +
    scale_fill_manual(values = cols,
                      labels = labs,
                      drop = F,
                      na.value = "grey70") +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          title = element_text(size = 12),
          text = element_text(size = 10),
          plot.margin = margin(0.5, 0, 1, 0, "cm"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank()) +
    guides(fill = guide_legend(nrow = 2))
  
  return(p)
}


# ==============================================================================

# PLOT: different file for each pharmacy type; each page is year; left is state; right is county

plot_vars <- final_vars

# STATE/COUNTY-LEVEL PHARMACIES OVER TIME --------------------------------------

for (v in plot_vars) {
  
  print(v)
  
  if (grepl('p10k', v)==T) {
    f <- 'per_capita'
  } else {
    f <- 'total'
  }
    
  pdf(paste0(plot_dir,"/pharmacies_by_year/", f, "/", v, "_density.pdf"), width = 20, height = 8, onefile = TRUE)

   for(y in 2010:2019){

     p_left <- make_map(lvl = 'state', var = v, year = y)

     p_right <- make_map(lvl = 'mcnty', var = v, year = y)

     grid.arrange(p_left, p_right, ncol = 2)

     }

  dev.off()
  
}

# CHANGE IN STATE/COUNTY-LEVEL PHARMACIES, 2010-2019 ---------------------------

# % CHANGE

for (v in plot_vars) {
  
  print(v)
  
  if (grepl('p10k', v)==T) {
    f <- 'per_capita'
  } else {
    f <- 'total'
  }
  
  pdf(paste0(plot_dir,"change_2010_2019/", f, "/", v, "_pct_diff_10_19.pdf"), width = 24, height = 8, onefile = T)
    
    p_left <- make_diff_map(lvl = 'state', var = v, start_year = 2010, end_year = 2019)
    
    p_right <- make_diff_map(lvl = 'mcnty', var = v, start_year = 2010, end_year = 2019)
    
    grid.arrange(p_left, p_right, ncol = 2)
  
  dev.off()
  
}
  
# LEVEL CHANGE

for (v in plot_vars) {
  
  print(v)
  
  if (grepl('p10k', v)==T) {
    f <- 'per_capita'
  } else {
    f <- 'total'
  }
  
  pdf(paste0(plot_dir,"change_2010_2019/", f, "/", v, "_raw_diff_10_19.pdf"), width = 24, height = 8, onefile = T)
  
    p_left <- make_diff_map(lvl = 'state', var = v, pct = F, start_year = 2010, end_year = 2019)
    
    p_right <- make_diff_map(lvl = 'mcnty', var = v, pct = F, start_year = 2010, end_year = 2019)
  
  grid.arrange(p_left, p_right, ncol = 2)
  
  dev.off()

}
