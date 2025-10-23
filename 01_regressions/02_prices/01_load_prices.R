# Import price data (no draws) and run regressions
# MSahu
# April 9, 2024

# TODO switch to draws?

rm(list=ls())
pacman::p_load(tidyverse, dplyr, arrow, plm, data.table)

dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"
out_dir <- paste0(dir, "plots/regressions/prices/")

rural <- fread("/mnt/share/dex/us_county/05_analysis/inputs/county_ushd/ushd_covs_county.csv")[covariate == "rural" , ]

source("~/repos/us_value/1_data_compilation/4_hospital_hhi/02_plots/plot_functions.R")
cols_of_interest <- c(custom_vars$cols)

level = 'county'

scaled_version = '102' # UPDATE THIS

# TODO : 
# 1. Look at r-squared
# 2. Normalize the HHI's
# 3. log the prices


# ------------------------------------------------------------------------------

# FUNCTION TO MAKE PANEL DATA FRAME

# PAYER

# read data
# 1. first pass regression
# 2. plot prices versus HHI --> inspect 
# TODO RATIO OF PRIVATE TO MEDICARE
# TODO update '10-unit increase' to new code
# TODO switch to draws
# TODO age-standardize spending per capita
# TODO regression fixed effects - WANT fixed effect for time and explore effect for state
# TODO rural/urban, pop density, income? 
# TODO IRPD-adjust the prices
# TODO explore lagging by 1-2 years
# TODO explore other payers


get_pdata <- function(level, PAYER, ratio = F, ratio_payer = 'mdcr') {
  
 # level = 'county'
 # PAYER <- "priv"
  # ratio = T
  # ratio_payer = 'mdcr'
  
  levelR <- switch(level,
                    'county' = 'mcnty',
                    'state' = 'state')
  
  # READ DEX DATA
  path <- paste0("/mnt/share/dex/us_county/04_final/scaled_version_", scaled_version, "/collapsed/data/geo=", level, "/toc=IP/")
  
  if (ratio == F) {
  
    dex_data <- open_dataset(path) %>% 
      filter(payer == PAYER) %>%
      group_by(year_id, location) %>% 
      summarize(vol = sum(mean_vol),
                spend = sum(mean_spend)) %>% 
      collect() %>%
      setDT() 
    
    dex_data <- dex_data[ , spend_per_vol := spend/vol]
  
  } else if (ratio == T) {
    
    dex_data <- open_dataset(path) %>% 
      filter(payer == PAYER | payer == ratio_payer) %>%
      group_by(year_id, location, payer) %>% 
      summarize(vol = sum(mean_vol),
                spend = sum(mean_spend)) %>% 
      collect() %>%
      setDT() 
    
    dex_data <- dex_data[ , spend_per_vol := spend/vol]
    
    dex_data <- dcast(dex_data, year_id + location ~ payer, value.var = "spend_per_vol")
    dex_data <- dex_data[, spend_per_vol := get(PAYER) / get(ratio_payer), by = c('year_id', 'location')] %>% select(year_id, location, spend_per_vol)
    
  }
  
  if (level == 'county') {dex_data <- dex_data[ , location := as.integer(location)]}
  setnames(dex_data, "location", levelR)
  
  # READ HHI
  level_cols <- switch(level,
                       'county' = c("mcnty", "state", "state_name"),
                       'state' = c("location_name","postal_code"))
  select_cols <- c('year_id', level_cols, cols_of_interest)
  
  hhi_wide <- read_feather(paste0(dir, "processed/compiled_hhi/compiled_hhi_", levelR, "_wide.feather")) %>%  
    select(all_of(select_cols)) %>% 
    setDT()
  
  if (level == 'state') {hhi_wide <- hhi_wide %>% rename(state_name = location_name, state = postal_code)}
  
  hhi_wide <- hhi_wide[year_id %in% 2010:2019, ]
  
  # Set up pdataframe
  IDcols <- c("year_id", levelR)
  reg_data <- merge(dex_data, hhi_wide, by = IDcols)
  reg_data <- pdata.frame(reg_data, index = IDcols)
  
  return(reg_data)
  
}


# ------------------------------------------------------------------------------

# PLOT FUNCTION

forest_plot <- function(DT, payer_label) {

 P <- ggplot( data = DT) +
  
  ## line
  geom_vline(xintercept = 0) +
  
  ## titles
  labs(
    x = "Change in hospital spending per admission associated \n with a one-unit increase in HHI",
    y = "", 
    title = "Association between hospital market\n consolidation and inpatient prices",
    subtitle = paste0("Payer: ", payer_label)
  ) +
  
  ## points and lines
  geom_point(aes(x = beta, y = col_labels, color = sign), size=3.5) +
  geom_segment(aes(x = beta_lower, xend = beta_upper, y = col_labels, yend = col_labels,  color = sign), size=.75) + 
  
  # Background lines   
  # scale_x_continuous(minor_breaks = c(-4:1)) +
  
  ## set colors
  scale_color_manual(values = colors) + 
  
  ## facet
  # facet_wrap(~group, ncol = 1) +
  
  ## limits
  # xlim(c(dt[,min(beta_lower)], dt[,max(beta_upper)])) +
  
  # variable labels
  # geom_text(aes(x = -40, y = variable, label = paste0("(",year_labels,")")), color = "navy") +
  
  ## theme options
  theme_bw() + 
  theme(
    legend.background = element_rect(colour="grey80"),
    legend.title = element_blank(),
    #  axis.text.y= element_text(face='plain',size=12, color = DT[order(variable), color]),
    strip.background = element_rect(fill="#bcc4ca"), 
    plot.margin=grid::unit(c(0,0,0,0), "mm"),
    text = element_text(size = 20)) +
  
  # no legend
  theme(legend.position = "none")

  return(P)
  
}

# ------------------------------------------------------------------------------

# LOOP THROUGH ALL



for (PAYER in c("priv", "mdcr", "mdcd", "oop")) {
  
  for (level in c("county", "state")) {

    #for (ratio in c(T, F)) {
    for (ratio in c(T)) {
        
    if (ratio == F) {
      ratio_payer = F
    } else if (ratio == T) {
      ratio_payer = 'mdcr'
    }
    
    # level = 'state'
    # payer = 'priv'
    
    payer_label <- switch (PAYER,
                           "priv" = "Commercial",
                           "mdcr" = "Medicare",
                           "mdcd" = "Medicaid",
                           "oop" = "Out-of-pocket"
    )
    
    # RUN regression
    
    reg_data <- get_pdata(level, PAYER, ratio)
    custom_covs <- cols_of_interest
    
    # Loop through and bind
    
    reg_results <- data.frame(variable = character(),
                              beta = numeric(),
                              se = numeric(),
                              p.val = numeric())
    
    for (c in unique(custom_covs)) {
      
      # regressions
      reg <- plm(get(c) ~ spend_per_vol, data = reg_data, model = "within", effect = "time")
      reg_summary <- summary(reg)
      
      ## Extract coefficients from this run
      pe <- coef(reg)
      
      ## Choose a variance-covariance matrix
      vc <- vcovHC(reg,  method = "white1")  
      se <- sqrt(diag(vc))
      
      # save
      reg_summary <- data.frame(variable = c, 
                                beta = pe, 
                                se = se, 
                                p.val = summary(reg)$coefficients[, "Pr(>|t|)"]
      )
      reg_results <- rbind(reg_results, reg_summary)
    }
    
    # Add labels
    
    DT <- reg_results %>% left_join(custom_vars, by = c("variable" = "cols")) %>% 
      mutate(beta = beta, se =se) %>%  # interpreted as a 10-unit increase!
      mutate(beta_lower = beta - qnorm(.975) * se,
             beta_upper = beta + qnorm(.975)*se) %>% setDT()
    DT[(sign(beta_lower) == sign(beta) & sign(beta) == sign(beta_upper)), sig := T]
    DT[is.na(sig), sig := F]
    
    ## assign direction
    DT[sig == T & beta > 0, sign := "Significantly positive"]
    DT[sig == T & beta < 0, sign := "Significantly negative"]
    DT[sig == F, sign := "Not significant"]
    
    ## assign colors
    colors <- c(
      "Significantly positive" = "mediumseagreen",
      "Significantly negative" = "#CD1D2C",
      "Not significant" =  "black"
    )    
    
    P <- forest_plot(DT, payer_label)
    
    # SAVE PLOT
    file_path <- paste0(plot_dir, "price_regressions/forest_plot_", PAYER, "_", level,"_ratio=", ratio_payer, ".pdf")
    pdf(file_path, width = 8, height = 8)
    par(mar=c(0,0,0,0)+0.5)
    print(P)
    dev.off()
    
    }
  }
}


# ==============================================================================

# Correlation coefficients

# Loop through and bind
# 
# reg_results <- data.frame(variable = character(),
#                           beta = numeric(),
#                           se = numeric(),
#                           p.val = numeric())
# 
# for (c in unique(custom_covs)) {
#   
#   # regressions
#   reg <- plm(get(c) ~ spend_per_vol, data = reg_data, model = "within", effect = "time")
#   reg_summary <- summary(reg)
#   
#   ## Extract coefficients from this run
#   pe <- coef(reg)
#   
#   ## Choose a variance-covariance matrix
#   vc <- vcovHC(reg,  method = "white1")  
#   se <- sqrt(diag(vc))
#   
#   # save
#   reg_summary <- data.frame(variable = c, 
#                             beta = pe, 
#                             se = se, 
#                             p.val = summary(reg)$coefficients[, "Pr(>|t|)"]
#   )
#   reg_results <- rbind(reg_results, reg_summary)
# }
# 
