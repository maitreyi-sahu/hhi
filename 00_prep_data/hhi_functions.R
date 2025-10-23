# MSahu
# March 23, 2024

# HHI FUNCTION (used for Hanson and Buffers [Linear, Drive Time])

# HHI using 4 market share vars - HOSPBD, IPDTOT, ADMTOT, MCRIPD
# Hospitals in the same system are the same [use "pref_ID" for the hospital ID]

# NOTE: can combine the HRR + county functions into one function

# ==============================================================================

pacman::p_load(dplyr, data.table)

final_cols <- c("hhi_beds", "hhi_admits", "hhi_pt_days", "hhi_mcr_pt_days", 
                "tot_beds", "tot_admits", "tot_pt_days", "tot_mcr_pt_days", 
                "aha_hosp_n", "aha_systems_n")

final_cols_hrr <- c("year_id", "hrrnum", c(final_cols))
final_cols_cnty <- c("year_id", "cnty", c(final_cols))
final_cols_mcnty <- c("year_id", "mcnty", c(final_cols))

final_cols_individ <- c("year_id", "ID", c(final_cols), "radius", "hosp_type")

# ------------------------------------------------------------------------------

###############################################################
# FUNCTION TO AGGREGATE HHI AT HRR LEVEL [HANSON METHOD]
# Input: hospital-level df with "hrrnum" and all years
###############################################################

# Testing only:
# df <- fread("/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/aha_clean_2000_2019.csv") %>% head(1000)

aggregate_hhi_hrr <- function (df) {
  
  aggregated_hhi_hrr <- setDT(df)[ , `:=` ( 
  
    # Add HRR-level total beds / inpt days/ etc
      hrr_beds = sum(HOSPBD),
      hrr_admits = sum(ADMTOT),
      hrr_pt_days = sum(IPDTOT),
      hrr_mcr_pt_days = sum(MCRIPD),
      hrr_hosp_n = .N
    ), by = .(year_id, hrrnum)][, .(
    
    # Summarize system- and HRR-level total beds / inpt days/ etc 
      system_beds = sum(HOSPBD),
      system_admits = sum(ADMTOT),
      system_pt_days = sum(IPDTOT),
      system_mcr_pt_days = sum(MCRIPD),
      
      hrr_beds = mean(hrr_beds),
      hrr_admits = mean(hrr_admits),
      hrr_pt_days = mean(hrr_pt_days),
      hrr_mcr_pt_days = mean(hrr_mcr_pt_days),
      hrr_hosp_n = mean(hrr_hosp_n)
    ), by = .(year_id, hrrnum, pref_ID)][,`:=` (
    
    # Add market share as a readable percentage
      market_share_beds = system_beds / hrr_beds * 100,
      market_share_admits = system_admits / hrr_admits * 100,
      market_share_pt_days = system_pt_days / hrr_pt_days * 100,
      market_share_mcr_pt_days = system_mcr_pt_days / hrr_mcr_pt_days * 100), 
    by = .(year_id, hrrnum)][, .(
    
    # Summarize sum of squared market share and totals at HRR level
      hhi_beds = sum(market_share_beds^2),
      hhi_admits = sum(market_share_admits^2),
      hhi_pt_days = sum(market_share_pt_days^2),
      hhi_mcr_pt_days = sum(market_share_mcr_pt_days^2),
      
      tot_beds = mean(hrr_beds),
      tot_admits = mean(hrr_admits),
      tot_pt_days = mean(hrr_pt_days),
      tot_mcr_pt_days = mean(hrr_mcr_pt_days),
      
      aha_hosp_n = mean(hrr_hosp_n),
      aha_systems_n = .N),
    by = .(year_id, hrrnum)]
  
  # Select/order final columns
  aggregated_hhi_hrr <- as.data.frame(aggregated_hhi_hrr[order(year_id, hrrnum), ..final_cols_hrr]) 
  
  return(aggregated_hhi_hrr)
}

############################################################
# FUNCTION TO AGGREGATE HHI AT COUNTY LEVEL [HANSON METHOD]
# Input: hospital-level df with "cnty" and all years
############################################################

aggregate_hhi_county <- function (df) {
  
  aggregated_hhi_cnty <- setDT(df)[ , `:=` (
    
    # Add HRR-level total beds / inpt days/ etc
    cnty_beds = sum(HOSPBD),
    cnty_admits = sum(ADMTOT),
    cnty_pt_days = sum(IPDTOT),
    cnty_mcr_pt_days = sum(MCRIPD),
    cnty_hosp_n = .N
  ), by = .(year_id, cnty)][, .(
    
    # Summarize system- and county-level total beds / inpt days/ etc 
    system_beds = sum(HOSPBD),
    system_admits = sum(ADMTOT),
    system_pt_days = sum(IPDTOT),
    system_mcr_pt_days = sum(MCRIPD),
    
    cnty_beds = mean(cnty_beds),
    cnty_admits = mean(cnty_admits),
    cnty_pt_days = mean(cnty_pt_days),
    cnty_mcr_pt_days = mean(cnty_mcr_pt_days),
    cnty_hosp_n = mean(cnty_hosp_n)
  ), by = .(year_id, cnty, pref_ID)][,`:=` (
    
    # Add market share as a readable percentage
    market_share_beds = system_beds / cnty_beds * 100,
    market_share_admits = system_admits / cnty_admits * 100,
    market_share_pt_days = system_pt_days / cnty_pt_days * 100,
    market_share_mcr_pt_days = system_mcr_pt_days / cnty_mcr_pt_days * 100), 
    by = .(year_id, cnty)][, .(
      
      # Summarize sum of squared market share and totals at cnty level
      hhi_beds = sum(market_share_beds^2),
      hhi_admits = sum(market_share_admits^2),
      hhi_pt_days = sum(market_share_pt_days^2),
      hhi_mcr_pt_days = sum(market_share_mcr_pt_days^2),
      
      tot_beds = mean(cnty_beds),
      tot_admits = mean(cnty_admits),
      tot_pt_days = mean(cnty_pt_days),
      tot_mcr_pt_days = mean(cnty_mcr_pt_days),
      
      aha_hosp_n = mean(cnty_hosp_n),
      aha_systems_n = .N),
      by = .(year_id, cnty)]
  
  # Select/order final columns
  aggregated_hhi_cnty <- as.data.frame(aggregated_hhi_cnty[order(year_id, cnty), ..final_cols_cnty]) 
  
  return(aggregated_hhi_cnty)
}


############################################################
# FUNCTION TO AGGREGATE HHI AT MCNTY LEVEL [HANSON METHOD]
# Input: hospital-level df with "mcnty" and all years
############################################################

aggregate_hhi_mcnty <- function (df) {
  
  aggregated_hhi_mcnty <- setDT(df)[ , `:=` (
    
    # Add HRR-level total beds / inpt days/ etc
    cnty_beds = sum(HOSPBD),
    cnty_admits = sum(ADMTOT),
    cnty_pt_days = sum(IPDTOT),
    cnty_mcr_pt_days = sum(MCRIPD),
    cnty_hosp_n = .N
  ), by = .(year_id, mcnty)][, .(
    
    # Summarize system- and county-level total beds / inpt days/ etc 
    system_beds = sum(HOSPBD),
    system_admits = sum(ADMTOT),
    system_pt_days = sum(IPDTOT),
    system_mcr_pt_days = sum(MCRIPD),
    
    cnty_beds = mean(cnty_beds),
    cnty_admits = mean(cnty_admits),
    cnty_pt_days = mean(cnty_pt_days),
    cnty_mcr_pt_days = mean(cnty_mcr_pt_days),
    cnty_hosp_n = mean(cnty_hosp_n)
  ), by = .(year_id, mcnty, pref_ID)][,`:=` (
    
    # Add market share as a readable percentage
    market_share_beds = system_beds / cnty_beds * 100,
    market_share_admits = system_admits / cnty_admits * 100,
    market_share_pt_days = system_pt_days / cnty_pt_days * 100,
    market_share_mcr_pt_days = system_mcr_pt_days / cnty_mcr_pt_days * 100), 
    by = .(year_id, mcnty)][, .(
      
      # Summarize sum of squared market share and totals at cnty level
      hhi_beds = sum(market_share_beds^2),
      hhi_admits = sum(market_share_admits^2),
      hhi_pt_days = sum(market_share_pt_days^2),
      hhi_mcr_pt_days = sum(market_share_mcr_pt_days^2),
      
      tot_beds = mean(cnty_beds),
      tot_admits = mean(cnty_admits),
      tot_pt_days = mean(cnty_pt_days),
      tot_mcr_pt_days = mean(cnty_mcr_pt_days),
      
      aha_hosp_n = mean(cnty_hosp_n),
      aha_systems_n = .N),
      by = .(year_id, mcnty)]
  
  # Select/order final columns
  aggregated_hhi_mcnty <- as.data.frame(aggregated_hhi_mcnty[order(year_id, mcnty), ..final_cols_mcnty]) 
  
  return(aggregated_hhi_mcnty)
}

##############################################################################
# FUNCTION TO GET INDIVIDUAL HOSPITAL'S HHI USING BUFFER
# Inputs: df filtered to intersected hospitals, hosp id, year_id, and radius
# Output: collapses to one row per hospital-year-radius
##############################################################################

# Testing only:
# df <- intersecting_hospitals
# current_radius <- 30
# current_year <- 2019
# current_id = "0006940110"
# current_hosp_type = "all"

aggregate_hhi_buffer <- function (df, current_id, current_year, current_radius, current_hosp_type) {
    
  aggregated_hhi_buffer <- setDT(df)[ , `:=` ( 
    
    # Add total hospital beds/ inpt days/ etc. [already at the level we need]
      tot_beds = sum(HOSPBD),
      tot_admits = sum(ADMTOT),
      tot_pt_days = sum(IPDTOT),
      tot_mcr_pt_days = sum(MCRIPD),
      tot_hosp_n = .N
    )][, .(
    
    # Summarize system- and HRR-level total beds / inpt days/ etc 
      system_beds = sum(HOSPBD),
      system_admits = sum(ADMTOT),
      system_pt_days = sum(IPDTOT),
      system_mcr_pt_days = sum(MCRIPD),
      
      tot_beds = mean(tot_beds),
      tot_admits = mean(tot_admits),
      tot_pt_days = mean(tot_pt_days),
      tot_mcr_pt_days = mean(tot_mcr_pt_days),
      tot_hosp_n = mean(tot_hosp_n)
    ), by = .(pref_ID)][, `:=` (
    
    # Add market share as a readable percentage
      market_share_beds = system_beds / tot_beds * 100,
      market_share_admits = system_admits / tot_admits * 100,
      market_share_pt_days = system_pt_days / tot_pt_days * 100,
      market_share_mcr_pt_days = system_mcr_pt_days / tot_mcr_pt_days * 100)][, .(
    
    # Summarize sum of squared market share and totals at HRR level
      hhi_beds = sum(market_share_beds^2),
      hhi_admits = sum(market_share_admits^2),
      hhi_pt_days = sum(market_share_pt_days^2),
      hhi_mcr_pt_days = sum(market_share_mcr_pt_days^2),
      
      tot_beds = mean(tot_beds),
      tot_admits = mean(tot_admits),
      tot_pt_days = mean(tot_pt_days),
      tot_mcr_pt_days = mean(tot_mcr_pt_days),
      
      aha_hosp_n = mean(tot_hosp_n),
      aha_systems_n = .N)]
    
    # Select/order final columns
    aggregated_hhi_buffer <- as.data.frame(aggregated_hhi_buffer[, `:=` (ID = current_id, year_id = current_year, radius = current_radius, hosp_type = current_hosp_type)][, ..final_cols_individ])
  
  return(aggregated_hhi_buffer)
}

