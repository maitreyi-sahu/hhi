# Compile HHI at the individual, hrr, and state levels AND IMPUTE WHERE MISSING
# Maitreyi Sahu
# Aug 27, 2023

# We aggregate the individual HHI's at the state level as follows:
# - (i) Take the (unweighted) mean of the HHI of the hospitals within an HRR. (we considered weighting by hospital beds, but decided not too because we overweight the big hospitals)
# -	(ii) Assign the HRR-level HHI to our  “modified state-HRR” which doesn’t cross state lines
# - (iii) Pop weight = state-HRR pop / total state pop
# - (iv) Take pop -weighted mean of the state-HRR HHI’s at the state level

# Note, all HHI's are at the individual level except Hanson / direct aggregation
# For Hanson, we assign the HRR-level HHI back to individual hospitals

# NOTE, the Cooper and HCUP datasets add some AHAID-years that didn't exist in our data
# 123200 --> 124,036

# ==============================================================================

rm(list=ls())
pacman::p_load(tidyr, dplyr, data.table, arrow, zoo, ggplot2, Matrix) 
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"
out_dir <- paste0(dir, "compiled_hhi/")
options(scipen = 999)

#===============================================================================

# 1) READ INDIVIDUAL-LEVEL HHI'S

# Load files 
hanson_hrr <- read_feather(paste0(dir, "hanson/hhi_individ_hanson_long.feather"))
hanson_county <-  read_feather(paste0(dir, "hanson/hhi_individ_county_long.feather"))
#cooper_data <- fread(paste0(dir, "cooper/cooper_hhi_formatted.csv"))
hcup_data <- fread(paste0(dir, "hcup/hcup_hhi_individ_linked_aha.csv")) %>% filter(year_id >= 2000)
buffer_drivetime <- read_feather(paste0(dir, "buffer/drive_time/drive_time_hhi_formatted.feather"))
buffer_linear_utm <- read_feather(paste0(dir, "buffer/linear/linear_distance_hhi_formatted.feather"))
#buffer_linear_crs3857 <- fread(paste0(dir, "buffer/linear_crs3857/linear_distance_hhi_formatted.csv"))[, hhi_type := "linear_old"]
# DRIVE TIME INTERSECTS [LATER]

# Files of interest
hhi_files <- list(hanson_hrr, hanson_county, hcup_data, buffer_linear_utm, #buffer_linear_crs3857, 
                  buffer_drivetime) # all except cooper

# Append rows and join the HRR 
hhi_individ <- rbindlist(hhi_files)
ids <- fread(paste0(dir, "id_vars/aha_clean_ids.csv"))[, c("ID", "year_id", "hrrnum", "cnty", "mcnty", "hosp_zip")] %>% distinct() # read hrrnum
hhi_individ <- hhi_individ[ids, on = c("ID", "year_id")] # join hrrnum
# hhi_individ <- rbindlist(list(hhi_individ, cooper_data), fill = T) # cooper data already has the hrrnum
# TODO add county and state-HRR to the cooper data and add in
hhi_individ[, hhi := as.numeric(hhi)][, .(ID, hrrnum, cnty, mcnty, hosp_zip, year_id, hhi_type, market_var, radius, radius_units, hhi, hosp_type)] %>% distinct()
setorder(hhi_individ, ID, year_id, hhi_type, market_var, radius, radius_units, hosp_type) 

rm(hhi_files
   #, cooper_data
   )  

#-------------------------------------------------------------------------------

# 2) AGGREGATE AT HRR and county-levels [unweighted mean of individual]

# hrr
hhi_hrr <- hhi_individ[, .(hrr_hhi = mean(hhi, na.rm = T)), 
                       by = .(hrrnum, year_id, hhi_type, market_var, radius, radius_units, hosp_type)] 

# cnty
hhi_county <- hhi_individ[, .(county_hhi = mean(hhi, na.rm = T)), 
                       by = .(cnty, year_id, hhi_type, market_var, radius, radius_units, hosp_type)] %>%
  filter(year_id %in% 2000:2019)
  #filter(!is.na(cnty)) # some missing from 2020-2022

# mcnty

hhi_mcnty <- hhi_individ[, .(mcnty_hhi = mean(hhi, na.rm = T)), 
                          by = .(mcnty, year_id, hhi_type, market_var, radius, radius_units, hosp_type)] %>% 
  filter(year_id %in% 2000:2019)
  # filter(!is.na(mcnty)) # some missing from 2020-2022

# Note: some HCUP data are lost because there's only a 65% match with AHA ID; we need the AHA data to link to HRR because that's where we get the Zip

# ------------------------------------------------------------------------------

# 3) AGGREGATE AT STATE-LEVEL [pop-weighted mean of the HRR, using the state-HRR weight]

# load state-hrr list
state_hrr <- fread(paste0(dir, "id_vars/aha_state_hrrs.csv")) 

# load pop data
pop_state_hrr <- fread(paste0(dir, "pop/pop_state_hrr.csv")) %>% left_join(state_hrr, by = c("year_id", "state_hrr", "hrrnum")) %>% 
  rename(pop_state_hrr = pop) %>% 
  filter(!is.na(state_hrr))

# aggregate HRR-level HHI's at state level using pop weights
hhi_state <- hhi_hrr %>% # start with HRR-level HHI's 
  
  left_join(state_hrr, by = c("year_id", "hrrnum"), relationship = "many-to-many") %>%  #join the state-HRR 
  
  # pop weight = state-HRR pop / total state pop
  left_join(pop_state_hrr, by = c("year_id", "MSTATE", "hrrnum", "state_hrr")) %>%  
  group_by(year_id, MSTATE, hhi_type, market_var, radius, radius_units, hosp_type) %>% 
  mutate(pop_state = sum(pop_state_hrr, na.rm = T)) %>% ungroup() %>% 
  mutate(pop_weight = pop_state_hrr/ pop_state) %>% 
  
  # calculate hhi as pop-weighted mean
  group_by(year_id, MSTATE, hhi_type, market_var, radius, radius_units, hosp_type) %>% 
  summarize(hhi = weighted.mean(hrr_hhi, w = pop_weight, na.rm = T)) %>% 
  ungroup() %>% 
  
  # cleanup state names
  left_join(fread(paste0(dir, "id_vars/state_ids.csv")), by = "MSTATE") %>%  select(-MSTATE) %>% 
  select(location_id, location_name, postal_code, year_id, everything()) %>% 
  
  # cleanup - ALASKA 2000 and 2001 have an HHI of 0, need to replace with NA
  mutate(state_hhi = ifelse(hhi == 0, NA, hhi)) %>% select(-hhi) %>% 
  
  setDT()

#------------------------

# CHECKS - PLOTS - OK

random_hrrs <- sample(hhi_hrr$hrrnum, 12)
a <- hhi_hrr %>% filter(hrrnum %in% random_hrrs) 
ggplot(data = a) + 
  geom_point(aes(x = year_id, y = hrr_hhi, color= hhi_type)) +
  facet_wrap(~hhi_type) 

random_states <- sample(hhi_state$location_name, 12)
a <- hhi_state %>% filter(location_name %in% random_states) 
ggplot(data = a) + 
  geom_point(aes(x = year_id, y = state_hhi, color= hhi_type)) +
  facet_wrap(~location_name) 

rm(a)

# ------------------------------------------------------------------------------

# -------------------------------------------
# Impute missing mcnty  [TAKES ABOUT AN HOUR]
# -------------------------------------------

counties <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv")
states <- fread("/mnt/share/dex/us_county/maps/states.csv")

counties <- merge(counties, states[,.(abbreviation, state_name)], by=c("state_name"))
merged_counties <- unique(counties[,.(state = abbreviation, mcnty)])

# Use adjacency matrix!
adjmat_file <- "/mnt/share/dex/us_county/maps/shapefiles/adjacency_matrix.rds"
adjmat <- readRDS(adjmat_file)

return_neighbors <- function(mcnty){
  # mcnty is 0 indexed but the matrix is 1 indexed
  loc <- as.numeric(mcnty)  + 1
  return(which(adjmat[loc,]==1))
}

hhi_mcnty <- hhi_mcnty %>% mutate(variable = paste(hhi_type, market_var, radius, radius_units, hosp_type, sep ="_")) %>% 
  select(variable, year_id, mcnty_hhi, mcnty) %>% 
  filter(!is.na(mcnty_hhi))

full_data_fuller <- data.table()

for(q in unique(hhi_mcnty$variable)){

  # q = "drive_time_HOSPBD_30_minutes_gen_surg"
  print(q)
  
  qdata_allyears <- hhi_mcnty[variable == q]
  
  for(y in unique(qdata_allyears$year_id)){
    # y <- 2019
    
    print(y)
    
    qdata <- qdata_allyears[year_id == y]
    
    missing_counties <- merged_counties$mcnty[!(merged_counties$mcnty %in% qdata$mcnty)]
    
    # Determine what fraction of neighbors have data for all the missing counties
    missing_counties_key <- data.table(mis_county = missing_counties)
    for(c in missing_counties){
      
      neighbors <- return_neighbors(c)
      missing_counties_key[mis_county == c, n_neighbors := length(neighbors)]
      missing_counties_key[mis_county == c, n_neighbors_missing := length(neighbors[neighbors %in% missing_counties])]
      
    }
    
    missing_counties_key[, frac_neighbors_w_data := (n_neighbors - n_neighbors_missing)/n_neighbors ]
    missing_county_order <- missing_counties_key[rev(order(frac_neighbors_w_data))]$mis_county
    
    # Now modify in place. That way counties with most of their neighbors missing will have some neighbors populated by the time we try to fill them in
    updated_qdata <- copy(qdata)
    updated_qdata[, imputed_based_on_neighbors :=  0]
    
    for (m in missing_county_order) {
      
      neighbors <- return_neighbors(m)
      neighbor_data <- updated_qdata[mcnty %in% neighbors]
      
      if (nrow(neighbor_data) > 0) {
        new_data <- neighbor_data[, .(mcnty_hhi = mean(mcnty_hhi)), by = .(variable, year_id)]
        new_data[, `:=` (mcnty = m, imputed_based_on_neighbors = 1)]
        updated_qdata <- rbind(updated_qdata, new_data, fill = TRUE)
      }
    }
    
    full_data_fuller <- rbind(full_data_fuller, updated_qdata)
    
  }
}

# ------------------------------------------------------------------------------

# SAVE

hhi_mcnty <- full_data_fuller

files_to_save <- c(
  "hhi_individ", "hhi_hrr", "hhi_state", "hhi_county", "hhi_mcnty"
)

invisible(lapply(seq_along(files_to_save), function(i) write_feather(get(files_to_save[i]), paste0(out_dir, "compiled_", files_to_save[i], ".feather"))))

# ------------------------------------------------------------------------------

# Reshape wide 

hhi_individ_wide <- hhi_individ %>% 
  group_by(hhi_type, market_var, radius, radius_units) %>%  
  pivot_wider(
    names_from = c("hhi_type", "market_var", "radius", "radius_units", "hosp_type"),
    values_from = hhi) 

hhi_hrr_wide <- hhi_hrr %>% 
  pivot_wider(
    names_from = c("hhi_type", "market_var", "radius", "radius_units", "hosp_type"),
    values_from = hrr_hhi ) 

hhi_state_wide <- hhi_state %>% 
  pivot_wider(
    names_from = c("hhi_type", "market_var", "radius", "radius_units", "hosp_type"),
    values_from = state_hhi ) 

hhi_county_wide <- hhi_county %>% 
  pivot_wider(
    names_from = c("hhi_type", "market_var", "radius", "radius_units", "hosp_type"),
    values_from = county_hhi )

hhi_mcnty_wide <- dcast(full_data_fuller, year_id + mcnty ~ variable, value.var = 'mcnty_hhi') %>% as.data.table() %>% 
  left_join(counties %>% select(-cnty, -cnty_name, -current, -location_id) %>% distinct(), by = 'mcnty') %>% 
  select(year_id, mcnty, state, state_name, abbreviation, everything())


# ------------------------------------------------------------------------------

# SAVE

files_to_save <- c(
  "hhi_individ_wide", "hhi_hrr_wide", "hhi_state_wide", "hhi_county_wide", "hhi_mcnty_wide" # wide format
)
invisible(lapply(seq_along(files_to_save), function(i) write_feather(get(files_to_save[i]), paste0(out_dir, "compiled_", files_to_save[i], ".feather"))))

#county_wide <- read_feather(paste0(out_dir, "compiled_hhi_county_wide.feather"))

# ------------------------------------------------------------------------------

# Check county / mcnty NA's

#counties <- fread("/mnt/share/dex/us_county/maps/merged_counties.csv") %>% select(mcnty, cnty) %>% cross_join(data.frame(year_id = 2014:2019)) %>% left_join(hhi_mcnty_wide, by = c('year_id', 'mcnty'))
