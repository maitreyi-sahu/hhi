# ==============================================================================

# LOAD AND CLEAN AHA DATA; MAP HOSPITALS TO HRRs and State-HRRs

# MSahu
# July 3, 2023

# STEPS
# 1: load and clean AHA hospital data for 2000-2019
# 2: load zip to HRR map from Dartmouth Atlas
# 3: map the AHA hospitals to HRR, using hospital zip
# 4: also, create our own modified State-HRRs which don't cross state lines
# 5: check how well it mapped and save
# 6: finally, create a binary indicator variable for whether a general / surgical hospital

# Inputs:
#   1) AHA hospital data (AHA data purchased from Mark Brand; received on 12/8)
#   2) Zip to HRR map (From Dartmouth Atlas: https://data.dartmouthatlas.org/supplemental/)

# Output:
#    1) aha_intermediate_2000_2019: cleaned individual hospital data with HRR and state-HRR

# ==============================================================================

rm(list=ls())

# Packages 
pacman::p_load(data.table, dplyr, stringr, readxl, reshape2, arrow)

# Directories
dir <- "/mnt/share/resource_tracking/us_value/data/"
in_dir <- paste0(dir, "hospital_hhi/raw/aha/AHA raw data/")
out_dir <- paste0(dir, "hospital_hhi/processed/")

# Load state abbreviations
loc_ids <- read.csv(paste0(dir, "/archive/laura_postal_codes.csv"))
postal_codes <- loc_ids[ , "postal_code"]

# Define function to extract last x characters from string
substrRight <- function(x, n){ substr(x, nchar(x)-n+1, nchar(x))}

# ==============================================================================

# STEP 1: Read and clean AHA data 

# Load 
aha_data <- paste0(in_dir, "2022_11_University of Washington.xlsx")
col_names <- c("YEAR", names(read_excel(aha_data, sheet = "FY2000")))
table <- data.frame(matrix(ncol = length(col_names), nrow = 0))

# Loop through sheets and bind
for (i in c(excel_sheets(aha_data)[-1])) {
  temp <- read_excel(aha_data, sheet = i, col_types = "text") %>% mutate(YEAR = i) %>% select(YEAR, everything())
  table <- plyr::rbind.fill(table, temp)
}

# Clean up / rename
origin <- "1900-01-01" # for dates

aha_temp <- table[,-c(1:40)] %>% 
  
  mutate(YEAR = as.numeric(substring(YEAR, 3, 7)),
         DTBEG = as.Date(as.numeric(DTBEG), origin = origin),
         DTEND = as.Date(as.numeric(DTEND), origin = origin),
         HOSPBD = as.numeric(HOSPBD), # hospital beds
         ADMTOT = as.numeric(ADMTOT), # total admissions
         IPDTOT = as.numeric(IPDTOT), # total inpatient days
         MCRIPD = as.numeric(MCRIPD), # Medicare inpatient days
         
         # Create a "preferred ID", which is the hospital ID if no system, and system ID if in a system
         in_system = ifelse(is.na(SYSID), F, T),
         pref_ID = ifelse(in_system == T, SYSID, ID)) %>% 
  
  # Remove territories
  filter(MSTATE %in% postal_codes) %>% 
  
  # Add leading 0's --> all should be 10 characters
  # Currently, 2000 and 2001 are 7 characters and 2002-2019 are 10 characters 
  mutate(ID = str_pad(ID, width = 10, side = "left", pad = "0")) # use stringr package because "sprintf" is adding trailing spaces...

# Check - now all 10 characters
# aha_temp$nchar <- nchar(aha_temp$ID)
# table(aha_temp$nchar, aha_temp$YEAR)

# ================================================================================================================================

# STEP 2: Read and clean Zip to HRR map (From Dartmouth Atlas)

zip_dir <- paste0(dir, "hospital_hhi/raw/dartmouth_atlas/zip_to_hrr/")

my_list <- paste0(zip_dir, list.files(zip_dir))

# Function to format the Dartmouth Atlas Excel files
formatXLS <- function (file) {
  
  year <- paste0("20", substrRight(sub(".xls.*", "", file), 2))   
  
  formattedFile <- read_excel(file) %>% rename(zip = 1) %>%  # 1st column is the zip code
    mutate(YEAR = as.numeric(year)) %>% select(YEAR, zip, everything())
  
  return(formattedFile)
  
}

# Append files and format
zip_df <- data.table::rbindlist(lapply(my_list, FUN = formatXLS)) 

# ------------------------------------------------------------------------------

# STEP 3: Apply zip to HRR map to AHA data and create our own State-HRRs which don't cross state lines
# Use the zip code for the HOSPITAL (not the system!)

aha_clean <- aha_temp %>% setDT() %>% 
  
  # Clean AHA zip to align with Dartmouth Atlas
  mutate(hosp_zip = as.numeric(sub("-.*", "", MLOCZIP))) %>%  
  
  # Merge with zip-hrr map to assign state-HRR
  left_join(zip_df, by = c("YEAR", "hosp_zip" = "zip")) 


# CHECK: How many zip codes didn't map to an HRR or have missing zip?
print(paste0("CAVEAT: There are ", nrow(aha_clean %>% filter(is.na(hrrnum) | is.na(MLOCZIP))), " hospital-years which are not assigned to HRRs or Zips")) #78

# CLEAN MANUALLY

library(zoo)

# Manually clean 1 missing Zip code
aha_clean[ , hosp_zip := ifelse(ID == '0006880085', 89086, hosp_zip)]
aha_clean[ , MLOCZIP := ifelse(ID == '0006880085', 89086, MLOCZIP)]

# use the city name to get the hrr num
unique_hrrs  <- zip_df %>% select(YEAR, hrrnum, hrrcity, hrrstate) %>% distinct() %>% 
  mutate(city = tolower(hrrcity),
         state = hrrstate,
         hrrnumR = hrrnum) 

aha_clean <- aha_clean %>%  
  mutate(state = MSTATE,
         city = tolower(MLOCCITY)) %>% 
  left_join(unique_hrrs %>% select(YEAR, hrrnumR, state, city), by = c('YEAR', 'city', 'state')) %>% 
  
  # join the revised hrrnum, city and state
  mutate(hrrnum = ifelse(is.na(hrrnum) & !is.na(hrrnumR), hrrnumR, hrrnum)) %>% 
  select(-hrrcity, -hrrstate, -state, -city, -hrrnumR) 

# fill HRR using most recent year from Zoo package
setorder(aha_clean, ID, YEAR)
# Apply na.locf within each group --> Fill backwards first, then forwards
aha_clean <- aha_clean[, hrrnum := {
  temp <- na.locf(hrrnum, fromLast = TRUE, na.rm = FALSE) # Fill backwards
  na.locf(temp, na.rm = FALSE)                            # Fill forwards
}, by = ID] %>% 
  left_join(unique_hrrs %>% select(YEAR, hrrnum, hrrcity, hrrstate), by = c('YEAR', 'hrrnum')) 

print(paste0("There are now ", nrow(aha_clean %>% filter(is.na(hrrnum) | is.na(MLOCZIP))), " hospital-years which are not assigned to HRRs or Zips")) #0

rm(unique_hrrs, temp)

# ------------------------------------------------------------------------------
  
# STEP 4: 
# CREATE OUR OWN STATE HRR's which don't cross state lines, i.e. "modified HRR"
# Notes from Joe: Normal HHRs can span several states, which doesn’t work for us. 
#       We should make our own HHRs that never span multiple states
  
aha_clean <- aha_clean %>% 

  mutate(state_hrr = ifelse(!is.na(hrrnum), paste0(MSTATE, as.character(hrrnum)), NA))  %>% 
  
  rename(year_id = YEAR)

# ==============================================================================

# STEP 5:  Is this a general medical / surgical or surgical hospital?

# SERV == 10: General medical and surgical
# SERV == 13: Surgical

aha_clean <- aha_clean %>% 
  mutate(gen_surg = ifelse(SERV == 10 | SERV == 13, 1, 0))

# ==============================================================================

# SAVE

write_feather(aha_clean, paste0(out_dir, ".archive/aha_intermediate_2000_2019.feather"))
