## ------------------------
## Launch array to run policy regressions including draw-level uncertainty from value scores
## Author: Sawyer Crosby / MSahu
## Updated: April 7, 2024
## ------------------------

rm(list = ls())
pacman::p_load(data.table, tidyverse, dplyr)
library(DataCombine, lib.loc="/mnt/share/homes/hkl1/package_lib")

## set timestamp
stamp <- '7apr2024_hospital_hhi'
reg_type <- 'timeFE'  # one of `timeFE` `locFE` `bothFE` `noFE`
note <- "2000_2020" 
var <- "" # if non-empty, will read in the other data file

## find current file location
here <- dirname(if(interactive()) rstudioapi::getSourceEditorContext()$path else rprojroot::thisfile())
repo_path <- strsplit(here, split = "us_value")[[1]][1]

## get job submission functions
source(paste0(repo_path, "/us_value/5_misc/cluster_utils.R"))
if(! reg_type %in% c("timeFE", "locFE", "bothFE", "noFE")){
  stop("reg_type is not valid")
}


## make output directories
dir <- paste0("/ihme/resource_tracking/us_value/data/policy/simulation/simbetas_", stamp, "_", reg_type, "/")
if(!dir.exists(dir)){
  dir.create(dir)
  dir.create(paste0(dir, "logs"))
  dir.create(paste0(dir, "logs/errors"))
  dir.create(paste0(dir, "logs/output"))
  dir.create(paste0(dir, "output"))
  dir.create(paste0(dir, "plots"))
}

## remove old files
system(paste0("find ", dir, " -type f -delete"))

# Read hhi
hosp_hhi <- read_feather("/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/compiled_hhi/compiled_hhi_state_wide.feather") %>% 
  select(-location_id, -postal_code)
fwrite(hosp_hhi, paste0(dir, "hosp_hhi_vars.csv"))

hhi_yrs_by_cov <- melt(hosp_hhi, id.vars = c("year_id", "location_name")) %>% setDT()
hhi_yrs_by_cov[, has_data := ifelse(is.na(value), 0, 1)]
hhi_yrs_by_cov <- unique(hhi_yrs_by_cov[,.(year_id, variable, has_data)])
write.csv(hhi_yrs_by_cov, paste0(dir, "hhi_vars_num_years.csv"))


## make iterator (the list we "loop" through in the array)
ITERATOR <- data.table(
  cov = names(hosp_hhi)[!names(hosp_hhi) %in% c("year_id", "location_id", "location_name")]
)
fwrite(ITERATOR, paste0(dir, "iterator.csv"))

## launch jobs
array_jid <- SUBMIT_ARRAY_JOB(
  script = paste0(here, "/02_hhi_sim_script.R"), # string filepath
  n_jobs = ITERATOR[,.N],
  memory = "2G",
  queue = "long.q",
  threads = "2",
  time = "03:00:00",
  error_dir = paste0(dir, "logs/errors/"),
  output_dir = paste0(dir, "logs/output/"),
  name = "polsim_mod", 
  args = c(stamp, reg_type, note, var)
)

## launch compilation script
compile_jid <- SUBMIT_JOB(
  script = paste0(here, "/03_hhi_sim_compile.R"), # string filepath
  memory = "2G",
  queue = "long.q",
  threads = "1",
  time = "01:00:00",
  error_dir = paste0(dir, "logs/errors/"),
  output_dir = paste0(dir, "logs/output/"),
  name = "polsim_compile",
  hold = array_jid, 
  args = c(stamp, reg_type, note, var)
)
