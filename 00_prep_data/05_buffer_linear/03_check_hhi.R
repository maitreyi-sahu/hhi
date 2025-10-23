# Maitreyi Sahu
# April 7, 2024

# COME back to this -
# Here I check the linear buffer results against each other

# 1. Check 2019 Harry's values versus mine for UTM zone version
# 2. Check the 3 versions of linear buffer (haversine, utm, crs 3857)

# ==============================================================================

rm(list=ls())
pacman::p_load(dplyr, data.table, stringr, arrow, ggplot2)

dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/"

# ------------------------------------------------------------------------------

# LOAD FILES

# 2000-2019 from MS
stamp <- "2024-04-03" # UPDATE IF RERUNNING THIS
linear_hhi <- read_feather(paste0(dir, "buffer/linear/linear_hhi_compiled_", stamp ,".feather")) %>% setDT()

# Import 2019-2022 from HK
harry_dir <- paste0(dir, "buffer/linear/from_harry/")

read_harry <- function(file) {
  hosp_type = ifelse(grepl("Gen_Surg", file), "gen_surg", "all")
  dt <- fread(paste0(harry_dir, file))
  dt <- dt[, year_id := 2000 + year_id]
  dt <- dt[, hosp_type := hosp_type]
  setnames(dt, "aha_systems", "aha_systems_n")
  return(dt)
}
harry_files <- list.files(harry_dir, pattern = ".csv")
linear_hhi_harry <- rbindlist(lapply(harry_files, read_harry))

# Check that 2019 data is the same
check2019 <- linear_hhi[year_id == 2019 & hosp_type == "gen_surg", c("year_id", "ID", "hhi_pt_days", "radius")] %>% left_join(linear_hhi_harry[year_id == 2019 & hosp_type == "gen_surg", c("year_id", "ID", "hhi_pt_days", "radius")], by = c("year_id", "ID", "radius"))
ggplot(check2019, aes(x =hhi_pt_days.x, y = hhi_pt_days.y)) + geom_point() + geom_abline(color = "blue") + xlab("MS 2019 linear HHI") + ylab("HK 2019 linear HHI")
check2019$diff = check2019$hhi_pt_days.y - check2019$hhi_pt_days.x
check2019 <- check2019 %>% arrange(desc(diff))
# there are ~10 hospitals with differences in 2019 for 30m radius - use MS values for now

combined_hhi <- individual_hhi %>% rbind(harry_linear %>% filter(year_id!=2019))

# ------------------------------------------------------------------------------

# Check - Haversine vs. UTM vs. CRS 3857