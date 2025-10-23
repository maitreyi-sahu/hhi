#Msahu
#Dec 12, 2023

# Table of service type

# ==============================================================================

rm(list=ls())

# Packages
pacman::p_load(tidyr, dplyr, data.table) 

# Directories 
dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"

# Cleaned AHA data
aha_clean <- fread(paste0(dir, "processed/aha_clean_2000_2019.csv"))
service_codes <- fread(paste0(dir, "raw/aha/AHA sample data and codebook/aha_serv_codes.csv"))

#===============================================================================

table(aha_clean$SERV)

aha_clean19 <- aha_clean %>% filter(year_id == 2019) 

colB <- table(aha_clean19$SERV)
colC <- round(100*prop.table(table(aha_clean19$SERV)), 1)
colA <- names(colB)

s1table <- cbind(colA, colB, colC) %>%  as.data.table %>% arrange(desc(colC))  %>%  
  mutate(Code = as.integer(colA)) %>% 
  left_join(service_codes, by = "Code") %>% 
  select(Description, Code, colB, colC)
write.csv(s1table, paste0(dir, "plots/appendix_s1table.csv"), row.names = F)


# Hanson paper uses 2008 data and includes 4593 hospitals... check

3152 + 1441 # 3152 included and 1441 excluded 

aha_clean08 <- aha_clean %>% filter(year_id == 2008) 

table(aha_clean08$SERV)

# general medical and surgical is 4822 hospitals