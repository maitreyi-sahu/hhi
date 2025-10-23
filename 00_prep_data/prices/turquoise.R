# MSahu
# March 20
# Explore Turqouise health data sample 

turq <- fread("/mnt/share/resource_tracking/us_value/data/hospital_hhi/raw/turquoise/turquoisehealth_researchdataset.csv") %>% 
  mutate(service_lower = tolower(service),
         colonoscopy = grepl("colonoscopy", service_lower))
aha_clean <- fread(paste0(dir, "/mnt/share/resource_tracking/us_value/data/hospital_hhi/processed/aha_clean_2000_2019.csv"))


colonoscopy <- turq %>% filter(colonoscopy == T)
arthroscopy <- turq %>%  filter(code == 29877)
