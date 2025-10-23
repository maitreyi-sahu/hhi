
library(tidyverse)
library(geosphere)


#aha_data <- paste0(in_dir, "2022_11_University of Washington.xlsx")
aha_data <- "Desktop/Hospital Data/AHA_ANNUAL_SURVEY_3YEARS.csv"
aha <- read_csv(aha_data)

# Load state abbreviations
loc_ids <- fread(paste0("~/Desktop/Hospital Data/HHI Project/laura_postal_codes.csv"))
postal_codes <- loc_ids[ , "postal_code"]

# Clean up / rename
origin <- "1900-01-01" # for dates

#aha_temp <- table[,-c(1:40)] %>% 
aha_temp <- aha %>%   
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

# You may want to process the code one year at a time, otherwise the join in the next step may overwhelm your computer
year=21
aha_clean <- filter(aha_temp,!is.na(LONG) & !is.na(LAT) & LONG!="." & LAT !=".",YEAR==year) %>% 
  select(YEAR, ID, LAT, LONG) %>% mutate(distance=NA)

# A full join by the distance column (which is all NAs), creates every combination of hospital in a given year
aha_join <- full_join(aha_clean,aha_clean,by=c("YEAR","distance")) 

# Identify the column numbers corresponding to the Longitude and Latitude coordinates (in that order)
# distHaversine will calculate the meters between the points, which can be converted to miles
aha_join$distance <- distHaversine(aha_join[4:3],aha_join[8:7])/1609.344

# At this point you can filter the data down to hospitals that are within a certain distance
# of each other (say 100 miles) and use the Lat/Long coordinates to calculate drive times,
# which can then be further filtered 
# Note: I should have formally coded a loop for this, but decided to just to do manually for each year
aha_19 <- filter(aha_join, distance<=100, distance!=0) %>% select(LAT.x, LONG.x, LAT.y, LONG.y) %>% unique()
aha_20 <- filter(aha_join, distance<=100, distance!=0) %>% select(LAT.x, LONG.x, LAT.y, LONG.y) %>% unique()
aha_21 <- filter(aha_join, distance<=100, distance!=0) %>% select(LAT.x, LONG.x, LAT.y, LONG.y) %>% unique()

aha_100_miles <- rbind(aha_19, aha_20, aha_21) %>% unique()

write_csv(aha_100_miles, "~/Desktop/Hospital_Coordinates_Within_100_Miles.csv")


