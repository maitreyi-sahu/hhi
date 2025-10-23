library(tigris)
library(sf)
library(spdep)

hhi_var <- "drive_time_HOSPBD_30_minutes_gen_surg"
  
county_wide <- read_feather(paste0(out_dir, "compiled_hhi_county_wide.feather")) 

county_wide_fill <- county_wide[ , c("cnty", "year_id", hhi_var)] %>% filter(cnty != "   NA") %>% setDT() %>% 
  mutate(fips = cnty, GEOID = cnty)

county_sf <- counties(cb = TRUE, class = "sf")  %>% 
 # filter(GEOID %in% unique(county_wide_fill$cnty)) %>% 
  left_join(county_wide_fill, by = "GEOID")

plot(st_geometry(county_sf %>%  filter(year_id == 2022)))

# plot if NA
plot_usmap(data = county_wide_fill %>% filter(year_id == 2019), values = hhi_var, regions = "counties", color = NA) 


# Using spdep package, get neighboring counties
county_neighbors <- poly2nb(county_sf %>% filter(year_id == 2019), queen=TRUE) # if true, a single shared boundary 
average_neighbors <- sapply(county_neighbors, function(n){mean(county_wide_fill$drive_time_HOSPBD_30_minutes_gen_surg[n], na.rm = T)})
