# MSahu
# March 30, 2023

# Create some maps of hospitals/HHI using the usmap package

# -----------------------------------------------------------------------------------------

pacman::p_load(usmap, usmapdata, wesanderson)
source('init.R')
source('plot_functions.R')

# Read data

aha_coords <- fread(paste0(data_dir, "processed/aha_clean_2000_2019.csv")) %>% setDT()

hhi_state_wide <- read_feather(paste0(data_dir, "processed/compiled_hhi/compiled_hhi_state_wide.feather")) %>% setDT() %>% 
  mutate(state = postal_code)
hhi_hrr_wide <- read_feather(paste0(data_dir, "processed/compiled_hhi/compiled_hhi_hrr_wide.feather")) %>% setDT()
hhi_county_wide <- read_feather(paste0(data_dir, "processed/compiled_hhi/compiled_hhi_county_wide.feather")) %>% setDT() %>% filter(cnty!="   NA")
hhi_individ_wide <- read_feather(paste0(data_dir, "processed/compiled_hhi/compiled_hhi_individ_wide.feather")) %>% select(-hrrnum) %>% setDT()

best_hhi <- "drive_time_HOSPBD_30_minutes_gen_surg"

# -------------------------------------------------------------------------------------------

# Transform AHA data to coordinates

aha_coords$lon = as.numeric(aha_coords$LONG)
aha_coords$lat = as.numeric(aha_coords$LAT)

aha_plot <- aha_coords %>%  filter(!is.na(lon) & !is.na(lat) & !lon<(1e8*-1) & !lat>50) %>% 
  mutate(`Part of system` = ifelse(in_system == T, "Yes", "No")) %>% 
  mutate(`Hospital beds` = HOSPBD)

hospitals <- usmap_transform(aha_plot) 

# ----------------------------------------------------------------------------------------------

# Fig. 1 : State HHI over time

hhi_upperlim <- 10000

map_colors <- wes_palette("Zissou1", 100, type = "continuous") 

#--

pdf(paste0(plot_dir,"Fig1_State_HHI.pdf"), width = 12, height = 8, onefile = TRUE)

for(i in 2000:2019){   # TODO get coordinates from Harry? then can add 2020-22

hospital_year <- hospitals %>%  filter(year_id == i, 
                                       y <1e6 & x >-2e6) # TODO double check what happened to these 2 hospitals with weird vals )  
hhi_state_year <- hhi_state_wide %>% filter(year_id == i)
  
p <- plot_usmap(data = hhi_state_year, values = best_hhi, regions = "state") +
  scale_fill_gradientn(colours = map_colors, name = "State-level\n Hospital HHI", label = scales::comma, 
                       limits = c(0, hhi_upperlim) ) +
  geom_point(data = hospital_year, aes(x = x, y = y), color = "#273046", size = 1, alpha = .35) +
  labs(title = paste0("\nState-level hospital HHI and hospital locations, ", i)) +
  theme(plot.title = element_text(size=20)) +
  theme(legend.position = "right",
        legend.justification = "top",
        legend.key.height=unit(1.35,"cm"),
        legend.title = element_text(size=14),
        legend.text=element_text(size=12),
        plot.caption = element_text(hjust = 0)) 

  print(p)

}

dev.off()

## Again without the dots

pdf(paste0(plot_dir,"Fig1_State_HHI_no_dots.pdf"), width = 12, height = 8, onefile = TRUE)

for(i in 2000:2022){
  
  hospital_year <- hospitals %>%  filter(year_id == i, 
                                         y <1e6 & x >-2e6) # later, should double check what happened to these 2 hospitals with weird vals )  
  hhi_state_year <- hhi_state_wide %>% filter(year_id == i)
  
  p <- plot_usmap(data = hhi_state_year, values = best_hhi, regions = "state") +
    scale_fill_gradientn(colours = map_colors, name = "State-level\n Hospital HHI", label = scales::comma, 
                         limits = c(0,hhi_upperlim) ) +
  #  geom_point(data = hospital_year, aes(x = x, y = y), color = "#273046", size = 1, alpha = .35) +
    labs(title = paste0("\nState-level hospital HHI (using the 30-minute drive-time), ", i)) +
    theme(plot.title = element_text(size=20)) +
    theme(legend.position = "right",
          legend.justification = "top",
          legend.key.height=unit(1.35,"cm"),
          legend.title = element_text(size=14),
          legend.text=element_text(size=12),
          plot.caption = element_text(hjust = 0)) 
  
  print(p)
  
}

dev.off()

#-------------------------------------------------------------------------------

# Fig 1b - categorical version

Zissou1 = c("#6FB2C1", "#DCCB4E", "#E79805", "#C52E19")

hhi_state_wide <- hhi_state_wide %>% 
  mutate(hhi_cat = cut(get(best_hhi), breaks = c(0, 1500, 2500, 5000, 10000), 
                       labels = c("0-1,500 (unconcentrated)", "1,500-2,500 (moderately concentrated)", "2,500-5,000 (highly concentrated)", "5,000-10,000 (very highly concentrated)")),
         hhi_colors = case_when(hhi_cat == "0-1,500 (unconcentrated)" ~ Zissou1[1],
                                hhi_cat == "1,500-2,500 (moderately concentrated)" ~ Zissou1[2],
                                hhi_cat == "2,500-5,000 (highly concentrated)" ~ Zissou1[3],
                                hhi_cat == "5,000-10,000 (very highly concentrated)" ~ Zissou1[4]))

unique_colors <- unique(hhi_state_wide[, c("hhi_cat", "hhi_colors")])
hhi_colors_vector <- setNames(unique_colors$hhi_colors, unique_colors$hhi_cat)


pdf(paste0(plot_dir,"Fig1_State_HHI_categorical.pdf"), width = 12, height = 8, onefile = TRUE)

for(i in 2000:2019){   # TODO get coordinates from Harry? then can add 2020-22
  
  hospital_year <- hospitals %>%  filter(year_id == i, 
                                         y <1e6 & x >-2e6) # TODO double check what happened to these 2 hospitals with weird vals )  
  hhi_state_year <- hhi_state_wide %>% filter(year_id == i)
  
  p <- plot_usmap(data = hhi_state_year, values = "hhi_cat", regions = "state") +
    scale_fill_manual(values = hhi_colors_vector, name = "State-level Hospital HHI\n(30-min drive-time)", drop = F) +
    geom_point(data = hospital_year, aes(x = x, y = y), color = "#273046", size = 1, alpha = .35) +
    labs(title = paste0("\nState-level hospital HHI and hospital locations, ", i)) +
    theme(plot.title = element_text(size=20)) +
    theme(legend.position = "right",
          legend.justification = "top",
          legend.key.height=unit(1.35,"cm"),
          legend.title = element_text(size=14),
          legend.text=element_text(size=12),
          plot.caption = element_text(hjust = 0)) 
  
  print(p)
}
dev.off()

## Again without the dots

pdf(paste0(plot_dir,"Fig1_State_HHI_no_dots_categorical.pdf"), width = 12, height = 8, onefile = TRUE)

for(i in 2000:2022){
  
  hospital_year <- hospitals %>%  filter(year_id == i, 
                                         y <1e6 & x >-2e6) # later, should double check what happened to these 2 hospitals with weird vals )  
  hhi_state_year <- hhi_state_wide %>% filter(year_id == i)
  
  p <- plot_usmap(data = hhi_state_year, values = "hhi_cat", regions = "state") +
    scale_fill_manual(values = hhi_colors_vector, name = "State-level Hospital HHI\n(30-min drive-time)", drop = F) +
    labs(title = paste0("\nState-level hospital HHI (using the 30-minute drive-time), ", i)) +
    theme(plot.title = element_text(size=20)) +
    theme(legend.position = "right",
          legend.justification = "top",
          legend.key.height=unit(1.35,"cm"),
          legend.title = element_text(size=14),
          legend.text=element_text(size=12),
          plot.caption = element_text(hjust = 0)) 
  
  print(p)
}

dev.off()

# -------------------------------------------------------------------------------------------

# FIG. 2 : Hospitals and their size and whether part of a system, over time

hospBed_lower <- min(hospitals$`Hospital beds`)
hospBed_upper <- ceiling(max(hospitals$`Hospital beds`)/1000)*1000

pdf(paste0(plot_dir,"Fig2_Hospitals.pdf"), width = 12, height = 8, onefile = TRUE)

for(i in 2000:2019){
  
  hospital_year <- hospitals %>%  filter(YEAR == i, 
                                         y <1e6 & x >-2e6) # later, should double check what happened to these 2 hospitals with weird vals 
p <- plot_usmap(regions = "state") +
  geom_point(data = hospital_year, aes(x = x, y = y, size = `Hospital beds`, color = `Part of system`), alpha = .3) +
  scale_size_continuous(range = c(1, 8), 
                       limits = c(hospBed_lower, hospBed_upper), 
                        label = scales::comma) +
  scale_color_manual(values = c(`No` = "#46ACC8", `Yes` = "#B40F20"))  +
  labs(title = paste0("\nHospitals by size and whether they are part of a system, ", i)) +
  theme(plot.title = element_text(size=20)) +
  theme(legend.position = "right",
        legend.justification = "top",
        legend.key.height=unit(1.35,"cm"),
        legend.title = element_text(size=14),
        legend.text=element_text(size=12),
        plot.caption = element_text(hjust = 0)) 

print(p)
}

dev.off()
  
# -------------------------------------------------------------------------------

# HRR MAPS !!!


# -----------------------------------------------------------------------------------------

# 1. close all your rstudio sessions 
# 2. Run /ihme/singularity-images/rstudio/shells/r_env_cleaning.sh
# 3. Get a new session.

rm(list=ls())
pacman::p_load(tidyverse, sf, sp, rgdal, usmap, usmapdata, ggplot2, gridExtra)
library(wesanderson, lib.loc = "~/rpackages")

dir <- "/mnt/share/resource_tracking/us_value/data/hospital_hhi/"
shp_dir <- paste0(dir, "raw/dartmouth_atlas/hrr_boundaries/hrr-shapefile/")
plot_dir <- paste0(dir, "processed/plots/")

# -----------------------------------------------------------------------------------------

# read the HRR shp files from Dartmouth Atlas and map
hrr_shp <- read_sf(dsn = shp_dir, layer = "Hrr98Bdry_AK_HI_unmodified")

# read the hrr-level hhi's
hrr_hhi <- fread(paste0(dir, "processed/hrr_hhi_2000_2019.csv")) 

# join
hrr_joined <- hrr_shp %>% 
  full_join(hrr_hhi, by = "hrrnum")

# add the coordinates
hrr_joined <- st_transform(hrr_joined, crs = 4326) # used for leaflet package
st_crs(hrr_joined)

# state boundaries
usa <- st_as_sf(maps::map("state", fill=F, plot =FALSE))

# -----------------------------------------------------------------------------------------

hrr_joined_19 <- hrr_joined %>%  filter(YEAR == 2019)

map_colors <- wes_palette("Zissou1", 100, type = "continuous") 

p <- ggplot() +
  
  geom_sf(data = hrr_joined_19, aes(fill = hhi), color = "black") +
  geom_sf(data = usa, alpha = 0) +
  
  scale_fill_gradientn(colours = map_colors,
                       #breaks = breaks,
                       name = "Herfindahl-Hirschman Index"
                       #,label = labs
  ) +
  theme_bw()
plot(p)

## Again without the dots

pdf(paste0(plot_dir,"Fig_HRR_HHI.pdf"), width = 12, height = 8, onefile = TRUE)

for(i in 2000:2019){
  
  hospital_year <- hospitals %>%  filter(YEAR == i) #   
  hhi__year <- HHI_state_new %>% filter(YEAR == i)
  
  p <- plot_usmap(data = hhi_state_year, values = "hhi_hospitals", regions = "state") +
    scale_fill_gradientn(colours = map_colors, name = "State-level\n Hospital HHI", label = scales::comma, 
                         limits = c(0,hhi_upperlim) ) +
    #  geom_point(data = hospital_year, aes(x = x, y = y), color = "#273046", size = 1, alpha = .35) +
    labs(title = paste0("\nState-level hospital HHI and hospital locations, ", i)) +
    theme(plot.title = element_text(size=20)) +
    theme(legend.position = "right",
          legend.justification = "top",
          legend.key.height=unit(1.35,"cm"),
          legend.title = element_text(size=14),
          legend.text=element_text(size=12),
          plot.caption = element_text(hjust = 0)) 
  
  print(p)
  
}

dev.off()