# --- Clear all --- 
rm( list = ls() ) 
graphics.off() 
cat("\014") 

# --- Load packages --- 
library(lubridate)
library(dplyr)
library(sf)
library( amt )
library(purrr)
library(tidyr)
library(ggplot2)
library(sf)
library(ctmm ) 
library(raster)
library(terra)
library(ggspatial)

# --- Seasonal home range --- 
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/") 
datapath <- "shp_files/"
shp_files <- list.files(datapath, pattern = "\\.shp$", full.names = TRUE) 
hr_list <- lapply(shp_files, sf::st_read) 
names(hr_list) <- tools::file_path_sans_ext(basename(shp_files)) 
hr_list <- hr_list[names(hr_list) != "AOI"] 
seasonal_sf <- lapply(names(hr_list), function(n) {
  hr_list[[n]] %>%
    mutate(
      bird_id = n,
      period = "Seasonal"
    )
}) %>% bind_rows()

# --- Pre fledge data --- 
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/shp_files/") 
datapath <- "pre_fledge/" 
shp_files <- list.files(datapath, pattern = "\\.shp$", full.names = TRUE) 
pre_hr_list <- lapply(shp_files, sf::st_read) 
names(pre_hr_list) <- tools::file_path_sans_ext(basename(shp_files)) 
names(pre_hr_list) <- gsub("^pre_", "", names(pre_hr_list))
pre_sf <- lapply(names(pre_hr_list), function(n) {
  pre_hr_list[[n]] %>%
    mutate(
      bird_id = n,
      period = "Pre-Fledge"
    )
}) %>% bind_rows()

# --- Post fledge data --- 
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/shp_files/") 
datapath <- "post_fledge/" 
shp_files <- list.files(datapath, pattern = "\\.shp$", full.names = TRUE) 
post_hr_list <- lapply(shp_files, sf::st_read) 
names(post_hr_list) <- tools::file_path_sans_ext(basename(shp_files))
names(post_hr_list) <- gsub("^post_", "", names(post_hr_list))
post_sf <- lapply(names(post_hr_list), function(n) {
  post_hr_list[[n]] %>%
    mutate(
      bird_id = n,
      period = "Post-Fledge"
    )
}) %>% bind_rows()

# --- Combine all ---
all_sf <- bind_rows(seasonal_sf, pre_sf, post_sf)

# --- Define color scheme
all_sf <- all_sf %>%
  mutate(color_group = case_when(
    period == "Seasonal" ~ bird_id,
    period == "Pre-Fledge" ~ "Pre-Fledge",
    period == "Post-Fledge" ~ "Post-Fledge"
  ))
color_key_full <- c(
  "F36312" = "red",
  "F44868" = "green",
  "M36313" = "blue",
  "M68230" = "magenta",
  "Pre-Fledge" = "darkgray",
  "Post-Fledge" = "darkorange4"
)

# Plot
graphics.off() 
bbox <- st_bbox(all_sf)
p <- ggplot(all_sf) +
  geom_sf(aes(color = color_group), fill = NA, linewidth = 0.8) +
  scale_color_manual(values = color_key_full) +
  facet_wrap(~ bird_id, ncol = 2) +
  theme_void() +
  labs(color = NULL) +
  theme(strip.text = element_text(size = 14)) +
  
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    unit_category = "metric"
  ) 
p

# Save 
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/figures/")
ggsave("grid_home_range_through_time.png",
       plot = p,
       width = 5,
       height = 5,
       units = "in",
       dpi = 300,
       bg = "white")
