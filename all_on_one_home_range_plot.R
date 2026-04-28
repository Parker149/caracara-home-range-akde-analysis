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

# ==== Plot seasonal home ranges of all birds on one plot ====
# Create color key
color_key <- c(
  "F36312" = "red",
  "F44868" = "green",
  "M36313" = "blue",
  "M68230" = "magenta"
)
# Add ID column
hr_list <- lapply(names(hr_list), function(n) {
  hr_list[[n]] %>% mutate(id = n)
})
# Combine
hr_all <- do.call(rbind, hr_list)
# Plot
ggplot(hr_all) +
  geom_sf(aes(color = id), fill = NA, linewidth = 0.8) +
  scale_color_manual(values = color_key) +
  labs(color = NULL) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    unit_category = "metric"
  ) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
# Save 
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/figures/")
ggsave("AllOnOne_home_range.png",
       width = 5,
       height = 5,
       units = "in",
       dpi = 300)












