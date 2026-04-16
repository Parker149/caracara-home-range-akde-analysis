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

# --- Define directory to load workspace ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/work_space/")
load("AKDE_NLCD_workspace.RData")

# --- Point script to data ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/")
datapath <- "shp_files/"

# --- Find shp file names ---
shp_files <- list.files(datapath, pattern = "\\.shp$", full.names = TRUE)

# --- Load in all shp files in data path into a list ---
hr_list <- lapply(shp_files, sf::st_read)

# --- Get all shp file names for looping ---
names(hr_list) <- tools::file_path_sans_ext(basename(shp_files))

# --- I had an AOI shp file I wanted to remove ---
hr_list <- hr_list[names(hr_list) != "AOI"]

# --- Point path to where NLCD is stored and load in raster ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/landcover_rasters/")
NLCD <- rast("clipped_NLCD_2011.tif")

# --- Define NLCD values, colors, and names ---
nlcd_vals <- c(11,21,22,23,24,31,42,52,71,81,82,90,95)
nlcd_cols <- c(
  "#476BA0", "#D2B48C", "#ffcccc", "#ff6666", "#ff0000",
  "#C7C7C7",
  "#006D2C",
  "#A1D99B",
  "#FEE08B",
  "#DEEBF7",
  "#FFFFB2",
  "#238B45",
  "#74C476"
)
nlcd_names <- c(
  "Water","Dev Open","Dev Low","Dev Med","Dev High",
  "Barren","Evergreen Forest",
  "Shrub/Scrub","Grassland","Pasture","Crops",
  "Woody Wetlands","Emergent Wetlands"
)

# --- Plot each home range shp with the NLCD raster ---
for (i in seq_along(hr_list)) {
  hr <- hr_list[[i]]
  plot(NLCD,
       col = nlcd_cols,
       type = "classes",
       legend = FALSE,
       main = names(hr_list)[i])
  plot(hr["geometry"], add = TRUE, border = "red", lwd = 2)
  legend(x = 520000, y = 3119000,,
         legend = nlcd_names,
         fill = nlcd_cols,
         bty = "o",
         cex = 0.7,
         title = "NLCD 2011",
         bg = "white")
  
}

# --- Pie charts of the NLCD type in each home range --- 
for (i in seq_along(hr_list)) {
  hr <- hr_list[[i]]
  vals <- raster::extract(NLCD, hr)
  results <- prop.table(table(unlist(vals))) * 100
  results_aligned <- results[as.character(nlcd_vals)]
  results_aligned[is.na(results_aligned)] <- 0
  # Pie chart
  pie(results_aligned,
      labels = NA,
      col = nlcd_cols,
      main = names(hr_list)[i])
  legend("topright",
         legend = nlcd_names,
         fill = nlcd_cols,
         cex = 0.6,
         bty = "n")
  # Summary table
  cat(names(hr_list)[i])
  print(data.frame(
    Landcover = nlcd_names,
    Percent = round(as.numeric(results_aligned), 2)
  ))
}

# --- Define directory to save workspace ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/work_space/")
save.image("AKDE_NLCD_workspace.RData")



