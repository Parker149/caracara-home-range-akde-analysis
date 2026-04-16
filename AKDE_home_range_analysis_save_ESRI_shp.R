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

# --- Define directory to load workspace ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/work_space/")
load("AKDE_workspace.RData")

# --- Point script to data ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/")
datapath <- "clean_data/"

# --- Look for "clean.csv" ---
myfiles <- list.files(datapath, pattern = "clean.*\\.csv$", full.names = TRUE, ignore.case = TRUE)

# --- Create to loop through data --- 
bird_names <- c()

# --- load all csv and clear blanks ---
for(f in myfiles){
  name <- tools::file_path_sans_ext(basename(f))
  bird_names <- c(bird_names, name)
  df <- read.csv(f,
                 header = TRUE,
                 strip.white = TRUE,
                 na.strings = c("", "NA"))
  assign(name, df)
}

# --- Ensure all datetimes are in POSIXct format ---
for(bird in bird_names){
  df <- get(bird)
  df$datetime <- as.POSIXct(df$datetime)
  assign(bird, df)
}

# --- Assess the variograms of each Caracara --- 
par(mfrow = c(2, 2))
ctmm.t <- list()
svf.t  <- list()
xlimz  <- c(0, 250000)
for (bird in bird_names) {
  df <- get(bird)
  tel <- ctmm::as.telemetry(
    df,
    timestamp = "datetime",
    longitude = "utm_x",
    latitude  = "utm_y"
  )
  ctmm.t[[bird]] <- tel
  vg <- variogram(tel)
  svf.t[[bird]] <- vg
  plot(vg, xlim = xlimz, main = paste0("Bird = ", bird))
}

# --- estimating a suitable movement model for the observed data using the empirical variogram ---
m.best <- list()
for (bird in bird_names) {
  guess <- ctmm.guess(
    data      = ctmm.t[[bird]],
    variogram = svf.t[[bird]],
    interactive = FALSE
  )
  m.best[[bird]] <- ctmm.select(
    data = ctmm.t[[bird]],
    CTMM = guess,       # <-- IMPORTANT!
    verbose = TRUE,
    trace = 2
  )
  cat("\n\n=== Summary for", bird, "===\n")
  print(summary(m.best[[bird]]))
}

# --- Plot variograms alongside the best-fit model ---
graphics.off()
par(mfrow = c(2, 2))  
for (bird in bird_names) {
  m.best[[bird]]$`IID isotropic` <- ctmm.fit(
    ctmm.t[[bird]],
    ctmm(isotropic = TRUE)
  )
  top_model_name <- rownames(summary(m.best[[bird]][1]))
  plot(
    svf.t[[bird]],
    m.best[[bird]][[1]],
    xlim = xlimz,
    main = paste(bird, "-", top_model_name)
  )
}

# --- Create home range for top model for each bird using ctmm ---
akde.top <- list()
for (bird in bird_names) {
  top_model <- m.best[[bird]][[1]]   # First model = best model
  akde.top[[bird]] <- ctmm::akde(
    ctmm.t[[bird]],
    top_model,
    weights = FALSE
  )
}

# --- plot HR for each bird based on top model using ctmm ---
par(mfrow = c(2, 2))   
for (bird in bird_names) {
  plot(ctmm.t[[bird]], akde.top[[bird]])
  title(paste("AKDE - Top Model:", bird))
}

# NOTES: In my case, the top model for 3/4 Caracaras was the OU anisotropic. The
# NOTES: one Caracara that didn't have OU anisotropic as the top model was very 
# NOTES: data poor. As a result I moved forward with using OU anisotropic for all. 

# === Use amt for further home range analysis due to its easy of spatial reference ===

# --- Create empty list to store results for each bird ---
trk_list <- list()

# --- Create an amt movement track for each bird --
for(bird in bird_names){
  df <- get(bird)
  df$datetime <- as.POSIXct(df$datetime)
  trk_list[[bird]] <- amt::make_track(df,
                                      utm_x,
                                      utm_y,
                                      datetime,
                                      crs = 32617)
}

# --- Fits OU model for each track & calcs corresponding AKDE ---
akde_all <- map(trk_list, ~{
  fit <- fit_ctmm(.x, model = "ou")
  hr_akde(.x, fit)
})

# Plots 95% AKDE from isopleths & svaes home range --- 
for (id in names(akde_all)) {
  # create 95% home range
  hr_sf <- hr_isopleths(akde_all[[id]])
  hr_est <- hr_sf[hr_sf$what == "estimate", ]
  assign(paste0("hr_est_", id), hr_est)
  a <- ggplot() +
    theme_bw(base_size = 8) +
    geom_sf(data = hr_est, fill = NA, col = "blue", linewidth = 1) +
    labs(title = id)
  print(a)
}

# --- Area of home range ? ---
cat("Home range area in km^2 for F36312 =",as.numeric(hr_est_clean_F36312$area) / 1e6)
cat("Home range area in km^2 for F44868 =",as.numeric(hr_est_clean_F44868$area) / 1e6)
cat("Home range area in km^2 for M36313 =",as.numeric(hr_est_clean_M36313$area) / 1e6)
cat("Home range area in km^2 for M68230 =",as.numeric(hr_est_clean_M68230$area) / 1e6)

# --- save home ranges as ESRI shapefiles --- 
sf::st_write(hr_est_clean_F36312,
             "/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/shp_files/F36312.shp",
             driver = "ESRI Shapefile")

sf::st_write(hr_est_clean_F44868,
             "/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/shp_files/F44868.shp",
             driver = "ESRI Shapefile")

sf::st_write(hr_est_clean_M36313,
             "/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/shp_files/M36313.shp",
             driver = "ESRI Shapefile")

sf::st_write(hr_est_clean_M68230,
             "/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/shp_files/M68230.shp",
             driver = "ESRI Shapefile")

# --- Define directory to save workspace ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/work_space/")
save.image("AKDE_workspace.RData")
























