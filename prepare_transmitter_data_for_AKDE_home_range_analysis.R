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

# --- Load work space ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/work_space/")
load("data_prep_workspace.RData")

# --- Point script to data ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/")
datapath <- "data/"

# --- Load in data ---
myfiles <- list.files(datapath, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)

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

# --- Create datetime variable [they all have different date columns !! required manual process] --- 
datetime_str <- paste(F36312$Date,F36312$Time) # = Time
datetime <- mdy_hm(datetime_str) # = hm
F36312$datetime <- datetime

datetime_str <- paste(F44868$Date,F44868$Time) # = Time 
datetime <- mdy_hm(datetime_str) # = hm
F44868$datetime <- datetime

datetime_str <- paste(M36313$Date,M36313$dstEST) # = dstEST
datetime <- mdy_hm(datetime_str) # = hm
M36313$datetime <- datetime
M36313 <- M36313[-1199, ] # row 1199 failed to create datetime

datetime_str <- paste(M68230$Date,M68230$dlsEST) # = dlsEST
datetime <- mdy_hms(datetime_str) # = hms
M68230$datetime <- datetime
M68230 <- M68230[-181, ] # row 181 failed to create datetime

# --- The 9PM time stamp of every Caracara is out of order, easiest solution was to just remove the 9PM rows ---
for(bird in bird_names){
  df <- get(bird)
  time_of_day <- format(df$datetime, "%H:%M:%S")
  df <- df[time_of_day != "21:00:00", ]
  assign(bird, df)
}

# --- Trapping Dates --- 
# F36312 --> 04/19/11
# M36313 --> 04/15/11

# F44868 --> 06/07/11
# M68230 --> 05/25/11

# --- Remove first day of data so Caracaras can rehabituate --- 
for(bird in bird_names){
  df <- get(bird)
  cutoff_time <- df$datetime[1] + as.difftime(1, units = "days")
  df <- df[df$datetime >= cutoff_time, ]
  assign(bird, df)
}

# --- Loop through all Caracaras and plot hists of time gaps
par(mfrow = c(2,2))
for(bird in bird_names){
  df <- get(bird)
  time_gaps <- c(NA, as.numeric(diff(df$datetime), units="mins"))
  hist(time_gaps,
       main = paste("Time gaps (mins) for", bird),
       xlab = "Time gap (minutes)",
       col = "lightblue",
       breaks = 50)
  grid()
}

# --- What is the median time gap for each bird (all 2 hours) to confirm sampling rate ---
for(bird in bird_names){
  df <- get(bird)
  time_gaps <- c(NA, as.numeric(diff(df$datetime), units="mins"))
  med_gap <- median(time_gaps, na.rm = TRUE)
  cat("Median time gap for", bird, "=", med_gap, "minutes\n")
}

# --- Remove any rows with NAs in lat or long ---
for(bird in bird_names){ # 151 12 1347 850
  df <- get(bird)
  df <- df[!is.na(df$lat) & !is.na(df$long), ]
  assign(bird, df)
}

# --- transform in UTM [convert from what data was recorded in WGS84 to UTM North Zone 17] ---
for(bird in bird_names){
  df <- get(bird)
  pts <- st_as_sf(df,coords = c("long","lat"),crs=4326) # WGS84
  pts_utm <- st_transform(pts, 32617)
  utm_coords <- st_coordinates(pts_utm)
  df$utm_x <- utm_coords[,1]
  df$utm_y <- utm_coords[,2]
  assign(bird, df)
}

# --- calc step length from UTM points ---  
for(bird in bird_names){
  df <- get(bird)
  dist_x <- diff(df$utm_x)
  dist_y <- diff(df$utm_y)
  dist <- sqrt((dist_x^2)+(dist_y^2))
  df$dist_meters <- c(NA, dist)
  assign(bird, df)
}

# --- hist of step length ---
for(bird in bird_names){
  df <- get(bird)
  step_length <- df$dist_meters
  hist(step_length,
       main = paste("Step Length (meters)", bird),
       col = "red",
       breaks = 25)
  grid()
}

# NOTES: I don't think its unreasonable for a bird to fly multiple kms in 2 hours. 
# NOTES: In addition the step length hists seem to be pretty progressive with none
# NOTES: having clear outliers. As a result I won't do any filtering based on step
# NOTES: length. 

# --- only M68230 appears to have 1 positional outlier which we will remove ---
M68230 <- M68230[M68230$utm_y <= 3121000, ]

# --- plot x and y of each bird ---
for(bird in bird_names){
  df <- get(bird)
  plot(df$utm_x,df$utm_y,main = paste("Locations of", bird))
  grid()
}

# --- Additional filter for any na in datetime columns ---
F44868 <- F44868[!is.na(F44868$datetime), ]
M68230 <- M68230[!is.na(M68230$datetime), ]
M36313 <- M36313[!is.na(M36313$datetime), ]
F36312 <- F36312[!is.na(F36312$datetime), ]

# --- plot utm_y versus time ---
for(bird in bird_names){
  df <- get(bird)
  plot(df$datetime,df$utm_y,main = paste("Position over time", bird))
  grid()
}

# NOTES: AKDEs can handle large temporal data gaps so no need to filter those out.

# --- Resample to 120m intervales ---
interval_hours <- 2
tolerance_mins <- 10  
for (bird in bird_names) {
  df <- get(bird)
  start_time <- min(df$datetime)
  end_time   <- max(df$datetime)
  time_seq <- seq(from = start_time, to = end_time,
                  by = paste(interval_hours, "hours"))
  df_resampled <- lapply(time_seq, function(t0) {
    window <- df %>%
      filter(abs(difftime(datetime, t0, units = "mins")) <= tolerance_mins)
    if (nrow(window) == 0) return(NULL)
    window[which.min(abs(difftime(window$datetime, t0, units = "secs"))), ]
  }) %>% bind_rows()
  assign(bird, df_resampled)
}

# --- clean empty rows for x, y, and datetime ---
for(bird in bird_names){
  df <- get(bird)
  df <- df[!is.na(df$utm_x) & !is.na(df$utm_y) & !is.na(df$datetime), ]
  assign(bird, df)
}

# --- Create new data frames to be saved as "clean" .csvs ---
clean_F36312 <- F36312
clean_F44868 <- F44868
clean_M36313 <- M36313
clean_M68230 <- M68230

# --- Define directory to save cleaned Caracara data --- 
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/clean_data/")
write.csv(clean_F36312,"clean_F36312.csv")
write.csv(clean_F44868,"clean_F44868.csv")
write.csv(clean_M36313,"clean_M36313.csv")
write.csv(clean_M68230,"clean_M68230.csv")

# --- Define directory to save workspace ---
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/data/work_space/")
save.image("data_prep_workspace.RData")
