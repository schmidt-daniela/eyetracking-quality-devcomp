# Eye blinks are characterized by a pronounced drop in the pupillary signal, followed by a full loss of signal. (https://rdrr.io/github/dmirman/gazer/f/vignettes/blink_detection.Rmd#:~:text=Blink%20onsets%20were%20subsequently%20identified,this%20way%2C%20a%20blink%20corresponds)

# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(gazer)

# Function ----------------------------------------------------------------
source(here("fun", "blink.R"))

# Adjust Parameter --------------------------------------------------------
age_group <- "4mo" # "4mo" or "6to18mo"
filenames <- list.files(path = here("data", "raw_included", age_group))

# Read and Manipulate Data ------------------------------------------------
for(i in 1:length(filenames)){
  nr <- i # nr 1 is first participant
  
  ## Read Data ----
  raw <- read.csv(here("data", "raw_included", age_group, filenames[nr]), header = T, sep = "\t")
  df <- raw
  
  ## Save Plot (Pre Smoothing)
  png(here("img", age_group, paste0(filenames[i] |> str_replace(".tsv",""), "_pupil_presmooth", ".png")), width = 2048, height = 1152, res = 300)
  p1 <- ggplot(df |>
           mutate(time = cumsum(c(0, diff(Recording.timestamp)))) |> 
           mutate(time = cumsum(time)/1000) |> 
           filter(!(Presented.Stimulus.name %in% c("Eyetracker Calibration", ""))),
         aes(x = time, y = Pupil.diameter.left)) + 
    geom_point() + 
    geom_line(colour="black") +
    ylim(1,4)
  print(p1)
  dev.off()
  
  ## Smooth Data
  # Original signal is too noisy to detect blinks efficiently (reference: https://rdrr.io/github/dmirman/gazer/f/vignettes/blink_detection.Rmd#:~:text=Blink%20onsets%20were%20subsequently%20identified,this%20way%2C%20a%20blink%20corresponds)
  # Filter width in Michel et al. (2017): 5 gaze samples * 16.6ms (60Hz) = 83ms
  # That is, 10 gaze samples * 8.3 (120Hz)
  # In other words, n = 10 aligns with Michel et al. (2017)
  df <-  df |> 
    mutate(Pupil.diameter.left = moving_average_pupil(Pupil.diameter.left, n = 10))

  ## Save Plot (Post Smoothing)
  png(here("img", age_group, paste0(filenames[i] |> str_replace(".tsv",""), "_pupil_postsmooth", ".png")), width = 2048, height = 1152, res = 300)
  p2 <- ggplot(df |>
           mutate(time = cumsum(c(0, diff(Recording.timestamp)))) |> 
           mutate(time = cumsum(time)/1000) |> 
           filter(!(Presented.Stimulus.name %in% c("Eyetracker Calibration", ""))),
         aes(x = time, y = Pupil.diameter.left)) + 
    geom_point() + 
    geom_line(colour="black") +
    ylim(1,4)
  print(p2)
  dev.off()
  
  ## Interpolate Outliers ----
  df <- interpolate_outliers(df = df, pupil_left_col  = "Pupil.diameter.left", pupil_right_col = "Pupil.diameter.right", n_sd = 3)
  
  ## Save Plot (Post Outlierinterpolation)
  png(here("img", age_group, paste0(filenames[i] |> str_replace(".tsv",""), "_pupil_postsmooth_postoutlier", ".png")), width = 2048, height = 1152, res = 300)
  p3 <- ggplot(df |>
           mutate(time = cumsum(c(0, diff(Recording.timestamp)))) |> 
           mutate(time = cumsum(time)/1000) |> 
           filter(!(Presented.Stimulus.name %in% c("Eyetracker Calibration", ""))),
         aes(x = time, y = Pupil.diameter.left)) + 
    geom_point() + 
    geom_line(colour="black") +
    ylim(1,4)
  print(p3)
  dev.off()
  
  ## Add Velocity ----
  df <- add_pupil_velocity(df = df, timestamp_col = "Recording.timestamp", timestamp_unit = "s", pupil_left_col = "Pupil.diameter.left", pupil_right_col = "Pupil.diameter.right")
  
  ## Add Onset Offset of NA Chains ----
  df <- mark_na_chain_onset_offset(df = df, col = "Pupil.diameter.left", onset_col = "Pupil.NA.onset.left", offset_col = "Pupil.NA.offset.left", min_run = 2)
  df <- mark_na_chain_onset_offset(df = df, col = "Pupil.diameter.right", onset_col = "Pupil.NA.onset.right", offset_col = "Pupil.NA.offset.right", min_run = 2)
  
  ## Add Velocity Threshold + Evaluate Whether It Was Crossed ----
  df <- detect_velocity_thresholds(df = df, vel_left_col  = "Velocity.left", vel_right_col = "Velocity.right",
                                   center = "median", n_sd = 1, onset_col_left   = "threshold_onset.left",
                                   onset_col_right  = "threshold_onset.right", offset_col_left  = "threshold_offset.left",
                                   offset_col_right = "threshold_offset.right")
  
  ## Detect Blinks Based on Onset of Threshold-Crossing ----
  df <- detect_blinks_from_onset(df, pupil_left_col = "Pupil.diameter.left", pupil_right_col = "Pupil.diameter.right",
                                 onset_col_left = "threshold_onset.left", onset_col_right = "threshold_onset.right",
                                 onset_value = "onset",
                                 blink_col_left = "blink_detection.left", blink_col_right = "blink_detection.right",
                                 blink_value = "blink")
  
  ## Exclude Too Short/ Long Blinks ----
  df <- filter_blinks_by_duration(df, blink_col = "blink_detection.left", blink_value = "blink",
    timestamp_col  = "Recording.timestamp",  # assumed in ms
    min_ms = 30, max_ms = 400)
  df <- filter_blinks_by_duration(df, blink_col = "blink_detection.right", blink_value = "blink",
                                  timestamp_col  = "Recording.timestamp",  # assumed in ms
                                  min_ms = 30, max_ms = 400)
    
  ## Visualize Detected Blinks ----
  plot_blinks(df, eye = "both", flank_n = 10, timestamp_col = "Recording.timestamp",
              pupil_left_col = "Pupil.diameter.left", pupil_right_col   = "Pupil.diameter.right",
              blink_col_left    = "blink_detection.left", blink_col_right   = "blink_detection.right",
              plot_col = 2)
  
  ## Write Table ----
  write.table(df, here("data", "raw_included_blink", age_group, filenames[nr]), 
              row.names = F, quote = F, sep = "\t", dec = ".")
  
  print(i)
}
