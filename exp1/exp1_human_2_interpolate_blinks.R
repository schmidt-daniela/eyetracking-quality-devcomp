## This script identifies and interpolates blinks.
## This is needed for the calculation of robustness.
## The identification of eye blink is based on their characteristic of
## having a pronounced drop in the pupillary signal, followed by a full loss of signal. 
# (https://link.springer.com/article/10.3758/s13428-017-1008-1)
## Jan 19 – Daniela Schmidt

# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(gazer)

# Function ----------------------------------------------------------------
source(here("exp1", "R", "blink.R"))

# Adjust Parameter --------------------------------------------------------
folder_name <- "4m" # "4m" or "6m" or "9m" or "18m" or "adults"
filenames <- list.files(path = here("exp1", "data", "raw_clean", folder_name))

# Read and Manipulate Data ------------------------------------------------
for(i in 1:length(filenames)){
  nr <- i # nr 1 is first participant
  
  ## Read Data ----
  raw <- readRDS(here("exp1", "data", "raw_clean", folder_name, filenames[nr]))
  df <- raw
  
  ## Save Plot (Pre Smoothing)
  png(here("exp1", "doc", folder_name, paste0(filenames[i] |> str_replace(".rds",""), "_1_pupil_presmooth", ".png")), width = 2048, height = 1152, res = 300)
  p1 <- ggplot(df |>
                 mutate(time = cumsum(c(0, diff(recording_timestamp)))) |> 
                 mutate(time = cumsum(time)/1000) |> 
                 filter(!(presented_stimulus_name %in% c("Eyetracker Calibration", ""))),
               aes(x = time, y = pupil_diameter_left)) + 
    geom_point() + 
    geom_line(colour="black") +
    ylim(1,7)
  print(p1)
  dev.off()
  
  ## Smooth Data
  # Original signal is too noisy to detect blinks efficiently (reference: https://rdrr.io/github/dmirman/gazer/f/vignettes/blink_detection.Rmd#:~:text=Blink%20onsets%20were%20subsequently%20identified,this%20way%2C%20a%20blink%20corresponds)
  # Filter width in Michel et al. (2017): 5 gaze samples * 16.6ms (60Hz) = 83ms
  # That is, 10 gaze samples * 8.3 (120Hz)
  # In other words, n = 10 aligns with Michel et al. (2017)
  df <-  df |> 
    mutate(pupil_diameter_left = moving_average_pupil(pupil_diameter_left, n = 10))
  
  ## Save Plot (Post Smoothing)
  png(here("exp1", "doc", folder_name, paste0(filenames[i] |> str_replace(".rds",""), "_2_pupil_postsmooth", ".png")), width = 2048, height = 1152, res = 300)
  p2 <- ggplot(df |>
                 mutate(time = cumsum(c(0, diff(recording_timestamp)))) |> 
                 mutate(time = cumsum(time)/1000) |> 
                 filter(!(presented_stimulus_name %in% c("Eyetracker Calibration", ""))),
               aes(x = time, y = pupil_diameter_left)) + 
    geom_point() + 
    geom_line(colour="black") +
    ylim(1,7)
  print(p2)
  dev.off()
  
  ## Interpolate Outliers ----
  df <- interpolate_outliers(df = df, pupil_left_col  = "pupil_diameter_left", pupil_right_col = "pupil_diameter_right", n_sd = 3)
  df <- df |> rowwise() |>  mutate(pupil_diameter_average = mean(c(pupil_diameter_left, pupil_diameter_right), na.rm = T)) |> ungroup()
  
  ## Save Plot (Post Outlierinterpolation)
  png(here("exp1", "doc", folder_name, paste0(filenames[i] |> str_replace(".rds",""), "_3_pupil_postsmooth_postoutlier", ".png")), width = 2048, height = 1152, res = 300)
  p3 <- ggplot(df |>
                 mutate(time = cumsum(c(0, diff(recording_timestamp)))) |> 
                 mutate(time = cumsum(time)/1000) |> 
                 filter(!(presented_stimulus_name %in% c("Eyetracker Calibration", ""))),
               aes(x = time, y = pupil_diameter_left)) + 
    geom_point() + 
    geom_line(colour="black") +
    ylim(1,7)
  print(p3)
  dev.off()
  
  ## Add Velocity ----
  df <- add_pupil_velocity(df = df, timestamp_col = "recording_timestamp", timestamp_unit = "s", pupil_left_col = "pupil_diameter_left", pupil_right_col = "pupil_diameter_right")
  
  ## Add Onset Offset of NA Chains ----
  df <- mark_na_chain_onset_offset(df = df, col = "pupil_diameter_left", onset_col = "pupil_na_onset_left", offset_col = "pupil_na_offset.left", min_run = 2)
  df <- mark_na_chain_onset_offset(df = df, col = "pupil_diameter_right", onset_col = "pupil_na_onset.right", offset_col = "pupil_na_offset.right", min_run = 2)
  
  ## Add Velocity Threshold + Evaluate Whether It Was Crossed ----
  df <- detect_velocity_thresholds(df = df, vel_left_col  = "Velocity.left", vel_right_col = "Velocity.right",
                                   center = "median", n_sd = 0.1, onset_col_left   = "threshold_onset.left", # threshold n_sd selected based on visual inspection of plots
                                   onset_col_right  = "threshold_onset.right", offset_col_left  = "threshold_offset.left",
                                   offset_col_right = "threshold_offset.right")
  
  ## Detect Blinks Based on Onset of Threshold-Crossing ----
  df <- detect_blinks_from_onset_offset(df, pupil_left_col = "pupil_diameter_left", pupil_right_col = "pupil_diameter_right",
                                        onset_col_left = "threshold_onset.left", onset_col_right = "threshold_onset.right",
                                        offset_col_left = "threshold_offset.left", offset_col_right = "threshold_offset.right",
                                        onset_value = "onset", offset_value = "offset",
                                        blink_col_left = "blink_detection.left", blink_col_right = "blink_detection.right",
                                        blink_value = "blink", lookback = 5, lookahead = 5)
  
  ## Exclude Too Short/ Long Blinks ----
  df <- filter_blinks_by_duration(df, blink_col = "blink_detection.left", blink_value = "blink",
                                  timestamp_col  = "recording_timestamp",  # assumed in ms
                                  min_ms = 10, max_ms = 400)
  df <- filter_blinks_by_duration(df, blink_col = "blink_detection.right", blink_value = "blink",
                                  timestamp_col  = "recording_timestamp",  # assumed in ms
                                  min_ms = 10, max_ms = 400)
  
  ## Visualize Detected Blinks ----
  plots <- plot_blinks(
    df,
    eye = "both",
    flank_n = 10,
    min_run = 2,
    time_col  = "timeline_trial_tot",
    pupil_left_col = "pupil_diameter_left",
    pupil_right_col = "pupil_diameter_right",
    blink_col_left  = "blink_detection.left",
    blink_col_right = "blink_detection.right",
    title_cols = c("participant_name", "recording_name", "trial")
  )
  save_detected_blinks_pdf(plots, df)
  
  ## Write Table ----
  write.table(df, here("exp1", "data", "raw_clean_blink", folder_name, filenames[nr]),
              row.names = F, quote = F, sep = "\t", dec = ".")
  
  print(i)
}

# next: run the same for humans