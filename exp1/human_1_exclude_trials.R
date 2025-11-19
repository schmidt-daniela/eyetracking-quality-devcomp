## This script excludes invalid trials.
## A trial is not valid, if an individual didn't look in the stimulus AOI for at least one fixation. 
## Furthermore, trials with latencies shorter than 100 ms (only applies for human infants) and more than 3 SD
## of the individual mean will be excluded from the latency analyses (only applied for humans) (Daum & Gredebäck, 2011).
## Nov 18 2025 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Set Parameters ----------------------------------------------------------
folder <- "adults" # "4m", "6m", "9m", "18m", or "adults"
sample_size <- 32
buffer <- 40 # 40px (1°) in humans

# Functions ---------------------------------------------------------------
source(here("exp1", "R", "cleaning.R"))

# Define AOIs -------------------------------------------------------------

# Objects
topobject_x_topleft <- 870 - buffer
topobject_y_topleft <- 80 - buffer
topobject_x_botright <- 1050 + buffer
topobject_y_botright <- 260 + buffer

botobject_x_topleft <- 870 - buffer
botobject_y_topleft <- 820 - buffer
botobject_x_botright <- 1050 + buffer
botobject_y_botright <- 1000 + buffer

# Popflakes
topleftflake_x_topleft <- 380 - buffer
topleftflake_y_topleft <- 170 - buffer
topleftflake_x_botright <- 580 + buffer
topleftflake_y_botright <- 370 + buffer

botleftflake_x_topleft <- 380 - buffer
botleftflake_y_topleft <- 710 - buffer
botleftflake_x_botright <- 580 + buffer
botleftflake_y_botright <- 910 + buffer

toprightflake_x_topleft <- 1340 - buffer
toprightflake_y_topleft <- 170 - buffer
toprightflake_x_botright <- 1540 + buffer
toprightflake_y_botright <- 370 + buffer

botrightflake_x_topleft <- 1340 - buffer
botrightflake_y_topleft <- 710 - buffer
botrightflake_x_botright <- 1540 + buffer
botrightflake_y_botright <- 910 + buffer

centralflake_x_topleft <- 860 - buffer
centralflake_y_topleft <- 440 - buffer
centralflake_x_botright <- 1060 + buffer
centralflake_y_botright <- 640 + buffer

# Attention Getter
at_x_topleft <- 863 - buffer
at_y_topleft <- 443 - buffer
at_x_botright <- 1057 + buffer
at_y_botright <- 637 + buffer

# Trial Exclusion ---------------------------------------------------------
for(i in c(1:sample_size)){
  
  # Select file
  filenames <- list.files(path = here("exp1", "data", "raw_public", folder))
  n <- i
  filename <- filenames[n]
  
  # Read file
  raw <- read.table(here("exp1", "data", "raw_public", folder, filename), header = T, sep = "\t")
  df <- raw
  
  # Prepare stimulus information
  df <- suppressWarnings(df |> 
    separate(Presented.Stimulus.name, 
             into = c("trial", "task", "stimulus", "cond_both", "cond_jointness", "cue_direction",
                      "cond_congruence", "actor", "object_identity", "object_location", "cond_party"), 
             remove = F) |> 
    rename(fix_x = Fixation.point.X, fix_y = Fixation.point.Y))
  
  # Add gaze-sample duration
  df$gaze_sample_duration <- c(diff(df$Recording.timestamp), NA) / 1000
  
  # Add cumulative duration per stimulus
  df <- df |> 
    group_by(trial, stimulus) |> 
    mutate(timeline = cumsum(gaze_sample_duration))
  
  # Define AOIs
  df$aoi <- "not_in_aoi"
  
  df <- df |>
    mutate(aoi = "not_in_aoi") |>
    mark_aoi("top_left", topleftflake_x_topleft, topleftflake_x_botright,
             topleftflake_y_topleft, topleftflake_y_botright, stimuli = "checkflake") |>
    mark_aoi("bot_left", botleftflake_x_topleft, botleftflake_x_botright,
             botleftflake_y_topleft, botleftflake_y_botright, stimuli = "checkflake") |>
    mark_aoi("top_right", toprightflake_x_topleft, toprightflake_x_botright,
             toprightflake_y_topleft, toprightflake_y_botright, stimuli = "checkflake") |>
    mark_aoi("bot_right",  botrightflake_x_topleft, botrightflake_x_botright,
             botrightflake_y_topleft,  botrightflake_y_botright, stimuli = "checkflake") |>
    mark_aoi("center_center", centralflake_x_topleft, centralflake_x_botright,
             centralflake_y_topleft,  centralflake_y_botright, stimuli = "checkflake") |>
    mark_aoi("at", at_x_topleft, at_x_botright, at_y_topleft, at_y_botright,
             stimuli = "at") |>
    mark_aoi("top", topobject_x_topleft, topobject_x_botright,
             topobject_y_topleft, topobject_y_botright, stimuli = c("move", "still")) |>
    mark_aoi("bottom", botobject_x_topleft, botobject_x_botright,
             botobject_y_topleft, botobject_y_botright, stimuli = c("move", "still"))
  
  # Identify latencies <100ms (based on gaze samples)
  latencies_top <- df |> 
    ungroup() |> 
    filter(stimulus %in% c("move", "still")) |> 
    filter(object_location == "top" & aoi == "top") |>
    select(Gaze.point.X, Gaze.point.Y, fix_x, fix_y, Validity.left, Validity.right, 
           Presented.Stimulus.name, Eye.movement.type, trial, gaze_sample_duration, timeline, aoi) |> 
    group_by(trial) |> 
    slice(1) |> 
    ungroup() |> 
    mutate(latency = timeline)
  
  latencies_bottom <- df |> 
    ungroup() |> 
    filter(stimulus %in% c("move", "still")) |> 
    filter(object_location == "bottom" & aoi == "bottom") |>
    select(Gaze.point.X, Gaze.point.Y, Validity.left, Validity.right, 
           Presented.Stimulus.name, Eye.movement.type, trial, gaze_sample_duration, timeline, aoi) |> 
    group_by(trial) |> 
    slice(1) |> 
    ungroup() |> 
    mutate(latency = timeline)
  
  excluded_trials_100ms <- latencies_top |> 
    bind_rows(latencies_bottom) |> 
    filter(latency < 100) |>
    pull(trial) |> 
    as.numeric()
  
  # Add exclusion information about latencies <100ms (except for adults)
  if(folder == "adult"){
    excluded_trials_100ms <- NULL
  }
  
  df$excluded_100ms <- "included"
  df[df$trial %in% excluded_trials_100ms & df$stimulus != "at", "excluded_100ms"] <- "excluded"
  
  # Add exclusion information about latencies deviating more than 3 standard deviations of each individual mean
  mean_latency <- c(latencies_top$latency, latencies_bottom$latency) |> mean(na.rm = T)
  twosd_latency <- c(latencies_top$latency, latencies_bottom$latency) |> sd(na.rm = T) * 3
  
  excluded_trials_threesd <- latencies_top |>
    bind_rows(latencies_bottom) |>
    filter(latency > mean_latency + twosd_latency | latency < mean_latency - twosd_latency) |>
    pull(trial) |> 
    as.numeric()
  
  df$excluded_3sd <- "included"
  df[df$trial %in% excluded_trials_threesd & df$stimulus != "at", "excluded_3sd"] <- "excluded"
  
  # Identify whether at least one fixation within AOI
  df <- df |> 
    unite("condition", cond_jointness:cue_direction, remove = F) |> 
    mutate(stimulus_location = NA)
  
  df[df$condition == "top_left","stimulus_location"] <- "top_left"
  df[df$condition == "top_right","stimulus_location"] <- "top_right"
  df[df$condition == "bot_left","stimulus_location"] <- "bot_left"
  df[df$condition == "bot_right","stimulus_location"] <- "bot_right"
  df[df$condition == "center_center","stimulus_location"] <- "center_center"
  df <- df |> replace_na(list(object_location = "0"))
  df[df$object_location == "top","stimulus_location"] <- "top"
  df[df$object_location == "bottom","stimulus_location"] <- "bottom"
  df <- df |> replace_na(list(stimulus = "0"))
  df[df$stimulus == "at","stimulus_location"] <- "at"
  
  included_fixation <- df |> 
    ungroup() |> 
    drop_na(stimulus) |> 
    select(trial, stimulus, cond_jointness, cue_direction, object_location, aoi, Eye.movement.type, stimulus_location) |> 
    filter(Eye.movement.type == "Fixation") |> 
    filter(aoi != "not_in_aoi") |> 
    distinct() |> 
    unite("condition", cond_jointness:cue_direction)
  
  included_fixation_2 <- included_fixation |> 
    filter(aoi == stimulus_location) |> 
    select(stimulus_location, aoi, trial) |>
    distinct() |> 
    unite("trial_stimulus_location", c(trial, stimulus_location), remove = F) |> 
    pull(trial_stimulus_location)
  
  df <- df |> 
    unite("trial_stimulus_location", c(trial, stimulus_location), remove = F)
  
  # Add exclusion information about whether at least one fixation within AOI
  df$excluded_fixation <- "excluded"
  df[df$trial_stimulus_location %in% included_fixation_2, "excluded_fixation"] <- "included"
  
  # Merge data
  dat_fin <- raw |> 
    left_join(df |> ungroup() |> select(Presented.Stimulus.name, Computer.timestamp, Recording.timestamp, Event, Sensor, aoi, excluded_100ms, excluded_3sd, excluded_fixation), 
              by = c("Recording.timestamp", "Computer.timestamp", "Event", "Presented.Stimulus.name", "Sensor")) # Added sensor because there was at least one case where 
                                                                                                                 # two rows had the same timestamp and were only distinguishable by sensor (Mouse versus Eyetracker)
  
  # Write data
  write.table(dat_fin, here("exp1", "data", "raw_public_exclude", folder, paste0(sub("\\.tsv$", "", filename), ".txt")), 
              row.names = F, quote = F, sep = "\t", dec = ".")
  print(i)
}