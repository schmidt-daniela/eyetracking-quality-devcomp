## This script excludes invalid trials.
## A trial is not valid, if an individual didn't look in the stimulus AOI for at least one fixation. 
## Furthermore, trials with latencies shorter than 100 ms (only applies for human infants) and more than 3 SD
## of the individual mean will be excluded from the latency analyses (only applied for humans) (Daum & Gredebäck, 2011).
## Dec 01 2025 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(readxl)

# Set Parameters ----------------------------------------------------------
folder <- "18m" # "4m", "6m", "9m", "18m", or "adults"
sample_size <- 32
buffer <- 40 # 40px (1°) in humans

# Functions ---------------------------------------------------------------
source(here("exp1", "R", "cleaning.R"))

# Load Sample Data --------------------------------------------------------
protocol <- read_excel(here("exp1", "doc", "protocol.xlsx"), sheet = folder)

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
  filenames <- list.files(path = here("exp1", "data", "raw_2", folder))
  n <- i
  filename <- filenames[n]
  
  # Read file
  raw <- read.table(here("exp1", "data", "raw_2", folder, filename), header = T, sep = "\t")
  df <- raw
  
  # Add gaze-sample duration
  df$gaze_sample_duration <- c(diff(df$Recording.timestamp), NA) / 1000
  
  # Tidy column names
  df <- df |>  
    rename_with(~ .x |> 
                  str_replace_all("[^A-Za-z0-9]+", "_") |>  # replace everything that's not a number or a letter with _
                  str_remove("^_+") |>  # remove one or more _ at the beginning of a string
                  str_remove("_+$") |>  # remove one or more _ at the end of a string
                  str_to_lower()) # make all letters lowercase
  
  # Remove columns that are not of interest and/or not informative
  df <- df |> 
    select(-c(ungrouped, client_area_position_x_dacspx, client_area_position_y_dacspx, viewport_position_x, # variables with NA only
              viewport_position_y, viewport_width, viewport_height, full_page_width, full_page_height, # variables with NA only
              mouse_position_x,mouse_position_y)) # variable not of interest

  # Prepare stimulus information
  df <- df |>
    separate(presented_stimulus_name,
             into = c("trial", "delete1", "stimulus", "stimulus_duration", "checkflake_location1", 
                      "checkflake_location2", "delete2", "delete3", "object_identity", "object_location", "delete4"), 
               remove = FALSE, sep = "_") |>
      select(-c(delete1, delete2, delete3, delete4)) |> 
    suppressWarnings()
  
  # Set up trial and stimulus information
  df <- df |> 
    mutate(stimulus_duration = case_when(
      stimulus == "move"  ~ "gaze_contingent",
      stimulus == "still" ~ "1s",
      stimulus == "at"    ~ "experimenter-controlled",
      stimulus_duration %in% c("Kalei1","Kalei2","Kalei3","Kalei4") ~ "4s",
      TRUE ~ stimulus_duration)) |> 
    mutate(stimulus = case_when(
      stimulus %in% c("move","still") ~ "object",
      stimulus == "x"                 ~ "kalei",
      TRUE                            ~ stimulus)) |> 
    mutate(position = case_when(
      stimulus == "checkflake" & checkflake_location1 == "center" & checkflake_location2 == "center" ~ "center",
      stimulus == "checkflake" ~ paste(checkflake_location1, checkflake_location2, sep = "_"),
      stimulus == "object" ~ object_location,
      stimulus == "at" ~ "center",
      TRUE ~ object_location)) |> 
    unite("stimulus_position", c("stimulus", "position"), sep = "_", remove = F)
  
  df[which(df$stimulus == "cueing"), "stimulus"] <- NA
  df[which(df$stimulus |> str_detect("Kalei")), "stimulus"] <- NA
  
  df <- df |> select(-object_location) # redundant, information is in position

  df <- df |> 
    mutate(trial = make_trial_num(stimulus, stimulus_position),
           trial = na_if(trial, 0L))

  df[is.na(df$stimulus),"trial"] <- NA
  
  # Add cumulative duration per stimulus
  df <- df |> 
    group_by(trial, stimulus_duration) |> 
    mutate(timeline_experiment = cumsum(gaze_sample_duration)) |> 
    ungroup()
  
  # Define AOIs (based on fixations)
  df$aoi_fixation <- "not_in_aoi"
  
  df <- df |>
    mutate(aoi_fixation = "not_in_aoi") |>
    mark_aoi("top_left", topleftflake_x_topleft, topleftflake_x_botright, topleftflake_y_topleft, topleftflake_y_botright,
             stimulus_name = "checkflake", position_name = "top_left", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("bot_left", botleftflake_x_topleft, botleftflake_x_botright, botleftflake_y_topleft, botleftflake_y_botright,
             stimulus_name = "checkflake", position_name = "bot_left", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("top_right", toprightflake_x_topleft, toprightflake_x_botright, toprightflake_y_topleft, toprightflake_y_botright, 
             stimulus_name = "checkflake", position_name = "top_right", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("bot_right",  botrightflake_x_topleft, botrightflake_x_botright, botrightflake_y_topleft,  botrightflake_y_botright, 
             stimulus_name = "checkflake", position_name = "bot_right", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("center_center", centralflake_x_topleft, centralflake_x_botright, centralflake_y_topleft,  centralflake_y_botright, 
             stimulus_name = "checkflake", position_name = "center", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("at", at_x_topleft, at_x_botright, at_y_topleft, at_y_botright,
             stimulus_name = "at", position_name = "center", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("top", topobject_x_topleft, topobject_x_botright, topobject_y_topleft, topobject_y_botright, 
             stimulus_name = "object", position_name = "top", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("bottom", botobject_x_topleft, botobject_x_botright, botobject_y_topleft, botobject_y_botright, 
             stimulus_name = "object", position_name = "bottom", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation")
  
  # Define AOIs (based on gaze samples)
  df$aoi_samples <- "not_in_aoi"
  
  df <- df |>
    mutate(aoi_samples = "not_in_aoi") |>
    mark_aoi("top_left", topleftflake_x_topleft, topleftflake_x_botright, topleftflake_y_topleft, topleftflake_y_botright,
             stimulus_name = "checkflake", position_name = "top_left", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("bot_left", botleftflake_x_topleft, botleftflake_x_botright, botleftflake_y_topleft, botleftflake_y_botright,
             stimulus_name = "checkflake", position_name = "bot_left", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("top_right", toprightflake_x_topleft, toprightflake_x_botright, toprightflake_y_topleft, toprightflake_y_botright, 
             stimulus_name = "checkflake", position_name = "top_right", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("bot_right",  botrightflake_x_topleft, botrightflake_x_botright, botrightflake_y_topleft,  botrightflake_y_botright, 
             stimulus_name = "checkflake", position_name = "bot_right", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("center_center", centralflake_x_topleft, centralflake_x_botright, centralflake_y_topleft,  centralflake_y_botright, 
             stimulus_name = "checkflake", position_name = "center", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("at", at_x_topleft, at_x_botright, at_y_topleft, at_y_botright,
             stimulus_name = "at", position_name = "center", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("top", topobject_x_topleft, topobject_x_botright, topobject_y_topleft, topobject_y_botright, 
             stimulus_name = "object", position_name = "top", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("bottom", botobject_x_topleft, botobject_x_botright, botobject_y_topleft, botobject_y_botright, 
             stimulus_name = "object", position_name = "bottom", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples")
  
  # Trial exclusion: Latencies <100ms (based on gaze samples)
  latencies_top <- df |> 
    filter(stimulus == "object") |> 
    filter(position == "top" & aoi_samples == "top") |>
    select(gaze_point_x, gaze_point_y, fixation_point_x, fixation_point_y, 
           stimulus, position, eye_movement_type, trial, gaze_sample_duration, timeline_experiment, aoi_samples) |> 
    group_by(trial) |> 
    slice(1) |> 
    ungroup() |> 
    mutate(latency = timeline_experiment)
  
  latencies_bottom <- df |> 
    filter(stimulus == "object") |> 
    filter(position == "bottom" & aoi_samples == "bottom") |>
    select(gaze_point_x, gaze_point_y, fixation_point_x, fixation_point_y, 
           stimulus, position, eye_movement_type, trial, gaze_sample_duration, timeline_experiment, aoi_samples) |> 
    group_by(trial) |> 
    slice(1) |> 
    ungroup() |> 
    mutate(latency = timeline_experiment)
  
  excluded_trials_100ms <- latencies_top |> 
    bind_rows(latencies_bottom) |> 
    filter(latency < 100) |>
    pull(trial) |> 
    as.numeric()
  
  if(folder == "adult"){
    excluded_trials_100ms <- NULL
  }
  
  df$excluded_100ms <- "included"
  df[df$trial %in% excluded_trials_100ms & df$stimulus == "object", "excluded_100ms"] <- "excluded"
  
  # Trial exclusion: Latencies deviating more than 3 standard deviations of each individual mean
  if(folder != "adults"){latencies_top <- latencies_top |> filter(latency >= 100)}
  if(folder != "adults"){latencies_bottom <- latencies_bottom |> filter(latency >= 100)}
  mean_latency <- c(latencies_top$latency, latencies_bottom$latency) |> mean(na.rm = T)
  twosd_latency <- c(latencies_top$latency, latencies_bottom$latency) |> sd(na.rm = T) * 3
  
  excluded_trials_threesd <- latencies_top |>
    bind_rows(latencies_bottom) |>
    filter(latency > mean_latency + twosd_latency | latency < mean_latency - twosd_latency) |>
    pull(trial) |> 
    as.numeric()
  
  df$excluded_3sd <- "included"
  df[df$trial %in% excluded_trials_threesd & df$stimulus == "object", "excluded_3sd"] <- "excluded"
  
  # Trial exclusion: At least one fixation within AOI
  included_fixation <- df |> 
    drop_na(stimulus) |> 
    select(trial, stimulus, position, aoi_fixation, eye_movement_type) |> 
    filter(eye_movement_type == "Fixation") |> 
    filter(aoi_fixation != "not_in_aoi") |> 
    distinct() |> 
    pull(trial)

  df$excluded_fixation <- "excluded"
  df[df$trial %in% included_fixation, "excluded_fixation"] <- "included"
  
  # Add cumulative duration per trial
  df <- df |> 
    group_by(trial, stimulus_duration) |> 
    mutate(timeline_trial_units = cumsum(gaze_sample_duration)) |> 
    group_by(trial) |> 
    mutate(timeline_trial_tot = cumsum(gaze_sample_duration)) |> 
    ungroup()

  # Add information about participant
  if(folder != "adults"){
    df_joined <- df |> 
      bind_cols(protocol |>  
                  select(age_ddd, sex, order, no_household, no_siblings, multilingual, kindergarten_yn, tagesmutter_yn, experimenter) |> 
                  slice(i))
  }
  
  if(folder == "adults"){
    df_joined <- df |> 
      bind_cols(protocol |>  
                  select(sex, order, age_md, experimenter) |> 
                  slice(i))
  }
  
  # Write data
  fname_base <- sub("\\.tsv$", "", filename)
  out_rds <- here("exp1", "data", "raw_clean", folder, paste0(fname_base, ".rds"))
  saveRDS(df_joined, out_rds, compress = "xz")

  print(i)
}