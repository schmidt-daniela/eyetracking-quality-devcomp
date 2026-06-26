## This script excludes invalid trials.
## A trial is not valid, if an individual didn't look in the stimulus AOI for at least one fixation. 
## Furthermore, trials with latencies shorter than 100 ms (only applies for human infants) and more than 3 SD
## of the individual mean will be excluded from the latency analyses (only applied for humans) (Daum & Gredebäck, 2011).
## June 12 2026 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(readxl)

# Set Parameters ----------------------------------------------------------
folder <- "4mo" # "4mo", "6to18mo"
sample_size <- ifelse(folder == "4mo", 24, 32)
buffer <- 40 # 40px (1°) in humans

# Functions ---------------------------------------------------------------
source(here("exp3", "R", "cleaning.R"))

# Load Sample Data --------------------------------------------------------
protocol <- read_excel(here("exp3", "doc", "protocol.xlsx"), sheet = folder)

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

# Attention Getter (Pinwheel)
at_x_topleft <- 863 - buffer
at_y_topleft <- 443 - buffer
at_x_botright <- 1057 + buffer
at_y_botright <- 637 + buffer

# Trial Exclusion ---------------------------------------------------------
for(i in c(1:sample_size)){
  
  # Select file
  filenames <- list.files(path = here("exp3", "data", "raw_included", folder))
  n <- i
  filename <- filenames[n]
  
  # Read file
  raw <- read.table(here("exp3", "data", "raw_included", folder, filename), header = T, sep = "\t")
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
  df$presented_stimulus_name <- rename_stimulus(df$presented_stimulus_name)
  
  df <- df |>
    separate(presented_stimulus_name, into = c("trial", "stimulus", "stimulus_duration", "position"), remove = FALSE, sep = "_") |> 
    suppressWarnings()

  df <- df |> 
    unite("stimulus_position", c("stimulus", "position"), sep = "_", remove = F)
  
  # Remove pilot stimuli of social attention study
  df <- df |> 
    filter(!(presented_stimulus_name %in% c("cajo2pilot", "Eyetracker Calibration"))) |> 
    drop_na(stimulus)
  
  # Add trial
  df <- df |>
    group_by(recording_name) |> 
    mutate(trial = make_trial_num(stimulus, stimulus_position)) |> 
    ungroup()

  # Define AOIs (based on fixations)
  df$aoi_fixation <- "not_in_aoi"
  
  df <- df |>
    mutate(aoi_fixation = "not_in_aoi") |>
    mark_aoi("top_left", topleftflake_x_topleft, topleftflake_x_botright, topleftflake_y_topleft, topleftflake_y_botright,
             stimulus_name = "popflake", position_name = "topleft", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("bot_left", botleftflake_x_topleft, botleftflake_x_botright, botleftflake_y_topleft, botleftflake_y_botright,
             stimulus_name = "popflake", position_name = "botleft", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("top_right", toprightflake_x_topleft, toprightflake_x_botright, toprightflake_y_topleft, toprightflake_y_botright, 
             stimulus_name = "popflake", position_name = "topright", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("bot_right",  botrightflake_x_topleft, botrightflake_x_botright, botrightflake_y_topleft,  botrightflake_y_botright, 
             stimulus_name = "popflake", position_name = "botright", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("center_center", centralflake_x_topleft, centralflake_x_botright, centralflake_y_topleft,  centralflake_y_botright, 
             stimulus_name = "popflake", position_name = "center", x_col = "fixation_point_x", y_col = "fixation_point_y",
             aoi_col = "aoi_fixation") |>
    mark_aoi("pinwheel", at_x_topleft, at_x_botright, at_y_topleft, at_y_botright,
             stimulus_name = "pinwheel", position_name = "center", x_col = "fixation_point_x", y_col = "fixation_point_y",
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
             stimulus_name = "popflake", position_name = "topleft", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("bot_left", botleftflake_x_topleft, botleftflake_x_botright, botleftflake_y_topleft, botleftflake_y_botright,
             stimulus_name = "popflake", position_name = "botleft", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("top_right", toprightflake_x_topleft, toprightflake_x_botright, toprightflake_y_topleft, toprightflake_y_botright, 
             stimulus_name = "popflake", position_name = "topright", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("bot_right",  botrightflake_x_topleft, botrightflake_x_botright, botrightflake_y_topleft,  botrightflake_y_botright, 
             stimulus_name = "popflake", position_name = "botright", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("center_center", centralflake_x_topleft, centralflake_x_botright, centralflake_y_topleft,  centralflake_y_botright, 
             stimulus_name = "popflake", position_name = "center", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("pinwheel", at_x_topleft, at_x_botright, at_y_topleft, at_y_botright,
             stimulus_name = "pinwheel", position_name = "center", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("top", topobject_x_topleft, topobject_x_botright, topobject_y_topleft, topobject_y_botright, 
             stimulus_name = "object", position_name = "top", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples") |>
    mark_aoi("bottom", botobject_x_topleft, botobject_x_botright, botobject_y_topleft, botobject_y_botright, 
             stimulus_name = "object", position_name = "bottom", x_col = "gaze_point_x", y_col = "gaze_point_y",
             aoi_col = "aoi_samples")
  
  # Trial exclusion: At least one fixation within AOI
  included_fixation <- df |> 
    drop_na(stimulus) |> 
    select(recording_name, trial, stimulus, position, aoi_fixation, eye_movement_type) |> 
    filter(eye_movement_type == "Fixation") |> 
    filter(aoi_fixation != "not_in_aoi") |> 
    distinct() |> 
    select(recording_name, trial)
  
  df <- df |>
    mutate(trial_included = if_else(
      interaction(recording_name, trial) %in%
        interaction(included_fixation$recording_name, included_fixation$trial),
      "yes",
      "no"
    ))

  # Add cumulative duration per trial
  df <- df |> 
    group_by(recording_name, trial, stimulus_duration) |> 
    mutate(timeline_trial_units = cumsum(gaze_sample_duration)) |> 
    group_by(recording_name, trial) |> 
    mutate(timeline_trial_tot = cumsum(gaze_sample_duration)) |> 
    ungroup()

  # Add information about participant
  df_joined <- df |> 
      bind_cols(protocol |>  
                  select(age_days, sex, order_peer_first, order_adult_first, adult_calibration, peer_calibration, no_household, no_siblings, multilingual, kindergarten_yn, tagesmutter_yn, experimenter) |> 
                  slice(i))

  # Write data
  fname_base <- sub("\\.tsv$", "", filename)
  out_rds <- here("exp3", "data", "raw_clean", folder, paste0(fname_base, ".rds"))
  saveRDS(df_joined, out_rds, compress = "xz")

  print(i)
}