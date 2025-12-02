## This script excludes invalid trials.
## A trial is not valid, if an individual didn't look in the stimulus AOI for at least one fixation. 
## Dec 01 2025 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Set Parameters ----------------------------------------------------------
folder <- "chimps"
sample_size <- 17
buffer <- 120 # 120px (3°) in chimps

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
  
  # # Remove rows that are not of interest
  # df <- df |> 
  #   filter(presented_stimulus_name != "Eyetracker Calibration")
  
  # Prepare stimulus information
  df <- df |>
    separate(presented_stimulus_name,
             into = c("session", "trial", "delete1", "stimulus", "stimulus_duration", "checkflake_location1", 
                      "checkflake_location2", "delete2", "delete3", "object_identity", "object_location", "delete4"), 
             remove = FALSE, sep = "_") |>
    select(-c(delete1, delete2, delete3, delete4)) |> 
    suppressWarnings()
  
  df <- df |>
    mutate(object_location = if_else(stimulus %in% c("move", "still"), checkflake_location2, object_location))

  # Set up trial and stimulus information
  df <- df |> 
    mutate(stimulus_duration = case_when(
      stimulus == "move"  ~ "gaze_contingent",
      stimulus == "still" ~ "1s",
      stimulus == "at1"  ~ "gaze_contingent",
      stimulus == "at2" ~ "1s",
      stimulus_duration == "20s" ~ "gaze_contingent",
      TRUE ~ stimulus_duration)) |> 
    mutate(stimulus = case_when(
      stimulus %in% c("move","still") ~ "object",
      stimulus %in% c("at1","at2") ~ "at",
      TRUE                            ~ stimulus)) |> 
    mutate(position = case_when(
      stimulus == "checkflake" & checkflake_location1 == "center" & checkflake_location2 == "center" ~ "center",
      stimulus == "checkflake" ~ paste(checkflake_location1, checkflake_location2, sep = "_"),
      object_location == "up" ~ "top",
      object_location == "down" ~ "bottom",
      stimulus == "object" ~ object_location,
      stimulus == "at" ~ "center",
      TRUE ~ object_location)) |> 
    unite("stimulus_position", c("stimulus", "position"), sep = "_", remove = F)
  
  df[which(df$stimulus == "cueing"), "stimulus"] <- NA
  df[which(df$stimulus |> str_detect("Kalei")), "stimulus"] <- NA
  
  df <- df |> select(-object_location, checkflake_location1, checkflake_location2) # redundant information
  
  df <- df |> 
    group_by(session, recording_name) |> 
    mutate(trial = make_trial_num(stimulus, stimulus_position),
           trial = na_if(trial, 0L)) |> 
    ungroup()
  
  df[is.na(df$stimulus),"trial"] <- NA
  
  # Add cumulative duration per stimulus
  df <- df |> 
    group_by(recording_name, trial, stimulus_duration) |> 
    mutate(timeline = cumsum(gaze_sample_duration)) |> 
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
  
  ## continue here
  # trial exclusion
  # blink detection
  # Remove delay after contingency was elicted
  # maybe save calibration results in extra file
  # add time per session (independent of trial, cum not per trial)
  
  # # Identify whether at least one fixation within AOI
  # fixation_in_aoi <- df |> 
  #   drop_na(stimulus) |> 
  #   filter(Eye.movement.type == "Fixation") |>
  #   select(Recording.name, Presented.Stimulus.name, aoi, stimulus) |> 
  #   filter(aoi != "not_in_aoi") |> 
  #   distinct() |> 
  #   select(Recording.name, Presented.Stimulus.name) |> 
  #   mutate(excluded_fixation = "included")
  # 
  # df <- df |> 
  #   left_join(fixation_in_aoi, by = c("Recording.name", "Presented.Stimulus.name")) |> 
  #   mutate(excluded_fixation = replace_na(excluded_fixation, "excluded"))
  
  # Write data
  write.table(df, here("exp1", "data", "raw_clean", folder, paste0(sub("\\.tsv$", "", filename), ".txt")), 
              row.names = F, quote = F, sep = "\t", dec = ".")
  print(i)
}
