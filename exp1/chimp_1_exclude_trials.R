## This script excludes invalid trials.
## A trial is not valid, if an individual didn't look in the stimulus AOI for at least one fixation. 
## Nov 18 2025 – Daniela Schmidt

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
  filenames <- list.files(path = here("exp1", "data", "raw_public", folder))
  n <- i
  filename <- filenames[n]
  
  # Read file
  raw <- read.table(here("exp1", "data", "raw_public", folder, filename), header = T, sep = "\t")
  df <- raw
  
  # Prepare stimulus information
  df <- df |> 
    mutate(stimulus = case_when(
      str_detect(Presented.Stimulus.name, "checkflake") & str_detect(Presented.Stimulus.name, "top_right") ~ "checkflake_topright",
      str_detect(Presented.Stimulus.name, "checkflake") & str_detect(Presented.Stimulus.name, "top_left") ~ "checkflake_topleft",
      str_detect(Presented.Stimulus.name, "checkflake") & str_detect(Presented.Stimulus.name, "bot_right") ~ "checkflake_botright",
      str_detect(Presented.Stimulus.name, "checkflake") & str_detect(Presented.Stimulus.name, "bot_left") ~ "checkflake_botleft",
      str_detect(Presented.Stimulus.name, "checkflake") & str_detect(Presented.Stimulus.name, "center_center") ~ "checkflake_center",
      str_detect(Presented.Stimulus.name, "at1|at2")    ~ "at",
      str_detect(Presented.Stimulus.name, "move|still") & str_detect(Presented.Stimulus.name, "up") ~ "objecttop",
      str_detect(Presented.Stimulus.name, "move|still") & str_detect(Presented.Stimulus.name, "down") ~ "objectbottom",
      TRUE ~ Presented.Stimulus.name)) |> 
    rename(fix_y = Fixation.point.Y, fix_x = Fixation.point.X)
  
  # Define AOIs
  df <- df |>
    mutate(aoi = "not_in_aoi") |>
    mark_aoi("top_left", topleftflake_x_topleft, topleftflake_x_botright,
             topleftflake_y_topleft, topleftflake_y_botright, stimuli = "checkflake_topleft") |>
    mark_aoi("bot_left", botleftflake_x_topleft, botleftflake_x_botright,
             botleftflake_y_topleft, botleftflake_y_botright, stimuli = "checkflake_botleft") |>
    mark_aoi("top_right", toprightflake_x_topleft, toprightflake_x_botright,
             toprightflake_y_topleft, toprightflake_y_botright, stimuli = "checkflake_topright") |>
    mark_aoi("bot_right",  botrightflake_x_topleft, botrightflake_x_botright,
             botrightflake_y_topleft,  botrightflake_y_botright, stimuli = "checkflake_botright") |>
    mark_aoi("center_center", centralflake_x_topleft, centralflake_x_botright,
             centralflake_y_topleft,  centralflake_y_botright, stimuli = "checkflake_center") |>
    mark_aoi("at", at_x_topleft, at_x_botright, at_y_topleft, at_y_botright,
             stimuli = "at") |>
    mark_aoi("top", topobject_x_topleft, topobject_x_botright,
             topobject_y_topleft, topobject_y_botright, stimuli = "objecttop") |>
    mark_aoi("bottom", botobject_x_topleft, botobject_x_botright,
             botobject_y_topleft, botobject_y_botright, stimuli = "objectbottom")
  
  # Identify whether at least one fixation within AOI
  fixation_in_aoi <- df |> 
    drop_na(stimulus) |> 
    filter(Eye.movement.type == "Fixation") |>
    select(Recording.name, Presented.Stimulus.name, aoi, stimulus) |> 
    filter(aoi != "not_in_aoi") |> 
    distinct() |> 
    select(Recording.name, Presented.Stimulus.name) |> 
    mutate(excluded_fixation = "included")
  
  df <- df |> 
    left_join(fixation_in_aoi, by = c("Recording.name", "Presented.Stimulus.name")) |> 
    mutate(excluded_fixation = replace_na(excluded_fixation, "excluded"))
  
  # Write data
  write.table(df, here("exp1", "data", "raw_public_exclude", folder, paste0(sub("\\.tsv$", "", filename), ".txt")), 
              row.names = F, quote = F, sep = "\t", dec = ".")
  print(i)
}
