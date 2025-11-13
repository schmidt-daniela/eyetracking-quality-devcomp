# INCLUSION OF TRIALS
# For a trial to be included, individuals must look in the AOI of the stimulus (see “Other” for details) for at least one fixation. 
# Only trials with latencies of at least 100 ms (does not apply for adults) and not more than 3 standard deviations of each individual 
# mean will be included in the latency analyses (Daum & Gredebäck, 2011).

# OTHER (AOIs)
# We define AOIs as the maximal rectangular dimensions of each stimulus plus 2° per side (Kano & Call, 2014). Tobii Pro Lab requires manual 
# definition of AOIs (and is not based on precise pixel coordinates). This approach may introduce minor deviations from the intended 2° extension.

# For a trial to be included, individuals must look in the AOI of the stimulus (see “Other” for details) 
# for at least one fixation. Only trials with latencies of at least 100 ms and not more than 3 standard 
# deviations of each individual mean will be included in the latency analyses (Daum & Gredebäck, 2011).

# Individuals must provide at least 2 valid trial per screen position (four corners, center, x-axis centered top, 
# x-axis centered bottom) to be included in the final sample. Individuals will be furthermore excluded when 
# technical errors, experimenter errors, or parental interference (infants only) occur.

# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Define AOIs -------------------------------------------------------------
# Remark: AOIs emtail stimulus size + 2°/80px per side

## Objects ----
aoi_topobject_x_topleft <- 790
aoi_topobject_y_topleft <- 0
aoi_topobject_x_botright <- 1130
aoi_topobject_y_botright <- 340

aoi_botobject_x_topleft <- 790
aoi_botobject_y_topleft <- 740
aoi_botobject_x_botright <- 1130
aoi_botobject_y_botright <- 1080

## Popflakes ----
aoi_topleftflake_x_topleft <- 300
aoi_topleftflake_y_topleft <- 90
aoi_topleftflake_x_botright <- 660
aoi_topleftflake_y_botright <- 450

aoi_botleftflake_x_topleft <- 300
aoi_botleftflake_y_topleft <- 630
aoi_botleftflake_x_botright <- 660
aoi_botleftflake_y_botright <- 990

aoi_toprightflake_x_topleft <- 1260
aoi_toprightflake_y_topleft <- 90
aoi_toprightflake_x_botright <- 1620
aoi_toprightflake_y_botright <- 450

aoi_botrightflake_x_topleft <- 1260
aoi_botrightflake_y_topleft <- 630
aoi_botrightflake_x_botright <- 1620
aoi_botrightflake_y_botright <- 990

aoi_centralflake_x_topleft <- 780
aoi_centralflake_y_topleft <- 360
aoi_centralflake_x_botright <- 1140
aoi_centralflake_y_botright <- 720

## Attention Getter ----
aoi_at_x_topleft <- 783
aoi_at_y_topleft <- 363
aoi_at_x_botright <- 1137
aoi_at_y_botright <- 717

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Adjust Parameter --------------------------------------------------------
for(i in c(1:32)){
age_group <- "adult" # 4, 6, 9, 18 or "adult"
filenames <- list.files(path = here("data", "raw_included_1", age_group))
n <- i
filename <- filenames[n]

# Read Data ---------------------------------------------------------------
raw <- read.table(here("data", "raw_included_1", age_group, filename), header = T, sep = "\t")
df <- raw

# Data Preparation --------------------------------------------------------
## Separate information about trials, etc. ----
df <- df |> 
  separate(Presented.Stimulus.name, into = c("trial", "task", "stimulus", "cond_both", "cond_jointness", "cue_direction",
                                             "cond_congruence", "actor", "object_identity", "object_location", "cond_party"), remove = F) |> 
  rename(fix_x = Fixation.point.X, fix_y = Fixation.point.Y)

## Add gaze-sample duration ----
df$gaze_sample_duration <- c(diff(df$Recording.timestamp), NA) / 1000

# Add cumulative duration per stimulus ----
df <- df |> 
  group_by(trial, stimulus) |> 
  mutate(timeline = cumsum(gaze_sample_duration))

## Define AOI ----
df$aoi <- "not_in_aoi"

# Popflakes
topleftflake <- which(df$fix_x >= (aoi_topleftflake_x_topleft) & df$fix_x <= (aoi_topleftflake_x_botright) & 
                        df$fix_y >= (aoi_topleftflake_y_topleft) & df$fix_y <= (aoi_topleftflake_y_botright) &
                        df$stimulus %in% c("checkflake"))
df[topleftflake,"aoi"] <- "top_left"

botleftflake <- which(df$fix_x >= (aoi_botleftflake_x_topleft) & df$fix_x <= (aoi_botleftflake_x_botright) & 
                        df$fix_y >= (aoi_botleftflake_y_topleft) & df$fix_y <= (aoi_botleftflake_y_botright) &
                        df$stimulus %in% c("checkflake"))
df[botleftflake,"aoi"] <- "bot_left"

toprightflake <- which(df$fix_x >= (aoi_toprightflake_x_topleft) & df$fix_x <= (aoi_toprightflake_x_botright) & 
                         df$fix_y >= (aoi_toprightflake_y_topleft) & df$fix_y <= (aoi_toprightflake_y_botright) &
                         df$stimulus %in% c("checkflake"))
df[toprightflake,"aoi"] <- "top_right"

botrightflake <- which(df$fix_x >= (aoi_botrightflake_x_topleft) & df$fix_x <= (aoi_botrightflake_x_botright) & 
                         df$fix_y >= (aoi_botrightflake_y_topleft) & df$fix_y <= (aoi_botrightflake_y_botright) &
                         df$stimulus %in% c("checkflake"))
df[botrightflake,"aoi"] <- "bot_right"

centralflake <- which(df$fix_x >= (aoi_centralflake_x_topleft) & df$fix_x <= (aoi_centralflake_x_botright) & 
                        df$fix_y >= (aoi_centralflake_y_topleft) & df$fix_y <= (aoi_centralflake_y_botright) &
                        df$stimulus %in% c("checkflake"))
df[centralflake,"aoi"] <- "center_center"

# Attention getter
at <- which(df$fix_x >= (aoi_at_x_topleft) & df$fix_x <= (aoi_at_x_botright) & 
              df$fix_y >= (aoi_at_y_topleft) & df$fix_y <= (aoi_at_y_botright) &
              df$stimulus %in% c("at"))
df[at,"aoi"] <- "at"

# Objects
top_object <- which(df$fix_x >= (aoi_topobject_x_topleft) & df$fix_x <= (aoi_topobject_x_botright) & 
                      df$fix_y >= (aoi_topobject_y_topleft) & df$fix_y <= (aoi_topobject_y_botright) &
                      df$stimulus %in% c("move", "still"))
df[top_object,"aoi"] <- "top"

bot_object <- which(df$fix_x >= (aoi_botobject_x_topleft) & df$fix_x <= (aoi_botobject_x_botright) & 
                      df$fix_y >= (aoi_botobject_y_topleft) & df$fix_y <= (aoi_botobject_y_botright) &
                      df$stimulus %in% c("move", "still"))
df[bot_object,"aoi"] <- "bottom"

# Add Information About Excluded Trials -----------------------------------

# Latencies <100ms (based on gaze samples)
# Top object
latencies_top <- df |> 
  ungroup() |> 
  filter(stimulus %in% c("move", "still")) |> 
  filter(object_location == "top" & aoi == "top") |>
  select(Gaze.point.X, Gaze.point.Y, fix_x, fix_y, Validity.left, Validity.right, 
         Presented.Stimulus.name, Eye.movement.type, trial,
         gaze_sample_duration, timeline, aoi) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  mutate(latency = timeline)

latencies_bottom <- df |> 
  ungroup() |> 
  filter(stimulus %in% c("move", "still")) |> 
  filter(object_location == "bottom" & aoi == "bottom") |>
  select(Gaze.point.X, Gaze.point.Y, Validity.left, Validity.right, 
         Presented.Stimulus.name, Eye.movement.type, trial,
         gaze_sample_duration, timeline, aoi) |> 
  group_by(trial) |> 
  slice(1) |> 
  ungroup() |> 
  mutate(latency = timeline)

excluded_trials_100ms <- latencies_top |> 
  bind_rows(latencies_bottom) |> 
  filter(latency < 100) |>
  pull(trial) |> 
  as.numeric()

excluded_trials_100ms

if(age_group == "adult"){
  excluded_trials_100ms <- NULL
}

df$excluded_100ms <- "included"
df[df$trial %in% excluded_trials_100ms & df$stimulus != "at", "excluded_100ms"] <- "excluded"

# Exclude latencies deviating more than 3 standard deviations of each individual mean
mean_latency <- c(latencies_top$latency, latencies_bottom$latency) |> mean(na.rm = T)
twosd_latency <- c(latencies_top$latency, latencies_bottom$latency) |> sd(na.rm = T) * 3

excluded_trials_threesd <- latencies_top |>
  bind_rows(latencies_bottom) |>
  filter(latency > mean_latency + twosd_latency | latency < mean_latency - twosd_latency) |>
  pull(trial) |> 
  as.numeric()

df$excluded_3sd <- "included"
df[df$trial %in% excluded_trials_threesd & df$stimulus != "at", "excluded_3sd"] <- "excluded"

# At least one fixation in AOI
# For a trial to be included, individuals must look in the AOI of the stimulus (see “Other” for details) 
# for at least one fixation.
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

df$excluded_fixation <- "excluded"
df[df$trial_stimulus_location %in% included_fixation_2, "excluded_fixation"] <- "included"

# df |> 
#   ungroup() |> 
#   select(trial_stimulus_location, excluded_100ms, excluded_3sd, excluded_fixation) |> 
#   filter(!(trial_stimulus_location %in% c("_NA", "Eyetracker_NA", "x_NA"))) |> 
#   distinct() |> 
#   View() # I checked first 4M child with gaze replay and excluded_fixation and excluded_3sd and 100ms aligned with video

# Merge With Raw Data -----------------------------------------------------
dat_fin <- raw |> 
  left_join(df |> ungroup() |> select(Presented.Stimulus.name, excluded_100ms, excluded_3sd, excluded_fixation) |> distinct(), 
            by = "Presented.Stimulus.name")

# Write Data --------------------------------------------------------------
write.table(dat_fin, here("data", "raw_included_2", age_group, paste0(sub("\\.tsv$", "", filename), ".txt")), 
            row.names = F, quote = F, sep = "\t", dec = ".")
print(i)
}


# Check 100ms criterion in VP1, 4M (alignes with gaze replay; was suspicious because latency was 8.3ms)
# check_4m_1 <- raw
# top_object <- which(check_4m_1$Gaze.point.X >= (aoi_topobject_x_topleft) & check_4m_1$Gaze.point.X <= (aoi_topobject_x_botright) & 
#                       check_4m_1$Gaze.point.Y >= (aoi_topobject_y_topleft) & check_4m_1$Gaze.point.Y <= (aoi_topobject_y_botright))
# check_4m_1[top_object,"aoi"] <- "top"
# 
# bot_object <- which(check_4m_1$Gaze.point.X >= (aoi_botobject_x_topleft) & check_4m_1$Gaze.point.X <= (aoi_botobject_x_botright) & 
#                       check_4m_1$Gaze.point.Y >= (aoi_botobject_y_topleft) & check_4m_1$Gaze.point.Y <= (aoi_botobject_y_botright))
# check_4m_1[bot_object,"aoi"] <- "bottom"
# 
# check_4m_1 |> 
#   ungroup() |> 
#   filter(str_detect(Presented.Stimulus.name, "14")) |> 
#   filter(!str_detect(Presented.Stimulus.name, "_at_")) |> 
#   select(Gaze.point.X, Gaze.point.Y,  Presented.Stimulus.name, Eye.movement.type, Recording.timestamp, aoi) |> 
#   mutate(sampledur = c(diff(Recording.timestamp)/1000,NA)) |>
#   group_by(Presented.Stimulus.name) |> 
#   mutate(timeline = cumsum(sampledur)) |>
#   View()