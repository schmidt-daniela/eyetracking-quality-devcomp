## This script identifies and interpolates blinks.
## This is needed for the calculation of robustness.
## The identification of eye blink is based on their characteristic of
## having a pronounced drop in the pupillary signal, followed by a full loss of signal. 
# (https://link.springer.com/article/10.3758/s13428-017-1008-1)
## Nov 17 – Daniela Schmidt

# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Adjust Parameter --------------------------------------------------------
for(i in c(1:32)){
age_group <- "adult" # 4, 6, 9, 18 or "adult"
filenames <- list.files(path = here("data", "raw_included_2", age_group))
n <- i
filename <- filenames[n]

# Adjust Parameter --------------------------------------------------------

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

aoi_buffer <- 0 # already considered 80px buffer above

# Read Data ---------------------------------------------------------------
raw <- read.table(here("data", "raw_included_2", age_group, filename), header = T, sep = "\t")
df <- raw

if(str_detect(filename, "Exp1")){trial_overview <- read.table(here("data", "trial_overview_exp1.txt"), header = T, sep = "\t") |> mutate(trial = 1:n())}
if(str_detect(filename, "Exp2")){trial_overview <- read.table(here("data", "trial_overview_exp2.txt"), header = T, sep = "\t") |> mutate(trial = 1:n())}
if(str_detect(filename, "Exp3")){trial_overview <- read.table(here("data", "trial_overview_exp3.txt"), header = T, sep = "\t") |> mutate(trial = 1:n())}
if(str_detect(filename, "Exp4")){trial_overview <- read.table(here("data", "trial_overview_exp4.txt"), header = T, sep = "\t") |> mutate(trial = 1:n())}

# Data Preparation --------------------------------------------------------

## Attention Getter and Objects ----
# Add gaze-sample duration
df$gaze_sample_duration <- c(diff(df$Recording.timestamp), NA) / 1000

# Single case adjustments due to recording error by Tobii
# Tobii did not record these stimuli, even though they were presented (checked gaze replay)
if(filename == "CAJO_25_M_C6_Rec43_Exp1.txt"){
  df[df$Recording.timestamp == 73311412, "Presented.Stimulus.name"] <- "pre_3_checkflake_2s_top_right"
  df[df$Recording.timestamp == 75930224, "Presented.Stimulus.name"] <- "pre_4_checkflake_30s_bot_left"
  df[df$Recording.timestamp == 76202626, "Presented.Stimulus.name"] <- "pre_4_checkflake_2s_bot_left"
  df[df$Recording.timestamp == 73311412, "Presented.Stimulus.name"] <- "pre_5_checkflake_2s_center_center"
  df[df$Recording.timestamp == 73311412, "Presented.Stimulus.name"] <- "pre_5_checkflake_2s_center_center"
}

# Exclude rows and columns, rename columns
df_atobject <- df |> 
  filter(str_detect(Presented.Stimulus.name, "at") |
         str_detect(Presented.Stimulus.name, "move") |
         str_detect(Presented.Stimulus.name, "still")) |> 
  select(Recording.timestamp, Computer.timestamp, Eyetracker.timestamp, gaze_sample_duration, Project.name, Timeline.name, Recording.name,
         Recording.software.version, Recording.resolution.height, Recording.resolution.width, Event,
         Event.value, Gaze.point.X, Gaze.point.Y, Gaze.point.left.X, Gaze.point.left.Y, Gaze.point.right.X, 
         Gaze.point.right.Y, Validity.left, Validity.right, Presented.Stimulus.name, Presented.Media.name,
         Presented.Media.width, Presented.Media.height, Eye.movement.type, Gaze.event.duration, Eye.movement.type.index, 
         Fixation.point.X, Fixation.point.Y, Eye.openness.left, Eye.openness.right, Eye.openness.filtered,
         Pupil.diameter.left, Pupil.diameter.right, Pupil.diameter.filtered,
         excluded_100ms, excluded_3sd, excluded_fixation) |> 
  rename(timestamp_rec = Recording.timestamp, timestamp_comp = Computer.timestamp, timestamp_et = Eyetracker.timestamp, project = Project.name, 
         experiment = Timeline.name, rec_name = Recording.name, tobii_version = Recording.software.version, res_height = Recording.resolution.height, 
         res_width = Recording.resolution.width, event = Event, event_value = Event.value, x_both = Gaze.point.X, 
         y_both = Gaze.point.Y, x_left = Gaze.point.left.X, y_left = Gaze.point.left.Y, x_right = Gaze.point.right.X, 
         y_right = Gaze.point.right.Y, val_left = Validity.left, val_right = Validity.right, stimulus_name = Presented.Stimulus.name, 
         media_name = Presented.Media.name, media_height = Presented.Media.height, media_width = Presented.Media.width, eye_movement_type = Eye.movement.type,
         gaze_event_duration = Gaze.event.duration, eye_movement_type_index = Eye.movement.type.index, x_fix = Fixation.point.X, y_fix = Fixation.point.Y, 
         eye_open_left = Eye.openness.left, eye_open_right = Eye.openness.right, eye_open_filtered = Eye.openness.filtered,
         pupil_left = Pupil.diameter.left, pupil_right = Pupil.diameter.right, pupil_filtered = Pupil.diameter.filtered) |> 
  separate(stimulus_name, into = c("trial_within_task", "task", "stimulus", "cond_both", "cond_jointness", "cue_direction",
                                   "cond_congruence", "actor", "object_identity", "position", "cond_party"), remove = F) |> 
  select(-c(task, cond_both, cond_jointness, cue_direction, cond_congruence, actor, object_identity, cond_party)) |> 
  filter(stimulus_name != "Eyetracker Calibration")

# Add information about stimli
df_atobject[df_atobject$stimulus == "move", "motion"] <- "video"
df_atobject[df_atobject$stimulus == "still", "motion"] <- "image"
df_atobject[df_atobject$stimulus == "at", "motion"] <- "video"
df_atobject[df_atobject$stimulus == "move", "stimulus"] <- "object"
df_atobject[df_atobject$stimulus == "still", "stimulus"] <- "object"
df_atobject[df_atobject$stimulus == "at", "position"] <- "center"
df_atobject$phase <- "mid"

## Popflakes ----
df_popflakes <- df |> 
  filter(str_detect(Presented.Stimulus.name, "checkflake")) |>
  select(Recording.timestamp, Computer.timestamp, Eyetracker.timestamp, gaze_sample_duration, Project.name, Timeline.name, Recording.name,
         Recording.software.version, Recording.resolution.height, Recording.resolution.width, Event,
         Event.value, Gaze.point.X, Gaze.point.Y, Gaze.point.left.X, Gaze.point.left.Y, Gaze.point.right.X, 
         Gaze.point.right.Y, Validity.left, Validity.right, Presented.Stimulus.name, Presented.Media.name,
         Presented.Media.width, Presented.Media.height, Eye.movement.type, Gaze.event.duration, Eye.movement.type.index, 
         Fixation.point.X, Fixation.point.Y, Eye.openness.left, Eye.openness.right, Eye.openness.filtered,
         Pupil.diameter.left, Pupil.diameter.right, Pupil.diameter.filtered,
         excluded_100ms, excluded_3sd, excluded_fixation) |> 
  rename(timestamp_rec = Recording.timestamp, timestamp_comp = Computer.timestamp, timestamp_et = Eyetracker.timestamp, project = Project.name, 
         experiment = Timeline.name, rec_name = Recording.name, tobii_version = Recording.software.version, res_height = Recording.resolution.height, 
         res_width = Recording.resolution.width, event = Event, event_value = Event.value, x_both = Gaze.point.X, 
         y_both = Gaze.point.Y, x_left = Gaze.point.left.X, y_left = Gaze.point.left.Y, x_right = Gaze.point.right.X, 
         y_right = Gaze.point.right.Y, val_left = Validity.left, val_right = Validity.right, stimulus_name = Presented.Stimulus.name, 
         media_name = Presented.Media.name, media_height = Presented.Media.height, media_width = Presented.Media.width, eye_movement_type = Eye.movement.type,
         gaze_event_duration = Gaze.event.duration, eye_movement_type_index = Eye.movement.type.index, x_fix = Fixation.point.X, y_fix = Fixation.point.Y, 
         eye_open_left = Eye.openness.left, eye_open_right = Eye.openness.right, eye_open_filtered = Eye.openness.filtered,
         pupil_left = Pupil.diameter.left, pupil_right = Pupil.diameter.right, pupil_filtered = Pupil.diameter.filtered) |> 
  separate(stimulus_name, into = c("phase", "trial_within_task", "stimulus", "duration", "position1", "position2"), remove = F) |> 
  unite("position", position1:position2) |> 
  filter(stimulus_name != "Eyetracker Calibration")

# Add information about stimli
df_popflakes[df_popflakes$duration == "30s", "motion"] <- "video"
df_popflakes[df_popflakes$duration == "2s", "motion"] <- "image"
df_popflakes$duration <- NULL

## Attention Getter, Objects, and Popflakes ----
# Merge tables
df_tot <- df_atobject |> 
  bind_rows(df_popflakes) |> 
  arrange(timestamp_rec)

# Add trials
if(df_tot |> unite("stimulus_trial_phase", c(stimulus, trial_within_task, phase)) |> select(stimulus_trial_phase) |> distinct() |> nrow() != 79){
  print(paste0("Participant ", filename, " exhibits a trial number deviating from n = 79.")) # 4M Nr. 53 is OK, was aborted after 61 trials
                                                                                             # 6M Nr. 41 is OK, was aborted after 73 trials.
                                                                                             # 9M, Nr. 13 is OK, was aborted after 61 trials.
                                                                                             # 9M, Nr. 8 is OK, was aborted after 57 trials.
                                                                                             # 18M, Nr. 10 is OK, was aborted after 59 trials.
                                                                                             # 18M, Nr. 18 is OK, was aborted after 61 trials.
                                                                                             # 18M, Nr. 35 is OK, was aborted after 55 trials.
                                                                                             # 18M, Nr. 7 is OK, was aborted after 63 trials.
                                                                                             # Adult, Nr. 22 is OK, was aborted after 49 trials.
  }

df_tot <- df_tot |>
  select(stimulus, phase) |>
  distinct()
  unite("stimulus_trial_phase", c(stimulus, trial_within_task, phase), remove = F) |>
  left_join(trial_overview) |>
  select(-stimulus_trial_phase)

df_tot |>
  unite("stimulus_trial_phase", c(stimulus, trial_within_task, phase), remove = F) |> select(stimulus_trial_phase, trial) |> distinct()

# Add cumulative duration per stimulus
df_tot <- df_tot |> 
  group_by(trial, motion) |> 
  mutate(timeline = cumsum(gaze_sample_duration))

## Define AOI ----
df_tot$aoi <- "outside_target_aoi"

# Popflakes
topleftflake <- which(df_tot$x_fix >= (aoi_topleftflake_x_topleft) & df_tot$x_fix <= (aoi_topleftflake_x_botright) & 
                        df_tot$y_fix >= (aoi_topleftflake_y_topleft) & df_tot$y_fix <= (aoi_topleftflake_y_botright) &
                        df_tot$stimulus %in% c("checkflake") & df_tot$position == "top_left")
df_tot[topleftflake,"aoi"] <- "top_left"

botleftflake <- which(df_tot$x_fix >= (aoi_botleftflake_x_topleft) & df_tot$x_fix <= (aoi_botleftflake_x_botright) & 
                        df_tot$y_fix >= (aoi_botleftflake_y_topleft) & df_tot$y_fix <= (aoi_botleftflake_y_botright) &
                        df_tot$stimulus %in% c("checkflake") & df_tot$position == "bot_left")
df_tot[botleftflake,"aoi"] <- "bot_left"

toprightflake <- which(df_tot$x_fix >= (aoi_toprightflake_x_topleft) & df_tot$x_fix <= (aoi_toprightflake_x_botright) & 
                         df_tot$y_fix >= (aoi_toprightflake_y_topleft) & df_tot$y_fix <= (aoi_toprightflake_y_botright) &
                         df_tot$stimulus %in% c("checkflake") & df_tot$position == "top_right")
df_tot[toprightflake,"aoi"] <- "top_right"

botrightflake <- which(df_tot$x_fix >= (aoi_botrightflake_x_topleft) & df_tot$x_fix <= (aoi_botrightflake_x_botright) & 
                         df_tot$y_fix >= (aoi_botrightflake_y_topleft) & df_tot$y_fix <= (aoi_botrightflake_y_botright) &
                         df_tot$stimulus %in% c("checkflake") & df_tot$position == "bot_right")
df_tot[botrightflake,"aoi"] <- "bot_right"

centralflake <- which(df_tot$x_fix >= (aoi_centralflake_x_topleft) & df_tot$x_fix <= (aoi_centralflake_x_botright) & 
                        df_tot$y_fix >= (aoi_centralflake_y_topleft) & df_tot$y_fix <= (aoi_centralflake_y_botright) &
                        df_tot$stimulus %in% c("checkflake") & df_tot$position == "center_center")
df_tot[centralflake,"aoi"] <- "center_center"

# Attention getter
at <- which(df_tot$x_fix >= (aoi_at_x_topleft) & df_tot$x_fix <= (aoi_at_x_botright) & 
              df_tot$y_fix >= (aoi_at_y_topleft) & df_tot$y_fix <= (aoi_at_y_botright) &
              df_tot$stimulus %in% c("at"))
df_tot[at,"aoi"] <- "at"

# Objects
top_object <- which(df_tot$x_fix >= (aoi_topobject_x_topleft) & df_tot$x_fix <= (aoi_topobject_x_botright) & 
                      df_tot$y_fix >= (aoi_topobject_y_topleft) & df_tot$y_fix <= (aoi_topobject_y_botright) &
                      df_tot$stimulus == "object" & df_tot$motion %in% c("image", "video") & df_tot$position == "top")
df_tot[top_object,"aoi"] <- "top"

bot_object <- which(df_tot$x_fix >= (aoi_botobject_x_topleft) & df_tot$x_fix <= (aoi_botobject_x_botright) & 
                      df_tot$y_fix >= (aoi_botobject_y_topleft) & df_tot$y_fix <= (aoi_botobject_y_botright) &
                      df_tot$stimulus == "object" & df_tot$motion %in% c("image", "video") & df_tot$position == "bottom")
df_tot[bot_object,"aoi"] <- "bottom"

# Potential check (does it align with gaze replay?)
# Check, e.g., adult 13 exhibits more blinks during attention getter in first 1.5 blocks compared to objects; this is visible in the data
# df_tot |>
#   select(trial, motion, gaze_sample_duration, stimulus, timeline, position, aoi, x_fix, y_fix) |>
#   View()

## Define AOI Gaze Samples ----
df_tot$aoi_gazesamples <- "outside_target_aoi"

# Popflakes
topleftflake <- which(df_tot$x_both >= (aoi_topleftflake_x_topleft) & df_tot$x_both <= (aoi_topleftflake_x_botright) & 
                        df_tot$y_both >= (aoi_topleftflake_y_topleft) & df_tot$y_both <= (aoi_topleftflake_y_botright) &
                        df_tot$stimulus %in% c("checkflake") & df_tot$position == "top_left")
df_tot[topleftflake,"aoi_gazesamples"] <- "top_left"

botleftflake <- which(df_tot$x_both >= (aoi_botleftflake_x_topleft) & df_tot$x_both <= (aoi_botleftflake_x_botright) & 
                        df_tot$y_both >= (aoi_botleftflake_y_topleft) & df_tot$y_both <= (aoi_botleftflake_y_botright) &
                        df_tot$stimulus %in% c("checkflake") & df_tot$position == "bot_left")
df_tot[botleftflake,"aoi_gazesamples"] <- "bot_left"

toprightflake <- which(df_tot$x_both >= (aoi_toprightflake_x_topleft) & df_tot$x_both <= (aoi_toprightflake_x_botright) & 
                         df_tot$y_both >= (aoi_toprightflake_y_topleft) & df_tot$y_both <= (aoi_toprightflake_y_botright) &
                         df_tot$stimulus %in% c("checkflake") & df_tot$position == "top_right")
df_tot[toprightflake,"aoi_gazesamples"] <- "top_right"

botrightflake <- which(df_tot$x_both >= (aoi_botrightflake_x_topleft) & df_tot$x_both <= (aoi_botrightflake_x_botright) & 
                         df_tot$y_both >= (aoi_botrightflake_y_topleft) & df_tot$y_both <= (aoi_botrightflake_y_botright) &
                         df_tot$stimulus %in% c("checkflake") & df_tot$position == "bot_right")
df_tot[botrightflake,"aoi_gazesamples"] <- "bot_right"

centralflake <- which(df_tot$x_both >= (aoi_centralflake_x_topleft) & df_tot$x_both <= (aoi_centralflake_x_botright) & 
                        df_tot$y_both >= (aoi_centralflake_y_topleft) & df_tot$y_both <= (aoi_centralflake_y_botright) &
                        df_tot$stimulus %in% c("checkflake") & df_tot$position == "center_center")
df_tot[centralflake,"aoi_gazesamples"] <- "center_center"

# Attention getter
at <- which(df_tot$x_both >= (aoi_at_x_topleft) & df_tot$x_both <= (aoi_at_x_botright) & 
              df_tot$y_both >= (aoi_at_y_topleft) & df_tot$y_both <= (aoi_at_y_botright) &
              df_tot$stimulus %in% c("at"))
df_tot[at,"aoi_gazesamples"] <- "at"

# Objects
top_object <- which(df_tot$x_both >= (aoi_topobject_x_topleft) & df_tot$x_both <= (aoi_topobject_x_botright) & 
                      df_tot$y_both >= (aoi_topobject_y_topleft) & df_tot$y_both <= (aoi_topobject_y_botright) &
                      df_tot$stimulus == "object" & df_tot$motion %in% c("image", "video") & df_tot$position == "top")
df_tot[top_object,"aoi_gazesamples"] <- "top"

bot_object <- which(df_tot$x_both >= (aoi_botobject_x_topleft) & df_tot$x_both <= (aoi_botobject_x_botright) & 
                      df_tot$y_both >= (aoi_botobject_y_topleft) & df_tot$y_both <= (aoi_botobject_y_botright) &
                      df_tot$stimulus == "object" & df_tot$motion %in% c("image", "video") & df_tot$position == "bottom")
df_tot[bot_object,"aoi_gazesamples"] <- "bottom"

## Calibration ----
df_calibration <- df |> 
  select(Recording.name, Timeline.name, matches("validation|calibration", ignore.case = TRUE)) |> 
  distinct() |> 
  rename(rec_name = Recording.name, 
         experiment = Timeline.name, 
         cal_acc_mm = Average.calibration.accuracy..mm., 
         cal_precsd_mm = Average.calibration.precision.SD..mm.,
         cal_precrms_mm = Average.calibration.precision.RMS..mm., 
         cal_acc_deg = Average.calibration.accuracy..degrees.,
         cal_precsd_deg = Average.calibration.precision.SD..degrees., 
         cal_precrms_deg = Average.calibration.precision.RMS..degrees.,
         cal_acc_px = Average.calibration.accuracy..pixels., 
         cal_precsd_px = Average.calibration.precision.SD..pixels.,
         cal_precrms_px = Average.calibration.precision.RMS..pixels., 
         val_acc_mm = Average.validation.accuracy..mm.,
         val_precsd_mm = Average.validation.precision.SD..mm., 
         val_precrms_mm = Average.validation.precision.RMS..mm.,
         val_acc_deg = Average.validation.accuracy..degrees., 
         val_precsd_deg = Average.validation.precision.SD..degrees.,
         val_precrms_deg = Average.validation.precision.RMS..degrees., 
         val_acc_px = Average.validation.accuracy..pixels.,
         val_precsd_px = Average.validation.precision.SD..pixels., 
         val_precrms_px = Average.validation.precision.RMS..pixels.)

# Write Data --------------------------------------------------------------
write.table(df_tot, here("data", "preproc_included_1", age_group, paste0("preproc1_", age_group, "_", sub("\\.txt$", "", filename), ".txt")), 
            row.names = F, quote = F, sep = "\t", dec = ".")
write.table(df_calibration, here("data", "calibration_tobii", age_group, paste0("calibration_tobii_", age_group, "_", sub("\\.tsv$", "", filename), ".txt")), 
            row.names = F, quote = F, sep = "\t", dec = ".")

# Notes -------------------------------------------------------------------
# Plot Eye Openess (might be interesting for blink detection)
# df_half <- df[c(15000:18000),]
# plot(x = df_half$Recording.timestamp, y = df_half$Eye.openness.left)
}
