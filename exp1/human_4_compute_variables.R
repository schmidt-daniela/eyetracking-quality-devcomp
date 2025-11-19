# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Load Functions ----------------------------------------------------------
source(here("fun", "calc_etdq.R"))
source(here("fun", "calc_etdm.R"))
source(here("fun", "onepx_in_visd.R"))

# Adjust Parameter --------------------------------------------------------
for(i in c(1:32)){
age_group <- "adult" # 4, 6, 9, 18 or "adult"
filenames <- list.files(path = here("data", "preproc_included_1", age_group))
n <- i
filename <- filenames[n]

# Read Data ---------------------------------------------------------------
raw <- read.table(here("data", "preproc_included_1", age_group, filename), header = T, sep = "\t") |> mutate(filename = filename)
df <- raw

if(df |> drop_na(excluded_fixation) |> pull(excluded_fixation) |> unique() |> length() > 1){
  df <- df |> filter(excluded_fixation == "included")
  print("Trials without at least 1 fixation in target AOI were excluded.")
}

# Add Fixation Duration ---------------------------------------------------
# Why? Because gaze_event_duration refers to the duration of a fixation, irrespective of
# whether the duration was within one trial or across two trials. The analyses, however,
# are conducted on a trial level. Therefore, we need the fixation duration in one trial
# and cut off the duration of the fixation in a previous or subsequent trial.
df <- df |> 
  group_by(trial, eye_movement_type_index, eye_movement_type) |> 
  mutate(gaze_event_duration_revised = cumsum(gaze_sample_duration)) |> 
  ungroup()

# Manual Check (scroll down until transition between trial n & trial n+1)
# df |> 
#   group_by(trial, eye_movement_type_index, eye_movement_type) |> 
#   mutate(gaze_event_duration_revised = cumsum(gaze_sample_duration)) |> 
#   ungroup() |> 
#   select(trial, eye_movement_type_index, eye_movement_type, gaze_sample_duration, gaze_event_duration_revised) |> 
#   View() # Check

# Add Time in Experiment --------------------------------------------------
# Not used in the following.
# However, "time_in_experiment" can be selected within the functions potentially,
# if we consider time not as trials, but as time in the experiment.
df <- df |> 
  mutate(time_in_experiment = cumsum(gaze_sample_duration))

# [1] Calculate Data Quality ----------------------------------------------

## [1.1] Accuracy ----
# We define data accuracy as the Euclidean distance between gaze location and center of the target (Dalrymple et al., 2018). 
# We will measure accuracy during the fixation whose gaze coordinates are (compared to all fixations of this trials) closed 
# to the center of the displayed target. Fixations will be classified by applying an eye-tracking filter algorithm. We only 
# consider fixations within pre-specified areas of interests (AOIs) (see “Other” for details). Lower values resulting from 
# this calculation will be interpreted in terms of better accuracy. Accuracy values are bounded between 0 (gaze location 
# equals the center of the target) and positive infinity.

### Attention Getter ----
df_acc_at <- calculate_accuracy(df |> filter(stimulus == "at"), 
                   xmin = 783, xmax = 1137, ymin = 363, ymax = 717, stimulus_vec = "at",
                   media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename",
                   gaze_event_index_col = "eye_movement_type_index", x_fix = "x_fix", y_fix = "y_fix",
                   stimulus_height = 354, stimulus_width = 354, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "at") |> 
  mutate(position = "center")

# Manual Check: Accuracy, 4M, 1, AT, Trial 6
# df |> 
#   filter(stimulus == "at" & trial == 6 & eye_movement_type == "Fixation") |> 
#   select(x_fix, y_fix) |> 
#   distinct()
# 
# fixation1 <- sqrt((927-960)^2 + (516-540)^2)
# fixation2 <- sqrt((911-960)^2 + (521-540)^2)
# fixation3 <- sqrt((950-960)^2 + (516-540)^2)
# fixation4 <- sqrt((928-960)^2 + (495-540)^2)
# fixation5 <- sqrt((936-960)^2 + (494-540)^2)
# fixation6 <- sqrt((939-960)^2 + (496-540)^2)
# fixation7 <- sqrt((941-960)^2 + (509-540)^2)
# fixation8 <- sqrt((944-960)^2 + (496-540)^2)
# fixation9 <- sqrt((948-960)^2 + (513-540)^2)
# fixation10 <- sqrt((950-960)^2 + (487-540)^2)
# fixation11 <- sqrt((953-960)^2 + (475-540)^2)
# fixation12 <- sqrt((950-960)^2 + (497-540)^2)
# min(fixation1, fixation2, fixation3, fixation4, fixation5, fixation6, fixation7, fixation8, fixation9, fixation10, fixation11, fixation12) # alignes with output of function: 26px
# 
# calculate_accuracy(df, xmin = 849, xmax = 1072, ymin = 417, ymax = 672, stimulus_vec = c("ATTENTION_Familiarization.mp4", "ATTENTION_Preflooking.mp4"),
#                    media_col = "Presented.Media.name", gaze_event_col = "Eye.movement.type", id_col = "Recording.name",
#                    gaze_event_index_col = "Eye.movement.type.index", x_fix = "Fixation.point.X", y_fix = "Fixation.point.Y",
#                    stimulus_height = 255, stimulus_width = 223, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80)

### Top Object ----
df_acc_objtop <- calculate_accuracy(df |> filter(stimulus == "object" & position == "top"), 
                             xmin = 790, xmax = 1130, ymin = 0, ymax = 340, stimulus_vec = "object",
                             media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename",
                             gaze_event_index_col = "eye_movement_type_index", x_fix = "x_fix", y_fix = "y_fix",
                             stimulus_height = 340, stimulus_width = 340, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "object") |> 
  mutate(position = "top")

### Bottom Object ----
df_acc_objbot <- calculate_accuracy(df |> filter(stimulus == "object" & position == "bottom"), 
                                    xmin = 790, xmax = 1130, ymin = 740, ymax = 1080, stimulus_vec = "object",
                                    media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename",
                                    gaze_event_index_col = "eye_movement_type_index", x_fix = "x_fix", y_fix = "y_fix",
                                    stimulus_height = 340, stimulus_width = 340, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "object") |> 
  mutate(position = "bottom")

### Popflake Top Left ----
df_acc_poptopleft <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "top_left"), 
                                    xmin = 300, xmax = 660, ymin = 90, ymax = 450, stimulus_vec = "checkflake",
                                    media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename",
                                    gaze_event_index_col = "eye_movement_type_index", x_fix = "x_fix", y_fix = "y_fix",
                                    stimulus_height = 360, stimulus_width = 360, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "top_left")

### Popflake Top Right ----
df_acc_poptopright <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "top_right"), 
                                         xmin = 1260, xmax = 1620, ymin = 90, ymax = 450, stimulus_vec = "checkflake",
                                         media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename",
                                         gaze_event_index_col = "eye_movement_type_index", x_fix = "x_fix", y_fix = "y_fix",
                                         stimulus_height = 360, stimulus_width = 360, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "top_right")

### Popflake Bottom Left ----
df_acc_popbotleft <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "bot_left"), 
                                        xmin = 300, xmax = 660, ymin = 630, ymax = 990, stimulus_vec = "checkflake",
                                        media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename",
                                        gaze_event_index_col = "eye_movement_type_index", x_fix = "x_fix", y_fix = "y_fix",
                                        stimulus_height = 360, stimulus_width = 360, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "bot_left")

### Popflake Bottom Right ----
df_acc_popbotright <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "bot_right"), 
                                        xmin = 1260, xmax = 1620, ymin = 630, ymax = 990, stimulus_vec = "checkflake",
                                        media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename",
                                        gaze_event_index_col = "eye_movement_type_index", x_fix = "x_fix", y_fix = "y_fix",
                                        stimulus_height = 360, stimulus_width = 360, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "bot_right")

### Popflake Center ----
df_acc_popcenter <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "center_center"), 
                                        xmin = 780, xmax = 1140, ymin = 360, ymax = 720, stimulus_vec = "checkflake",
                                        media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename",
                                        gaze_event_index_col = "eye_movement_type_index", x_fix = "x_fix", y_fix = "y_fix",
                                        stimulus_height = 360, stimulus_width = 360, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "center_center")

## Merge All Stimuli ----
df_acc_tot <- df_acc_at |>
  bind_rows(df_acc_objtop, df_acc_objbot,
            df_acc_poptopleft, df_acc_poptopright, df_acc_popbotleft, df_acc_popbotright, df_acc_popcenter) |> 
  mutate(filename = str_replace(filename, ".txt", "")) |> 
  mutate(data_quality = "accuracy")

rm(df_acc_at, df_acc_objbot, df_acc_objtop, df_acc_popbotleft, df_acc_popbotright, df_acc_popcenter, df_acc_poptopleft, df_acc_poptopright)

## [1.2] Precision RMS ----
# We will calculate data precision within fixations as the standard deviation and the root mean square of the Euclidean distances 
# between subsequent gaze samples (Dalrymple et al., 2018; Holmqvist et al., 2011, 2012). Fixations will be classified by applying 
# an eye-tracking filter algorithm. We only consider on-screen fixations. Lower values resulting from this calculation will be 
# interpreted in terms of higher precision. Precision values are bounded between 0 (no variation in gaze coordinates within a fixation) 
# and positive infinity. 
# Formula: https://dl.acm.org/doi/pdf/10.1145/2168556.2168563

## Attention Getter ----
df_precrms_at <- calculate_precision_rms(df |> filter(stimulus == "at"), 
                                         media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename", 
                                         stimulus_vec = "at", gaze_event_index_col = "eye_movement_type_index", gaze_event_dur_col = "gaze_event_duration_revised",
                                         x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                         screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80,
                                         xmin = 863, xmax = 1057, ymin = 443, ymax = 637,
                                         off_exclude = T, longest_fix_only = F, AOI_only = T) |> 
  mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "at") |> 
  mutate(position = "center")

# Manual Check: Precision RMS, Adult, 1, AT, Trial 6
# library(png)
# img_precrms <- readPNG(here("img", "precrms_formular.png"))
# plot(1:2, type = "n", xlab = "", ylab = "", axes = FALSE)  # create empty plot
# rasterImage(img_precrms, 1, 1, 1.9, 1.5) # image boundaries
# 
# precrms_test <- df |>
#   filter(trial == 6 & stimulus == "at" & eye_movement_type == "Fixation") |>
#   select(x_both, y_both, x_fix, y_fix) |>
#   mutate(x_diff = c(diff(x_both), NA),
#          y_diff = c(diff(y_both), NA)) |>
#   mutate(eucl_diff = sqrt(x_diff^2 + y_diff^2)) |>
#   mutate(eucl_diff_2 = eucl_diff^2)
# mean(precrms_test$eucl_diff_2, na.rm = T) |> sqrt() # hooray, same result as with function applied above, i.e.: check!

## Popflakes and Objects ----
param_precrms_objpop <- data.frame(
  stimulus = c("object", "object", "checkflake", "checkflake", "checkflake", "checkflake", "checkflake"),
  position = c("top", "bottom", "top_left", "bot_left", "top_right", "bot_right", "center_center"),
  xmin = c(870, 870, 380, 380, 1340, 1340, 860),
  xmax = c(1050, 1050, 580, 580, 1540, 1540, 1060),
  ymin = c(80, 820, 170, 710, 170, 710, 440),
  ymax = c(260, 1000, 370, 910, 370, 910, 640),
  df_name = c("df_precrms_objtop", "df_precrms_objbot",
              "df_precrms_poptopleft", "df_precrms_popbotleft",
              "df_precrms_poptopright", "df_precrms_popbotright", "df_precrms_popcenter"))

for(j in c(1:7)){
  df_precrms_objpop_temp <- calculate_precision_rms(df |> filter(stimulus == param_precrms_objpop$stimulus[j] & position == param_precrms_objpop$position[j]), 
                                               media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename", 
                                               stimulus_vec = param_precrms_objpop$stimulus[j], 
                                               gaze_event_index_col = "eye_movement_type_index", gaze_event_dur_col = "gaze_event_duration_revised",
                                               x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                               screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80,
                                               xmin = param_precrms_objpop$xmin[j], xmax = param_precrms_objpop$xmax[j],
                                               ymin = param_precrms_objpop$ymin[j], ymax = param_precrms_objpop$ymax[j],
                                               off_exclude = T, longest_fix_only = F, AOI_only = T) |> 
    mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |>
    left_join(df |> select(stimulus, position, trial) |> distinct(), by = "trial")
  
  assign(param_precrms_objpop$df_name[j], df_precrms_objpop_temp)
  rm(df_precrms_objpop_temp)
}
# Check for first 4M (only fixations within AOI taken)
# df_precrms_popbotright
# df |> 
#   filter(eye_movement_type_index %in% c(1020:1029)) |> 
#   select(eye_movement_type, eye_movement_type_index, x_fix, y_fix, aoi)

## Merge All Stimuli ----
df_precrms_tot <- df_precrms_at |>
  bind_rows(df_precrms_objtop, df_precrms_objbot, df_precrms_poptopleft, df_precrms_popbotleft,
            df_precrms_poptopright, df_precrms_popbotright, df_precrms_popcenter) |> 
  mutate(filename = str_replace(filename, ".txt", "")) |> 
  mutate(data_quality = "precisionrms")

rm(df_precrms_at, df_precrms_objtop, df_precrms_objbot, df_precrms_poptopleft, df_precrms_popbotleft,
   df_precrms_poptopright, df_precrms_popbotright, df_precrms_popcenter)

## [1.3] Precision SD ----
# We will calculate data precision within fixations as the standard deviation and the root mean square of the Euclidean distances 
# between subsequent gaze samples (Dalrymple et al., 2018; Holmqvist et al., 2011, 2012). Fixations will be classified by applying 
# an eye-tracking filter algorithm. We only consider on-screen fixations. Lower values resulting from this calculation will be 
# interpreted in terms of higher precision. Precision values are bounded between 0 (no variation in gaze coordinates within a fixation) 
# and positive infinity.

## Attention Getter ----
df_precsd_at <- calculate_precision_sd(df |> filter(stimulus == "at"), 
                                       media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename", 
                                       stimulus_vec = "at", gaze_event_index_col = "eye_movement_type_index", gaze_event_dur_col = "gaze_event_duration_revised",
                                       x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                       screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80,
                                       xmin = 863, xmax = 1057, ymin = 443, ymax = 637,
                                       off_exclude = T, longest_fix_only = F, AOI_only = T) |> 
  mutate(precsd_visd = precsd * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "at") |> 
  mutate(position = "center")

# Manual Check: Precision RMS, 18, 1, AT, Trial 6
# df |>
#   filter(trial == 6 & stimulus == "at" & eye_movement_type == "Fixation") |>
#   select(x_both, y_both, eye_movement_type_index) |> 
#   group_by(eye_movement_type_index) |> 
#   mutate(x_diff = c(diff(x_both), NA),
#          y_diff = c(diff(y_both), NA)) |>
#   mutate(eucl_diff = sqrt(x_diff^2 + y_diff^2)) |> 
#   summarize(rmssd = sd(eucl_diff, na.rm = T))  # hooray, same result as with function applied above, i.e.: check!

## Popflakes and Objects ----
param_precsd_objpop <- data.frame(
  stimulus = c("object", "object", "checkflake", "checkflake", "checkflake", "checkflake", "checkflake"),
  position = c("top", "bottom", "top_left", "bot_left", "top_right", "bot_right", "center_center"),
  xmin = c(870, 870, 380, 380, 1340, 1340, 860),
  xmax = c(1050, 1050, 580, 580, 1540, 1540, 1060),
  ymin = c(80, 820, 170, 710, 170, 710, 440),
  ymax = c(260, 1000, 370, 910, 370, 910, 640),
  df_name = c("df_precsd_objtop", "df_precsd_objbot",
              "df_precsd_poptopleft", "df_precsd_popbotleft",
              "df_precsd_poptopright", "df_precsd_popbotright", "df_precsd_popcenter"))

for(k in c(1:7)){
  df_precsd_objpop_temp <- calculate_precision_sd(df |> filter(stimulus == param_precsd_objpop$stimulus[k] & position == param_precsd_objpop$position[k]), 
                                                  media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "filename", 
                                                  stimulus_vec = param_precsd_objpop$stimulus[k], 
                                                  gaze_event_index_col = "eye_movement_type_index", gaze_event_dur_col = "gaze_event_duration_revised",
                                                  x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                                  screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80,
                                                  xmin = param_precsd_objpop$xmin[k], xmax = param_precsd_objpop$xmax[k],
                                                  ymin = param_precsd_objpop$ymin[k], ymax = param_precsd_objpop$ymax[k],
                                                  off_exclude = T, longest_fix_only = F, AOI_only = T) |> 
    mutate(precsd_visd = precsd * onepx_in_visd(60, 92)) |>
    left_join(df |> select(stimulus, position, trial) |> distinct(), by = "trial")
  
  assign(param_precsd_objpop$df_name[k], df_precsd_objpop_temp)
  rm(df_precsd_objpop_temp)
}

## Merge All Stimuli ----
df_precsd_tot <- df_precsd_at |>
  bind_rows(df_precsd_objtop, df_precsd_objbot, df_precsd_poptopleft, df_precsd_popbotleft,
            df_precsd_poptopright, df_precsd_popbotright, df_precsd_popcenter) |> 
  mutate(filename = str_replace(filename, ".txt", "")) |> 
  mutate(data_quality = "precisionsd")

rm(df_precsd_at, df_precsd_objtop, df_precsd_objbot, df_precsd_poptopleft, df_precsd_popbotleft,
   df_precsd_poptopright, df_precsd_popbotright, df_precsd_popcenter)

## [1.4] Robustness ----
# We will calculate data robustness as the duration of the mean usable data fragments obtained after interpolating blinks. The greater the 
# mean duration of usable data, the more robust the data. Robustness values are bounded between 0 (no usable data) and the total recording 
# duration considered.

# [2] Calculate Eye-Tracking Outcomes -------------------------------------
# We consider the following common eye-tracking measures as dependent variables: fixation durations, number of fixations, latencies (i.e., 
# the time between stimulus onset and first gaze point in AOI), and proportional looking time in AOI (relative to looking time on screen)

## Fixation Duration ----
df_fixdur_tot <- calculate_fixdur(df, 
                                  gaze_event_col = "eye_movement_type", media_col = "stimulus",
                                  stimulus_vec = c("checkflake", "at", "object"),
                                  id_col = "filename", gaze_event_index_col = "eye_movement_type_index", 
                                  gaze_event_dur_col = "gaze_event_duration_revised", x_fix = "x_fix", y_fix = "y_fix",
                                  screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80,
                                  off_exclude = T, longest_fix_only = F) |> 
  left_join(df |> select(stimulus, position, trial) |> distinct(), by = "trial") |> 
  mutate(eyetracking_outcome = "fixationduration")

# Manual Check: Fixation Duration, Adult, 1, Trial 1
# df |>
#   filter(trial == 1 & eye_movement_type == "Fixation") |>
#   select(trial, eye_movement_type, eye_movement_type_index, gaze_event_duration_revised) |> 
#   distinct() |> 
#   pull(gaze_event_duration_revised) |> 
#   mean() # hooray, same result as with function applied above, i.e.: check!

## Number of Fixations ----
df_fixnum_tot <- calculate_fixnum(df, 
                                  gaze_event_col = "eye_movement_type", media_col = "stimulus",
                                  stimulus_vec = c("checkflake", "at", "object"),
                                  id_col = "filename", gaze_event_index_col = "eye_movement_type_index", 
                                  gaze_event_dur_col = "gaze_event_duration_revised", x_fix = "x_fix", y_fix = "y_fix",
                                  screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80,
                                  off_exclude = T)

# Manual Check: Fixation Number, Adult, 1, Trial 1
# df |>
#   filter(trial == 1 & eye_movement_type == "Fixation") |>
#   select(trial, eye_movement_type, eye_movement_type_index) |>
#   distinct() |>
#   nrow() # hooray, same result as with function applied above, i.e.: check!

df_fixnum_tot <- df_fixnum_tot |> 
  left_join(df |> select(stimulus, position, trial) |> distinct(), by = "trial") |> 
  mutate(eyetracking_outcome = "fixationnumber") |> 
  rename(mean_fixation_number = n)

## Latencies ----
df_lat <- df

if(df |> drop_na(excluded_100ms) |> pull(excluded_100ms) |> unique() |> length() > 1 & age_group != "adult"){
  df_lat <- df_lat |> filter(excluded_100ms == "included")
  print("<100ms latency trials were excluded.")
}

if(df_lat |> drop_na(excluded_3sd) |> pull(excluded_3sd) |> unique() |> length() > 1){
  df_lat <- df_lat |> filter(excluded_3sd == "included")
  print(">3sd latency trials were excluded.")
}

first_sample_in_trial <- df_lat |> 
  select(stimulus_name, stimulus, timestamp_rec, aoi_gazesamples) |> 
  group_by(stimulus_name) |> 
  slice(1) |> 
  ungroup() |> 
  rename(trialstart = timestamp_rec) |> 
  select(-aoi_gazesamples)

first_sample_in_aoi <- df_lat |> 
  select(stimulus_name, timestamp_rec, aoi_gazesamples) |> 
  filter(aoi_gazesamples != "outside_target_aoi") |> 
  group_by(stimulus_name) |> 
  slice(1) |> 
  ungroup() |> 
  rename(arrivalinaoi = timestamp_rec) |> 
  select(-aoi_gazesamples)

latencies <- first_sample_in_trial |> 
  left_join(first_sample_in_aoi, by = "stimulus_name") |> 
  mutate(latencies = (arrivalinaoi - trialstart) / 1000) # unit: ms

df_latencies_tot <- df_lat |> 
  select(filename, trial, stimulus_name, position, motion) |> 
  left_join(latencies, by = "stimulus_name") |> 
  select(-c(trialstart, arrivalinaoi)) |> 
  distinct() |> 
  filter(motion != "image") |>  # checked 4M, 1, first trials, alignes with gaze replay
  mutate(eyetracking_outcome = "latency")

rm(first_sample_in_trial, first_sample_in_aoi, latencies)

## Proportional Looking Time in AOI ----
### Attention Getter ----
df_rlt_at <- calculate_ltaoi(df |> filter(stimulus == "at"), media_col = "stimulus", stimulus_vec = "at",
                            rectime = "timestamp_rec", id_col = "filename", 
                            x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                            aoi_left_upper = c(783, 363), aoi_right_lower = c(1137, 717), is_00_upleftcorner = T) |>  # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
  map(~ mutate(.x, stimulus = "at", position = "center", eyetracking_outcome = "relativelookingtime"))

# Manual Check: Adult, 1, AT, Trial 10
# df |> 
#   filter(trial == 10 & aoi == "at") |> 
#   select(x_both, y_both, x_fix, y_fix, aoi, aoi_gazesamples, gaze_sample_duration, gaze_event_duration) |> 
#   nrow() * 8.33 # Check, alignes with df_rlt_at, column abs_fix_in_aoi_duration

# df |>
#   filter(trial == 10 & aoi_gazesamples == "at") |>
#   select(x_both, y_both, x_fix, y_fix, aoi, aoi_gazesamples, gaze_sample_duration, gaze_event_duration) |>
#   nrow() * 8.33 # Check, alignes with df_rlt_at, column abs_gaze_in_aoi_duration

### Top Object ----
df_rlt_objtop <- calculate_ltaoi(df |> filter(stimulus == "object" & position == "top"), media_col = "stimulus", stimulus_vec = "object",
                                 rectime = "timestamp_rec", id_col = "filename", 
                                 x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                 aoi_left_upper = c(790, 0), aoi_right_lower = c(1130, 340), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
  map(~ mutate(.x, stimulus = "object", position = "top", eyetracking_outcome = "relativelookingtime"))

# Manual Check: Adult, Object Top, Prelast (Trial 64)
# 100% gaze at object, validated in gaze replay

### Bottom Object ----
df_rlt_objbot <- calculate_ltaoi(df |> filter(stimulus == "object" & position == "bottom"), media_col = "stimulus", stimulus_vec = "object",
                                 rectime = "timestamp_rec", id_col = "filename", 
                                 x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                 aoi_left_upper = c(790, 740), aoi_right_lower = c(1130, 1080), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
  map(~ mutate(.x, stimulus = "object", position = "bottom", eyetracking_outcome = "relativelookingtime"))

### Popflake Top Left ----
df_rlt_poptopleft <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "top_left"), media_col = "stimulus", stimulus_vec = "checkflake",
                                     rectime = "timestamp_rec", id_col = "filename", 
                                     x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                     aoi_left_upper = c(300, 90), aoi_right_lower = c(660, 450), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
  map(~ mutate(.x, stimulus = "checkflake", position = "top_left", eyetracking_outcome = "relativelookingtime"))

### Popflake Top Right ----
df_rlt_poptopright <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "top_right"), media_col = "stimulus", stimulus_vec = "checkflake",
                                     rectime = "timestamp_rec", id_col = "filename", 
                                     x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                     aoi_left_upper = c(1260, 90), aoi_right_lower = c(1620, 450), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
  map(~ mutate(.x, stimulus = "checkflake", position = "top_right", eyetracking_outcome = "relativelookingtime"))

### Popflake Bottom Left ----
df_rlt_popbotleft <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "bot_left"), media_col = "stimulus", stimulus_vec = "checkflake",
                                      rectime = "timestamp_rec", id_col = "filename", 
                                      x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                      aoi_left_upper = c(300, 630), aoi_right_lower = c(660, 990), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
  map(~ mutate(.x, stimulus = "checkflake", position = "bot_left", eyetracking_outcome = "relativelookingtime"))

### Popflake Bottom Right ----
df_rlt_popbotright <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "bot_right"), media_col = "stimulus", stimulus_vec = "checkflake",
                                     rectime = "timestamp_rec", id_col = "filename", 
                                     x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                     aoi_left_upper = c(1260, 630), aoi_right_lower = c(1620, 990), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
  map(~ mutate(.x, stimulus = "checkflake", position = "bot_right", eyetracking_outcome = "relativelookingtime"))

### Popflake Center ----
df_rlt_popcenter <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "center_center"), media_col = "stimulus", stimulus_vec = "checkflake",
                                      rectime = "timestamp_rec", id_col = "filename", 
                                      x_fix = "x_fix", y_fix = "y_fix", x = "x_both", y = "y_both", 
                                      aoi_left_upper = c(780, 360), aoi_right_lower = c(1140, 720), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
  map(~ mutate(.x, stimulus = "checkflake", position = "center_center", eyetracking_outcome = "relativelookingtime"))

### Merge All Stimuli ----
df_rlt_gaze_tot <- df_rlt_at[[1]] |>
  bind_rows(df_rlt_objtop[[1]], df_rlt_objbot[[1]],
            df_rlt_poptopleft[[1]], df_rlt_poptopright[[1]], df_rlt_popbotleft[[1]], df_rlt_popbotright[[1]], df_rlt_popcenter[[1]]) |> 
  mutate(filename = str_replace(filename, ".txt", "")) 

df_rlt_fix_tot <- df_rlt_at[[2]] |>
  bind_rows(df_rlt_objtop[[2]], df_rlt_objbot[[2]],
            df_rlt_poptopleft[[2]], df_rlt_poptopright[[2]], df_rlt_popbotleft[[2]], df_rlt_popbotright[[2]], df_rlt_popcenter[[2]]) |> 
  mutate(filename = str_replace(filename, ".txt", ""))

df_rlt_tot <- df_rlt_gaze_tot |> 
  left_join(df_rlt_fix_tot |> ungroup() |> select(trial, abs_fix_in_aoi_duration, abs_fix_out_aoi_duration, 
                                                  abs_fix_recorded_duration, rel_fix_in_aoi), by = "trial")

rm(df_rlt_at, df_rlt_objbot, df_rlt_objtop, df_rlt_popbotleft, df_rlt_popbotright, df_rlt_popcenter, df_rlt_poptopleft, df_rlt_poptopright,
   df_rlt_gaze_tot, df_rlt_fix_tot)

# Merge All ---------------------------------------------------------------
df_tot <- data.frame(trial = 1:79) |>
  left_join(df_acc_tot |> select(-data_quality)) |>
  left_join(df_precrms_tot |>
              group_by(filename, trial, stimulus, position) |>
              summarize(precrms = mean(precrms, na.rm = T), precrms_visd = mean(precrms_visd, na.rm = T))) |>
  left_join(df_precsd_tot |>
              group_by(filename, trial, stimulus, position) |>
              summarize(precsd = mean(precsd, na.rm = T), precsd_visd = mean(precsd_visd, na.rm = T))) |> 
  left_join(df_fixdur_tot |> select(-eyetracking_outcome) |> mutate(filename = str_replace(filename, ".txt", ""))) |> 
  left_join(df_fixnum_tot |> select(-eyetracking_outcome) |> mutate(filename = str_replace(filename, ".txt", ""))) |> 
  left_join(df_latencies_tot |> select(-eyetracking_outcome) |> mutate(filename = str_replace(filename, ".txt", ""))) |> 
  left_join(df_rlt_tot |> select(-eyetracking_outcome) |> mutate(filename = str_replace(filename, ".txt", "")))  

# [3] Merge DF with ET-DQ and ET-Outcomes ---------------------------------
write.table(df_tot, here("data", "preproc_included_2", age_group, str_replace(filename, "preproc1", "preproc2")), 
            row.names = F, quote = F, sep = "\t", dec = ".")

# write.table(df_acc_tot, here("data", "preproc_included_2", age_group, paste0("accuracy_", str_replace(filename, "preproc1", "preproc2"))), 
#             row.names = F, quote = F, sep = "\t", dec = ".")
# write.table(df_precrms_tot, here("data", "preproc_included_2", age_group, paste0("precisionrms_", str_replace(filename, "preproc1", "preproc2"))), 
#             row.names = F, quote = F, sep = "\t", dec = ".")
# write.table(df_precsd_tot, here("data", "preproc_included_2", age_group, paste0("precisionsd_", str_replace(filename, "preproc1", "preproc2"))), 
#             row.names = F, quote = F, sep = "\t", dec = ".")
# write.table(df_fixdur_tot, here("data", "preproc_included_2", age_group, paste0("fixdur_", str_replace(filename, "preproc1", "preproc2"))), 
#             row.names = F, quote = F, sep = "\t", dec = ".")
# write.table(df_fixnum_tot, here("data", "preproc_included_2", age_group, paste0("fixnum_", str_replace(filename, "preproc1", "preproc2"))), 
#             row.names = F, quote = F, sep = "\t", dec = ".")
# write.table(df_latencies_tot, here("data", "preproc_included_2", age_group, paste0("latency_", str_replace(filename, "preproc1", "preproc2"))), 
#             row.names = F, quote = F, sep = "\t", dec = ".")
# write.table(df_rlt_tot, here("data", "preproc_included_2", age_group, paste0("rlt_", str_replace(filename, "preproc1", "preproc2"))), 
#             row.names = F, quote = F, sep = "\t", dec = ".")
print(i)
}
