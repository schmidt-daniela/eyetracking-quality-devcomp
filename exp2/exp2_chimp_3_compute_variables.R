# Clear Workspace ---------------------------------------------------------
rm(list = ls())
options(warn = -1) # -1 = hide warnings

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Load Functions ----------------------------------------------------------
source(here("exp1", "R", "eyetracking_data_quality.R"))
source(here("exp1", "R", "eyetracking_outcomes.R"))
source(here("exp1", "R", "utils.R"))

# Adjust Parameter --------------------------------------------------------
participant_names <- list.files(path = here("exp1", "data", "raw_clean", "chimps"))

for(i in 1:17){
nr <- i

# Read Data ---------------------------------------------------------------
raw <- read.table(here("exp1", "data", "raw_clean", "chimps", participant_names[nr]), header = T, sep = "\t") |> mutate(participant_name = participant_names[nr])
df <- raw

# Data Preparation --------------------------------------------------------

## Add Fixation Duration ----
# Why? Because gaze_event_duration refers to the duration of a fixation, irrespective of
# whether the duration was within one trial or across two trials. The analyses, however,
# are conducted on a trial level. Therefore, we need the fixation duration in one trial
# # and cut off the duration of the fixation in a previous or subsequent trial.
# df <- df |> 
#   group_by(trial, eye_movement_type_index, eye_movement_type) |> 
#   mutate(gaze_event_duration_revised = cumsum(gaze_sample_duration)) |> 
#   ungroup()

# Manual Check (scroll down until transition between trial n & trial n+1)
# df |>
#   select(trial, eye_movement_type_index, eye_movement_type, gaze_sample_duration, gaze_event_duration_revised) |>
#   View() # Check
# df |>
#   filter(Eye.movement.type == "Fixation") |> 
#   select(trial, eye_movement_type_index) |>
#   distinct() |> 
#   View() # Check

# ## Add Time in Experiment ----
# # Not used in the following.
# # However, "time_in_experiment" can be selected within the functions potentially,
# # if we consider time not as trials, but as time in the experiment.
# df <- df |> 
#   mutate(time_in_experiment = cumsum(gaze_sample_duration))

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
                                xmin = 783-40, xmax = 1137+40, ymin = 363-40, ymax = 717+40, stimulus_vec = "at",
                                media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name", trial = "trial",
                                gaze_event_index_col = "eye_movement_type_index", x_fix = "fixation_point_x", y_fix = "fixation_point_y",
                                stimulus_height = 354+80, stimulus_width = 354+80, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "at") |> 
  mutate(position = "center")

### Top Object ----
df_acc_objtop <- calculate_accuracy(df |> filter(stimulus == "object" & position == "top"), 
                                    xmin = 790-40, xmax = 1130+40, ymin = 0-40, ymax = 340+40, stimulus_vec = "object",
                                    media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name", trial = "trial",
                                    gaze_event_index_col = "eye_movement_type_index", x_fix = "fixation_point_x", y_fix = "fixation_point_y",
                                    stimulus_height = 340+80, stimulus_width = 340+80, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "object") |> 
  mutate(position = "top")

### Bottom Object ----
df_acc_objbot <- calculate_accuracy(df |> filter(stimulus == "object" & position == "bottom"), 
                                    xmin = 790-40, xmax = 1130+40, ymin = 740-40, ymax = 1080+40, stimulus_vec = "object",
                                    media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name",
                                    gaze_event_index_col = "eye_movement_type_index", x_fix = "fixation_point_x", y_fix = "fixation_point_y",
                                    stimulus_height = 340+80, stimulus_width = 340+80, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "object") |> 
  mutate(position = "bottom")

### Popflake Top Left ----
df_acc_poptopleft <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "top_left"), 
                                        xmin = 300-40, xmax = 660+40, ymin = 90-40, ymax = 450+40, stimulus_vec = "checkflake",
                                        media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name", trial = "trial",
                                        gaze_event_index_col = "eye_movement_type_index", x_fix = "fixation_point_x", y_fix = "fixation_point_y",
                                        stimulus_height = 360+80, stimulus_width = 360+80, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "top_left")

### Popflake Top Right ----
df_acc_poptopright <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "top_right"), 
                                         xmin = 1260-40, xmax = 1620+40, ymin = 90-40, ymax = 450+40, stimulus_vec = "checkflake",
                                         media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name", trial = "trial",
                                         gaze_event_index_col = "eye_movement_type_index", x_fix = "fixation_point_x", y_fix = "fixation_point_y",
                                         stimulus_height = 360+80, stimulus_width = 360+80, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "top_right")

### Popflake Bottom Left ----
df_acc_popbotleft <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "bot_left"), 
                                        xmin = 300-40, xmax = 660+40, ymin = 630-40, ymax = 990+40, stimulus_vec = "checkflake",
                                        media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name", trial = "trial",
                                        gaze_event_index_col = "eye_movement_type_index", x_fix = "fixation_point_x", y_fix = "fixation_point_y",
                                        stimulus_height = 360+80, stimulus_width = 360+80, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "bot_left")

### Popflake Bottom Right ----
df_acc_popbotright <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "bot_right"), 
                                         xmin = 1260-40, xmax = 1620+40, ymin = 630-40, ymax = 990+40, stimulus_vec = "checkflake",
                                         media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name", trial = "trial",
                                         gaze_event_index_col = "eye_movement_type_index", x_fix = "fixation_point_x", y_fix = "fixation_point_y",
                                         stimulus_height = 360+80, stimulus_width = 360+80, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "bot_right")

### Popflake Center ----
df_acc_popcenter <- calculate_accuracy(df |> filter(stimulus == "checkflake" & position == "center"), 
                                       xmin = 780-40, xmax = 1140+40, ymin = 360-40, ymax = 720+40, stimulus_vec = "checkflake",
                                       media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name", trial = "trial",
                                       gaze_event_index_col = "eye_movement_type_index", x_fix = "fixation_point_x", y_fix = "fixation_point_y",
                                       stimulus_height = 360+80, stimulus_width = 360+80, aoi_buffer_px_x = 0, aoi_buffer_px_y = 0) |> # aoi buffer of 80px is already in xyminmax
  mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |> 
  mutate(stimulus = "checkflake") |> 
  mutate(position = "center")

## Merge All Stimuli ---- # continue here
df_acc_tot <- df_acc_at |>
  bind_rows(df_acc_objtop, df_acc_objbot,
            df_acc_poptopleft, df_acc_poptopright, df_acc_popbotleft, df_acc_popbotright, df_acc_popcenter) |> 
  mutate(variable = "accuracy")|> 
  left_join(df |> select(trial, recording_name, recording_date, average_calibration_accuracy_degrees) |> distinct()) 
rm(df_acc_at, df_acc_objbot, df_acc_objtop, df_acc_popbotleft, df_acc_popbotright, df_acc_popcenter, df_acc_poptopleft, df_acc_poptopright)

# ## [1.2] Precision RMS ----
# # We will calculate data precision within fixations as the standard deviation and the root mean square of the Euclidean distances 
# # between subsequent gaze samples (Dalrymple et al., 2018; Holmqvist et al., 2011, 2012). Fixations will be classified by applying 
# # an eye-tracking filter algorithm. We only consider on-screen fixations. Lower values resulting from this calculation will be 
# # interpreted in terms of higher precision. Precision values are bounded between 0 (no variation in gaze coordinates within a fixation) 
# # and positive infinity. 
# # Formula: https://dl.acm.org/doi/pdf/10.1145/2168556.2168563
# 
# ## Attention Getter ----
# df_precrms_at <- calculate_precision_rms(df |> filter(stimulus == "at") |> mutate(trial = trial), 
#                                          media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name", 
#                                          stimulus_vec = "at", gaze_event_index_col = "eye_movement_type_index", gaze_event_dur_col = "gaze_event_duration_revised",
#                                          fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                                          screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80,
#                                          xmin = 863, xmax = 1057, ymin = 443, ymax = 637,
#                                          off_exclude = T, longest_fix_only = F, AOI_only = F) |> 
#   mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |> 
#   mutate(stimulus = "at") |> 
#   mutate(position = "center")
# 
# ## Popflakes and Objects ----
# param_precrms_objpop <- data.frame(
#   stimulus = c("object", "object", "checkflake", "checkflake", "checkflake", "checkflake", "checkflake"),
#   position = c("up", "down", "topleft", "botleft", "topright", "botright", "centercenter"),
#   xmin = c(870, 870, 380, 380, 1340, 1340, 860),
#   xmax = c(1050, 1050, 580, 580, 1540, 1540, 1060),
#   ymin = c(80, 820, 170, 710, 170, 710, 440),
#   ymax = c(260, 1000, 370, 910, 370, 910, 640),
#   df_name = c("df_precrms_objup", "df_precrms_objdown",
#               "df_precrms_poptopleft", "df_precrms_popbotleft",
#               "df_precrms_poptopright", "df_precrms_popbotright", "df_precrms_popcenter"))
# 
# for(j in c(1:7)){
#   df_precrms_objpop_temp <- calculate_precision_rms(df |> filter(stimulus == param_precrms_objpop$stimulus[j] & position == param_precrms_objpop$position[j]), 
#                                                     media_col = "stimulus", gaze_event_col = "eye_movement_type", id_col = "participant_name", 
#                                                     stimulus_vec = param_precrms_objpop$stimulus[j], 
#                                                     gaze_event_index_col = "eye_movement_type_index", gaze_event_dur_col = "gaze_event_duration_revised",
#                                                     fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                                                     screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80,
#                                                     xmin = param_precrms_objpop$xmin[j], xmax = param_precrms_objpop$xmax[j],
#                                                     ymin = param_precrms_objpop$ymin[j], ymax = param_precrms_objpop$ymax[j],
#                                                     off_exclude = T, longest_fix_only = F, AOI_only = F) |> 
#     mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |>
#     left_join(df |> select(stimulus, position, trial) |> distinct(), by = "trial")
#   
#   assign(param_precrms_objpop$df_name[j], df_precrms_objpop_temp)
#   rm(df_precrms_objpop_temp)
# }
# 
# ## Merge All Stimuli ----
# df_precrms_tot <- df_precrms_at |>
#   bind_rows(df_precrms_objup, df_precrms_objdown, df_precrms_poptopleft, df_precrms_popbotleft,
#             df_precrms_poptopright, df_precrms_popbotright, df_precrms_popcenter) |> 
#   mutate(participant_name = str_replace(participant_name, ".txt", "")) |> 
#   mutate(variable = "precisionrms") |> 
#   left_join(df |> select(trial, Recording.name, Recording.date) |> distinct()) |> 
#   rename(session = Recording.name) |> 
#   rename(date = Recording.date)
# 
# df_precrms_tot <- df |> 
#   select(trial, Participant.name) |> 
#   distinct() |> 
#   left_join(df_precrms_tot, by = c("trial")) |> 
#   rename(calibration = Participant.name) |>
#   mutate(calibration = str_replace_all(calibration, "1", ""))
# 
# 
# rm(df_precrms_at, df_precrms_objup, df_precrms_objdown, df_precrms_poptopleft, df_precrms_popbotleft,
#    df_precrms_poptopright, df_precrms_popbotright, df_precrms_popcenter)
# 
# ## [1.3] Robustness ----
# df_rob_left <- calculate_robustness(df, only_onscreen = "no", screen_width = 1920, screen_height = 1080,
#                                     blink_value = "blink", blink_col = "blink_detection.left", timestamp = "Recording.timestamp",
#                                     trial = "trial", gaze_col_x = "Gaze.point.X", gaze_col_y = "Gaze.point.Y",
#                                     gaze_col_side = "Gaze.point.left.X") |> 
#   rename(robustness_left = robustness)
# 
# df_rob_right <- calculate_robustness(df, only_onscreen = "no", screen_width = 1920, screen_height = 1080,
#                                      blink_value = "blink", blink_col = "blink_detection.left", timestamp = "Recording.timestamp",
#                                      trial = "trial", gaze_col_x = "Gaze.point.X", gaze_col_y = "Gaze.point.Y",
#                                      gaze_col_side = "Gaze.point.right.X") |> 
#   rename(robustness_right = robustness)
# 
# df_rob_tot <- df_rob_left |> 
#   full_join(df_rob_right) |> 
#   left_join(df |> select(c(trial, participant_name, Participant.name, stimulus, position, session, Recording.date)), by = "trial") |> 
#   rename(calibration = Participant.name) |> 
#   rename(date = Recording.date) |> 
#   distinct() |> 
#   mutate(variable = "robustness") |> 
#   rowwise() |> 
#   mutate(robustness = mean(c(robustness_left, robustness_right), na.rm = T))
# 
# ## [2.1] Proportional Looking Time in AOI ----
# ### Attention Getter ----
# df_rlt_at <- calculate_ltaoi(df |> filter(stimulus == "at"), media_col = "stimulus", stimulus_vec = "at",
#                              rectime = "Recording.timestamp", id_col = "participant_name", 
#                              fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                              aoi_left_upper = c(783, 363), aoi_right_lower = c(1137, 717), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower 
#   map(~ mutate(.x, stimulus = "at", position = "center", variable = "relativelookingtime"))
# # Manual Check: Zira with gaze replays
# 
# ### Top Object ----
# df_rlt_objtop <- calculate_ltaoi(df |> filter(stimulus == "object" & position == "up"), media_col = "stimulus", stimulus_vec = "object",
#                                  rectime = "Recording.timestamp", id_col = "participant_name", 
#                                  fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                                  aoi_left_upper = c(790, 0), aoi_right_lower = c(1130, 340), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
#   map(~ mutate(.x, stimulus = "object", position = "top", variable = "relativelookingtime"))
# 
# # Manual Check: Adult, Object Top, Prelast (Trial 64)
# # 100% gaze at object, validated in gaze replay
# 
# ### Bottom Object ----
# df_rlt_objbot <- calculate_ltaoi(df |> filter(stimulus == "object" & position == "down"), media_col = "stimulus", stimulus_vec = "object",
#                                  rectime = "Recording.timestamp", id_col = "participant_name", 
#                                  fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                                  aoi_left_upper = c(790, 740), aoi_right_lower = c(1130, 1080), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
#   map(~ mutate(.x, stimulus = "object", position = "bottom", variable = "relativelookingtime"))
# 
# ### Popflake Top Left ----
# df_rlt_poptopleft <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "topleft"), media_col = "stimulus", stimulus_vec = "checkflake",
#                                      rectime = "Recording.timestamp", id_col = "participant_name", 
#                                      fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                                      aoi_left_upper = c(300, 90), aoi_right_lower = c(660, 450), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
#   map(~ mutate(.x, stimulus = "checkflake", position = "topleft", variable = "relativelookingtime"))
# 
# ### Popflake Top Right ----
# df_rlt_poptopright <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "topright"), media_col = "stimulus", stimulus_vec = "checkflake",
#                                       rectime = "Recording.timestamp", id_col = "participant_name", 
#                                       fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                                       aoi_left_upper = c(1260, 90), aoi_right_lower = c(1620, 450), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
#   map(~ mutate(.x, stimulus = "checkflake", position = "topright", variable = "relativelookingtime"))
# 
# ### Popflake Bottom Left ----
# df_rlt_popbotleft <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "botleft"), media_col = "stimulus", stimulus_vec = "checkflake",
#                                      rectime = "Recording.timestamp", id_col = "participant_name", 
#                                      fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                                      aoi_left_upper = c(300, 630), aoi_right_lower = c(660, 990), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
#   map(~ mutate(.x, stimulus = "checkflake", position = "botleft", variable = "relativelookingtime"))
# 
# ### Popflake Bottom Right ----
# df_rlt_popbotright <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "botright"), media_col = "stimulus", stimulus_vec = "checkflake",
#                                       rectime = "Recording.timestamp", id_col = "participant_name", 
#                                       fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                                       aoi_left_upper = c(1260, 630), aoi_right_lower = c(1620, 990), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
#   map(~ mutate(.x, stimulus = "checkflake", position = "botright", variable = "relativelookingtime"))
# 
# ### Popflake Center ----
# df_rlt_popcenter <- calculate_ltaoi(df |> filter(stimulus == "checkflake" & position == "centercenter"), media_col = "stimulus", stimulus_vec = "checkflake",
#                                     rectime = "Recording.timestamp", id_col = "participant_name", 
#                                     fixation_point_x = "fixation_point_x", fixation_point_y = "fixation_point_y", x = "Gaze.point.X", y = "Gaze.point.Y", 
#                                     aoi_left_upper = c(780, 360), aoi_right_lower = c(1140, 720), is_00_upleftcorner = T) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
#   map(~ mutate(.x, stimulus = "checkflake", position = "centercenter", variable = "relativelookingtime"))
# 
# ### Merge All Stimuli ----
# df_rlt_gaze_tot <- df_rlt_at[[1]] |>
#   bind_rows(df_rlt_objtop[[1]], df_rlt_objbot[[1]],
#             df_rlt_poptopleft[[1]], df_rlt_poptopright[[1]], df_rlt_popbotleft[[1]], df_rlt_popbotright[[1]], df_rlt_popcenter[[1]])
# 
# df_rlt_fix_tot <- df_rlt_at[[2]] |>
#   bind_rows(df_rlt_objtop[[2]], df_rlt_objbot[[2]],
#             df_rlt_poptopleft[[2]], df_rlt_poptopright[[2]], df_rlt_popbotleft[[2]], df_rlt_popbotright[[2]], df_rlt_popcenter[[2]])
# 
# df_rlt_tot <- df_rlt_gaze_tot |> 
#   left_join(df_rlt_fix_tot |> ungroup() |> select(trial, abs_fix_in_aoi_duration, abs_fix_out_aoi_duration, 
#                                                   abs_fix_recorded_duration, rel_fix_in_aoi), by = "trial")
# 
# if(folder == "chimps_2p"){
#   df_rlt_tot <- df_rlt_tot |> 
#     mutate(calibration = rep(df$Participant.name |> unique(), nrow(df_rlt_tot))) |> 
#     mutate(calibration = gsub("_exp1|_exp2|_exp3|_exp4", "", calibration)) |> 
#     left_join(df |> select(Recording.name, trial) |> distinct(), by = "trial") |> 
#     rename(session = Recording.name) |> 
#     rename(participant_name = id) |> 
#     mutate(calibration = rep(df$Participant.name |> unique(), nrow(df_rlt_tot))) |>
#     left_join(df |> select(trial, Recording.date) |> rename(date = Recording.date)) |> 
#     as.data.frame()
# }
# 
# if(folder == "chimps_9p"){
#   df_rlt_tot <- df_rlt_tot |> 
#     left_join(df |> select(Participant.name, trial) |> distinct(), by = "trial") |> 
#     rename(calibration = Participant.name) |> 
#     mutate(calibration = gsub("1", "", calibration)) |> 
#     left_join(df |> select(Recording.name, trial) |> distinct(), by = "trial") |> 
#     rename(session = Recording.name) |> 
#     rename(participant_name = id) |> 
#     separate(session, c("session", "delete"), remove = T) |> 
#     select(-delete) |> 
#     left_join(df |> select(trial, Recording.date) |> rename(date = Recording.date)) |> 
#     as.data.frame()
# }
# 
# rm(df_rlt_at, df_rlt_objbot, df_rlt_objtop, df_rlt_popbotleft, df_rlt_popbotright, df_rlt_popcenter, df_rlt_poptopleft, df_rlt_poptopright,
#    df_rlt_gaze_tot, df_rlt_fix_tot)
# 
# # Merge All ---------------------------------------------------------------
# dfoutcome <- df_rlt_tot
# # df_tot <- data.frame(trial = 1:(79*2)) |>
# #   left_join(df_acc_tot |> select(-variable)) |>
# #   left_join(df_precrms_tot |>
# #               group_by(participant_name, trial, stimulus, position) |>
# #               summarize(precrms = mean(precrms, na.rm = T), precrms_visd = mean(precrms_visd, na.rm = T))) # |>
# # left_join(df_precsd_tot |>
# #             group_by(participant_name, trial, stimulus, position) |>
# #             summarize(precsd = mean(precsd, na.rm = T), precsd_visd = mean(precsd_visd, na.rm = T))) 
# # df_tot <- df_tot |> 
# #   left_join(df |> select(trial, Recording.name, Recording.date) |> distinct()) |> 
# #   rename(session = Recording.name) |> 
# #   rename(date = Recording.date) |> 
# #   drop_na(participant_name)

# [3] Merge DF with ET-DQ and ET-Outcomes ---------------------------------
write.table(df_acc_tot, here("exp1", "data", "preproc", "chimps", paste0("accuracy_", participant_names[nr])), row.names = F, quote = F, sep = "\t", dec = ".")
# write.table(df_precrms_tot |> select(-eye_movement_type_index), here("data", "preproc_included_2", folder, paste0("precisionrms_", participant_names[nr])), row.names = F, quote = F, sep = "\t", dec = ".")
# write.table(dfoutcome |> distinct(), here("data", "preproc_included_2", folder, paste0("etoutcomes_", participant_names[nr])), row.names = F, quote = F, sep = "\t", dec = ".")
# rm(df, df_acc_tot, df_precrms_tot, dfoutcome)
print(df_acc_tot |> group_by(participant_name) |> summarize(acc_visd = median(acc_visd, na.rm = T), average_calibration_accuracy_degrees = min(average_calibration_accuracy_degrees, na.rm = T)))
}