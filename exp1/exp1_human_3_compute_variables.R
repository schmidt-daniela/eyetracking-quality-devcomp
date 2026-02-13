# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Load Functions ----------------------------------------------------------
source(here("exp1", "R", "eyetracking_data_quality.R"))
source(here("exp1", "R", "eyetracking_outcomes.R"))
source(here("exp1", "R", "utils.R"))

# Adjust Parameter --------------------------------------------------------
for (i in c(1:32)) {
  folder <- "4m" # "4m", "6m", "9m", "18m", or "adults"
  filenames <- list.files(path = here("exp1", "data", "raw_clean_blink", folder))
  n <- i
  filename <- filenames[n]

  # Read Data ---------------------------------------------------------------
  raw <- readRDS(here("exp1", "data", "raw_clean_blink", folder, filename))
  df <- raw |> mutate(group_id = str_remove(filename, ".rds"))

  if ("excluded" %in% (df |> drop_na(excluded_fixation) |> pull(excluded_fixation) |> unique())) {
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
    mutate(gaze_event_duration_revised = sum(gaze_sample_duration)) |>
    ungroup()

  # [1] Calculate Data Quality ----------------------------------------------

  ## [1.1] Accuracy ----
  # We define data accuracy as the Euclidean distance between gaze location and 
  # center of the target (Dalrymple et al., 2018). We measure accuracy during 
  # the fixation whose gaze coordinates are (compared to all fixations of this trials) 
  # closest to the center of the displayed target. Fixations are classified by 
  # applying an Tobii eye-tracking filter algorithm. We only consider fixations within 
  # pre-specified areas of interests (AOIs). Lower values resulting from this 
  # calculation are interpreted in terms of better accuracy.Accuracy values are bounded 
  # between 0 (gaze location equals the center of the target) and positive infinity.

  ### Attention Getter ----
  df_acc_at <- calculate_accuracy(
    df |> filter(stimulus == "at"),
    xmin = 783,
    xmax = 1137,
    ymin = 363,
    ymax = 717,
    stimulus_vec = "at",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 354,
    stimulus_width = 354,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "at") |>
    mutate(position = "center")

  ### Top Object ----
  df_acc_objtop <- calculate_accuracy(
    df |> filter(stimulus == "object" & position == "top"),
    xmin = 790,
    xmax = 1130,
    ymin = 0,
    ymax = 340,
    stimulus_vec = "object",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 340,
    stimulus_width = 340,
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0
  ) |> # aoi buffer of 80px is already in xyminmax
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "object") |>
    mutate(position = "top")

  ### Bottom Object ----
  df_acc_objbot <- calculate_accuracy(
    df |> filter(stimulus == "object" & position == "bottom"),
    xmin = 790,
    xmax = 1130,
    ymin = 740,
    ymax = 1080,
    stimulus_vec = "object",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 340,
    stimulus_width = 340,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "object") |>
    mutate(position = "bottom")

  ### Popflake Top Left ----
  df_acc_poptopleft <- calculate_accuracy(
    df |> filter(stimulus == "checkflake" & position == "top_left"),
    xmin = 300,
    xmax = 660,
    ymin = 90,
    ymax = 450,
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "checkflake") |>
    mutate(position = "top_left")

  ### Popflake Top Right ----
  df_acc_poptopright <- calculate_accuracy(
    df |> filter(stimulus == "checkflake" & position == "top_right"),
    xmin = 1260,
    xmax = 1620,
    ymin = 90,
    ymax = 450,
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "checkflake") |>
    mutate(position = "top_right")

  ### Popflake Bottom Left ----
  df_acc_popbotleft <- calculate_accuracy(
    df |> filter(stimulus == "checkflake" & position == "bot_left"),
    xmin = 300,
    xmax = 660,
    ymin = 630,
    ymax = 990,
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "checkflake") |>
    mutate(position = "bot_left")

  ### Popflake Bottom Right ----
  df_acc_popbotright <- calculate_accuracy(
    df |> filter(stimulus == "checkflake" & position == "bot_right"),
    xmin = 1260,
    xmax = 1620,
    ymin = 630,
    ymax = 990,
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "checkflake") |>
    mutate(position = "bot_right")

  ### Popflake Center ----
  df_acc_popcenter <- calculate_accuracy(
    df |> 
      filter(stimulus == "checkflake" & position == "center"),
    xmin = 780,
    xmax = 1140,
    ymin = 360,
    ymax = 720,
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "checkflake") |>
    mutate(position = "center")

  ## Merge All Stimuli ----
  df_acc_tot <- df_acc_at |>
    bind_rows(
      df_acc_objtop,
      df_acc_objbot,
      df_acc_poptopleft,
      df_acc_poptopright,
      df_acc_popbotleft,
      df_acc_popbotright,
      df_acc_popcenter
    ) |>
    mutate(data_quality = "accuracy")

  rm(
    df_acc_at,
    df_acc_objbot,
    df_acc_objtop,
    df_acc_popbotleft,
    df_acc_popbotright,
    df_acc_popcenter,
    df_acc_poptopleft,
    df_acc_poptopright
  )

  ## [1.2] Precision RMS ----
  # We will calculate data precision within fixations as the standard deviation
  # and the root mean square of the Euclidean distances between subsequent gaze
  # samples (Dalrymple et al., 2018; Holmqvist et al., 2011, 2012). Fixations
  # will be classified by applying an eye-tracking filter algorithm. We only
  # consider on-screen fixations. Lower values resulting from this calculation
  # will be interpreted in terms of higher precision. Precision values are
  # bounded between 0 (no variation in gaze coordinates within a fixation)
  # and positive infinity.
  # Formula: https://dl.acm.org/doi/pdf/10.1145/2168556.2168563

  ## Attention Getter ----
  df_precrms_at <- calculate_precision_rms(
    df |> filter(stimulus == "at"),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    stimulus_vec = "at",
    gaze_event_index_col = "eye_movement_type_index",
    gaze_event_dur_col = "gaze_event_duration_revised",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    screen_height_min = 0, screen_width_min = 0,
    screen_height_max = 1080, screen_width_max = 1920,
    aoi_buffer_px_x = 80,
    aoi_buffer_px_y = 80,
    xmin = 863,
    xmax = 1057,
    ymin = 443,
    ymax = 637,
    off_exclude = TRUE,
    longest_fix_only = FALSE,
    AOI_only = TRUE
  ) |>
    mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "at") |>
    mutate(position = "center")

  ## Popflakes and Objects ----
  param_precrms_objpop <- data.frame(
    stimulus = c(
      "object",
      "object",
      "checkflake",
      "checkflake",
      "checkflake",
      "checkflake",
      "checkflake"
    ),
    position = c(
      "top",
      "bottom",
      "top_left",
      "bot_left",
      "top_right",
      "bot_right",
      "center"
    ),
    xmin = c(870, 870, 380, 380, 1340, 1340, 860),
    xmax = c(1050, 1050, 580, 580, 1540, 1540, 1060),
    ymin = c(80, 820, 170, 710, 170, 710, 440),
    ymax = c(260, 1000, 370, 910, 370, 910, 640),
    df_name = c(
      "df_precrms_objtop",
      "df_precrms_objbot",
      "df_precrms_poptopleft",
      "df_precrms_popbotleft",
      "df_precrms_poptopright",
      "df_precrms_popbotright",
      "df_precrms_popcenter"
    )
  )

  for (j in c(1:7)) {
    df_precrms_objpop_temp <- calculate_precision_rms(
      df |> filter(
        stimulus == param_precrms_objpop$stimulus[j] &
          position == param_precrms_objpop$position[j]
      ),
      media_col = "stimulus",
      gaze_event_col = "eye_movement_type",
      id_col = "group_id",
      stimulus_vec = param_precrms_objpop$stimulus[j],
      gaze_event_index_col = "eye_movement_type_index",
      gaze_event_dur_col = "gaze_event_duration_revised",
      x_fix = "fixation_point_x",
      y_fix = "fixation_point_y",
      x = "gaze_point_x",
      y = "gaze_point_y",
      screen_height_min = 0, screen_width_min = 0,
      screen_height_max = 1080, screen_width_max = 1920,
      aoi_buffer_px_x = 80,
      aoi_buffer_px_y = 80,
      xmin = param_precrms_objpop$xmin[j],
      xmax = param_precrms_objpop$xmax[j],
      ymin = param_precrms_objpop$ymin[j],
      ymax = param_precrms_objpop$ymax[j],
      off_exclude = TRUE,
      longest_fix_only = FALSE,
      AOI_only = TRUE
    ) |>
      mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |>
      left_join(df |> select(stimulus, position, trial) |> distinct(), by = "trial")

    assign(param_precrms_objpop$df_name[j], df_precrms_objpop_temp)
    rm(df_precrms_objpop_temp)
  }

  ## Merge All Stimuli ----
  df_precrms_tot <- df_precrms_at |>
    bind_rows(
      df_precrms_objtop,
      df_precrms_objbot,
      df_precrms_poptopleft,
      df_precrms_popbotleft,
      df_precrms_poptopright,
      df_precrms_popbotright,
      df_precrms_popcenter
    ) |>
    mutate(data_quality = "precisionrms")

  rm(
    df_precrms_at,
    df_precrms_objtop,
    df_precrms_objbot,
    df_precrms_poptopleft,
    df_precrms_popbotleft,
    df_precrms_poptopright,
    df_precrms_popbotright,
    df_precrms_popcenter,
    param_precrms_objpop
  )

  ## [1.3] Precision SD ----
  # We will calculate data precision within fixations as the standard deviation and the root mean square of the Euclidean distances
  # between subsequent gaze samples (Dalrymple et al., 2018; Holmqvist et al., 2011, 2012). Fixations will be classified by applying
  # an eye-tracking filter algorithm. We only consider on-screen fixations. Lower values resulting from this calculation will be
  # interpreted in terms of higher precision. Precision values are bounded between 0 (no variation in gaze coordinates within a fixation)
  # and positive infinity.

  ## Attention Getter ----
  df_precsd_at <- calculate_precision_sd(
    df |> filter(stimulus == "at"),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    stimulus_vec = "at",
    gaze_event_index_col = "eye_movement_type_index",
    gaze_event_dur_col = "gaze_event_duration_revised",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    screen_height_min = 0, screen_width_min = 0,
    screen_height_max = 1080, screen_width_max = 1920,
    aoi_buffer_px_x = 80,
    aoi_buffer_px_y = 80,
    xmin = 863,
    xmax = 1057,
    ymin = 443,
    ymax = 637,
    off_exclude = TRUE,
    longest_fix_only = FALSE,
    AOI_only = TRUE
  ) |>
    mutate(precsd_visd = precsd * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "at") |>
    mutate(position = "center")

  ## Popflakes and Objects ----
  param_precsd_objpop <- data.frame(
    stimulus = c(
      "object",
      "object",
      "checkflake",
      "checkflake",
      "checkflake",
      "checkflake",
      "checkflake"
    ),
    position = c(
      "top",
      "bottom",
      "top_left",
      "bot_left",
      "top_right",
      "bot_right",
      "center"
    ),
    xmin = c(870, 870, 380, 380, 1340, 1340, 860),
    xmax = c(1050, 1050, 580, 580, 1540, 1540, 1060),
    ymin = c(80, 820, 170, 710, 170, 710, 440),
    ymax = c(260, 1000, 370, 910, 370, 910, 640),
    df_name = c(
      "df_precsd_objtop",
      "df_precsd_objbot",
      "df_precsd_poptopleft",
      "df_precsd_popbotleft",
      "df_precsd_poptopright",
      "df_precsd_popbotright",
      "df_precsd_popcenter"
    )
  )

  for (k in c(1:7)) {
    df_precsd_objpop_temp <- calculate_precision_sd(
      df |> filter(
        stimulus == param_precsd_objpop$stimulus[k] &
          position == param_precsd_objpop$position[k]
      ),
      media_col = "stimulus",
      gaze_event_col = "eye_movement_type",
      id_col = "group_id",
      stimulus_vec = param_precsd_objpop$stimulus[k],
      gaze_event_index_col = "eye_movement_type_index",
      gaze_event_dur_col = "gaze_event_duration_revised",
      x_fix = "fixation_point_x",
      y_fix = "fixation_point_y",
      x = "gaze_point_x",
      y = "gaze_point_y",
      screen_height_min = 0, screen_width_min = 0,
      screen_height_max = 1080, screen_width_max = 1920,
      aoi_buffer_px_x = 80,
      aoi_buffer_px_y = 80,
      xmin = param_precsd_objpop$xmin[k],
      xmax = param_precsd_objpop$xmax[k],
      ymin = param_precsd_objpop$ymin[k],
      ymax = param_precsd_objpop$ymax[k],
      off_exclude = TRUE,
      longest_fix_only = FALSE,
      AOI_only = TRUE
    ) |>
      mutate(precsd_visd = precsd * onepx_in_visd(60, 92)) |>
      left_join(df |> select(stimulus, position, trial) |> distinct(), by = "trial")

    assign(param_precsd_objpop$df_name[k], df_precsd_objpop_temp)
    rm(df_precsd_objpop_temp)
  }

  ## Merge All Stimuli ----
  df_precsd_tot <- df_precsd_at |>
    bind_rows(
      df_precsd_objtop,
      df_precsd_objbot,
      df_precsd_poptopleft,
      df_precsd_popbotleft,
      df_precsd_poptopright,
      df_precsd_popbotright,
      df_precsd_popcenter
    ) |>
    mutate(data_quality = "precisionsd")

  rm(
    df_precsd_at,
    df_precsd_objtop,
    df_precsd_objbot,
    df_precsd_poptopleft,
    df_precsd_popbotleft,
    df_precsd_poptopright,
    df_precsd_popbotright,
    df_precsd_popcenter,
    param_precsd_objpop
  )

  ## [1.4] Robustness ----
  # We will calculate data robustness as the duration of the mean usable data fragments obtained after interpolating blinks. The greater the
  # mean duration of usable data, the more robust the data. Robustness values are bounded between 0 (no usable data) and the total recording
  # duration considered.
  df_robustness_tot <- calculate_robustness(
    df,
    trial_col             = "trial",
    gaze_x_col            = "gaze_point_x",
    gaze_y_col            = "gaze_point_y",
    sample_duration_col   = "gaze_sample_duration",
    blink_left_col        = "blink_detection.left",
    blink_right_col       = "blink_detection.right",
    blink_removal         = TRUE,
    blink_label           = "blink",
    blink_replacement_value = 99999,
    validity_col          = "sample_validity", 
    relative_robustness = TRUE
  ) |>
    left_join(df |> select(group_id, stimulus, position, trial) |> distinct(), by = "trial") |>
    mutate(data_quality = "robustness") |> 
    drop_na(stimulus)

  # [2] Calculate Eye-Tracking Outcomes -------------------------------------
  # We consider the following common eye-tracking measures as dependent variables: fixation durations, number of fixations, latencies (i.e.,
  # the time between stimulus onset and first gaze point in AOI), and proportional looking time in AOI (relative to looking time on screen)

  ## Fixation Duration ----
  df_fixdur_tot <- calculate_fixdur(
    df,
    gaze_event_col = "eye_movement_type",
    media_col = "stimulus",
    stimulus_vec = c("checkflake", "at", "object"),
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    gaze_event_dur_col = "gaze_event_duration_revised",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    screen_height_min = 0, screen_width_min = 0,
    screen_height_max = 1080, screen_width_max = 1920,
    aoi_buffer_px_x = 80,
    aoi_buffer_px_y = 80,
    off_exclude = TRUE,
    longest_fix_only = FALSE
  ) |>
    left_join(df |> select(stimulus, position, trial) |> distinct(), by = "trial") |>
    mutate(eyetracking_outcome = "fixationduration")

  ## Number of Fixations ----
  df_fixnum_tot <- calculate_fixnum(
    df,
    gaze_event_col = "eye_movement_type",
    media_col = "stimulus",
    stimulus_vec = c("checkflake", "at", "object"),
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    gaze_event_dur_col = "gaze_event_duration_revised",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    screen_height_min = 0, screen_width_min = 0,
    screen_height_max = 1080, screen_width_max = 1920,
    aoi_buffer_px_x = 80,
    aoi_buffer_px_y = 80,
    off_exclude = TRUE
  )

  df_fixnum_tot <- df_fixnum_tot |>
    left_join(df |> select(stimulus, position, trial) |> distinct(), by = "trial") |>
    mutate(eyetracking_outcome = "fixationnumber") |>
    rename(mean_fixation_number = n)

  ## Latencies ----
  df_lat <- df

  if (df |> drop_na(excluded_100ms) |> pull(excluded_100ms) |> unique() |> length() > 1 & folder != "tt") {
    df_lat <- df_lat |> filter(excluded_100ms == "included")
    print("<100ms latency trials were excluded.")
  }

  if (df_lat |> drop_na(excluded_3sd) |> pull(excluded_3sd) |> unique() |> length() > 1) {
    df_lat <- df_lat |> filter(excluded_3sd == "included")
    print(">3sd latency trials were excluded.")
  }

  first_sample_in_trial <- df_lat |>
    filter(stimulus_position %in% c("object_top", "object_bottom") & stimulus_duration == "gaze_contingent") |> 
    select(trial, stimulus_position, recording_timestamp, aoi_samples) |>
    group_by(trial, stimulus_position) |>
    slice(1) |>
    ungroup() |>
    rename(trialstart = recording_timestamp) |>
    select(-aoi_samples)

  first_sample_in_aoi <- df_lat |>
    filter(stimulus_position %in% c("object_top", "object_bottom") & stimulus_duration == "gaze_contingent") |> 
    select(trial, stimulus_position, recording_timestamp, aoi_samples) |>
    filter(aoi_samples != "not_in_aoi") |>
    group_by(trial, stimulus_position) |>
    slice(1) |>
    ungroup() |>
    rename(arrivalinaoi = recording_timestamp) |>
    select(-aoi_samples)

  latencies <- first_sample_in_trial |>
    left_join(first_sample_in_aoi, by = c("trial", "stimulus_position")) |>
    mutate(latencies = (arrivalinaoi - trialstart) / 1000) # unit: ms

  df_latencies_tot <- latencies |>
    mutate(eyetracking_outcome = "latency") |> 
    select(-c(trialstart, arrivalinaoi)) |> 
    mutate(group_id = str_remove(filename, ".rds")) |> 
    separate(stimulus_position, into = c("stimulus", "position"), sep = "_")
  
  df_latencies_tot <- df |> 
    filter(str_detect(presented_stimulus_name, "move")) |> 
    transmute(
      trial,
      congruence = case_when(
        str_detect(presented_stimulus_name, "_con_")   ~ "con",
        str_detect(presented_stimulus_name, "_incon_") ~ "incon",
        TRUE ~ NA_character_
      )
    ) |> 
    distinct() |> 
    right_join(df_latencies_tot, by = "trial")
  
  rm(first_sample_in_trial, first_sample_in_aoi, latencies)

  ## Proportional Looking Time in AOI ----
  ### Attention Getter ----
  df_rlt_at <- calculate_ltaoi(
    df |> filter(stimulus == "at"),
    media_col = "stimulus",
    stimulus_vec = "at",
    rectime = "recording_timestamp",
    id_col = "group_id",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    aoi_left_upper = c(783, 363),
    aoi_right_lower = c(1137, 717),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
    map(
      ~ mutate(
        .x,
        stimulus = "at",
        position = "center",
        eyetracking_outcome = "relativelookingtime"
      )
    )

  ### Top Object ----
  df_rlt_objtop <- calculate_ltaoi(
    df |> filter(stimulus == "object" &
      position == "top"),
    media_col = "stimulus",
    stimulus_vec = "object",
    rectime = "recording_timestamp",
    id_col = "group_id",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    aoi_left_upper = c(790, 0),
    aoi_right_lower = c(1130, 340),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
    map(
      ~ mutate(
        .x,
        stimulus = "object",
        position = "top",
        eyetracking_outcome = "relativelookingtime"
      )
    )

  ### Bottom Object ----
  df_rlt_objbot <- calculate_ltaoi(
    df |> filter(stimulus == "object" &
      position == "bottom"),
    media_col = "stimulus",
    stimulus_vec = "object",
    rectime = "recording_timestamp",
    id_col = "group_id",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    aoi_left_upper = c(790, 740),
    aoi_right_lower = c(1130, 1080),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
    map(
      ~ mutate(
        .x,
        stimulus = "object",
        position = "bottom",
        eyetracking_outcome = "relativelookingtime"
      )
    )

  ### Popflake Top Left ----
  df_rlt_poptopleft <- calculate_ltaoi(
    df |> filter(stimulus == "checkflake" &
      position == "top_left"),
    media_col = "stimulus",
    stimulus_vec = "checkflake",
    rectime = "recording_timestamp",
    id_col = "group_id",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    aoi_left_upper = c(300, 90),
    aoi_right_lower = c(660, 450),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
    map(
      ~ mutate(
        .x,
        stimulus = "checkflake",
        position = "top_left",
        eyetracking_outcome = "relativelookingtime"
      )
    )

  ### Popflake Top Right ----
  df_rlt_poptopright <- calculate_ltaoi(
    df |> filter(stimulus == "checkflake" &
      position == "top_right"),
    media_col = "stimulus",
    stimulus_vec = "checkflake",
    rectime = "recording_timestamp",
    id_col = "group_id",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    aoi_left_upper = c(1260, 90),
    aoi_right_lower = c(1620, 450),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
    map(
      ~ mutate(
        .x,
        stimulus = "checkflake",
        position = "top_right",
        eyetracking_outcome = "relativelookingtime"
      )
    )

  ### Popflake Bottom Left ----
  df_rlt_popbotleft <- calculate_ltaoi(
    df |> filter(stimulus == "checkflake" &
      position == "bot_left"),
    media_col = "stimulus",
    stimulus_vec = "checkflake",
    rectime = "recording_timestamp",
    id_col = "group_id",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    aoi_left_upper = c(300, 630),
    aoi_right_lower = c(660, 990),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
    map(
      ~ mutate(
        .x,
        stimulus = "checkflake",
        position = "bot_left",
        eyetracking_outcome = "relativelookingtime"
      )
    )

  ### Popflake Bottom Right ----
  df_rlt_popbotright <- calculate_ltaoi(
    df |> filter(stimulus == "checkflake" &
      position == "bot_right"),
    media_col = "stimulus",
    stimulus_vec = "checkflake",
    rectime = "recording_timestamp",
    id_col = "group_id",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    aoi_left_upper = c(1260, 630),
    aoi_right_lower = c(1620, 990),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
    map(
      ~ mutate(
        .x,
        stimulus = "checkflake",
        position = "bot_right",
        eyetracking_outcome = "relativelookingtime"
      )
    )

  ### Popflake Center ----
  df_rlt_popcenter <- calculate_ltaoi(
    df |> filter(stimulus == "checkflake" &
      position == "center"),
    media_col = "stimulus",
    stimulus_vec = "checkflake",
    rectime = "recording_timestamp",
    id_col = "group_id",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    x = "gaze_point_x",
    y = "gaze_point_y",
    aoi_left_upper = c(780, 360),
    aoi_right_lower = c(1140, 720),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 80px is already in aoi_left_upper and aoi_right_lower
    map(
      ~ mutate(
        .x,
        stimulus = "checkflake",
        position = "center",
        eyetracking_outcome = "relativelookingtime"
      )
    )

  ### Merge All Stimuli ----
  df_rlt_gaze_tot <- df_rlt_at[[1]] |>
    bind_rows(
      df_rlt_objtop[[1]],
      df_rlt_objbot[[1]],
      df_rlt_poptopleft[[1]],
      df_rlt_poptopright[[1]],
      df_rlt_popbotleft[[1]],
      df_rlt_popbotright[[1]],
      df_rlt_popcenter[[1]]
    )

  df_rlt_fix_tot <- df_rlt_at[[2]] |>
    bind_rows(
      df_rlt_objtop[[2]],
      df_rlt_objbot[[2]],
      df_rlt_poptopleft[[2]],
      df_rlt_poptopright[[2]],
      df_rlt_popbotleft[[2]],
      df_rlt_popbotright[[2]],
      df_rlt_popcenter[[2]]
    )

  df_rlt_tot <- df_rlt_gaze_tot |>
    left_join(
      df_rlt_fix_tot |> 
        ungroup() |> 
        select(
          trial,
          abs_fix_in_aoi_duration,
          abs_fix_out_aoi_duration,
          abs_fix_recorded_duration,
          rel_fix_in_aoi
          ),
      by = "trial"
      )

  rm(
    df_rlt_at,
    df_rlt_objbot,
    df_rlt_objtop,
    df_rlt_popbotleft,
    df_rlt_popbotright,
    df_rlt_popcenter,
    df_rlt_poptopleft,
    df_rlt_poptopright,
    df_rlt_gaze_tot,
    df_rlt_fix_tot
  )

  # Merge All ---------------------------------------------------------------
  df_tot <- data.frame(trial = 1:79) |>
    left_join(df_acc_tot |> select(-data_quality), by = "trial") |>
    left_join(
      df_precrms_tot |>
        group_by(group_id, trial, stimulus, position) |>
        summarise(
          precrms = mean(precrms, na.rm = TRUE),
          precrms_visd = mean(precrms_visd, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "trial"
    ) |>
    left_join(
      df_precsd_tot |>
        group_by(group_id, trial, stimulus, position) |>
        summarise(
          precsd = mean(precsd, na.rm = TRUE),
          precsd_visd = mean(precsd_visd, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "trial"
    ) |>
    left_join(df_robustness_tot |> select(group_id, trial, robustness_ms, robustness_prop), by = "trial") |>
    left_join(df_fixdur_tot |> select(-eyetracking_outcome), by = "trial") |>
    left_join(df_fixnum_tot |> select(-eyetracking_outcome), by = "trial") |>
    left_join(df_latencies_tot |> select(-eyetracking_outcome), by = "trial") |>
    left_join(df_rlt_tot |> select(-eyetracking_outcome), by = "trial") |>
    add_demo_cols(df = df, folder = folder)
  
  base_cols <- c(
    "group_id", "trial", "stimulus", "position",
    "accuracy", "acc_visd",
    "precrms", "precrms_visd",
    "precsd", "precsd_visd",
    "robustness_ms", "robustness_prop",
    "mean_fixation_duration", "mean_fixation_number",
    "latencies", "congruence",
    "rel_gaze_in_aoi", "rel_fix_in_aoi",
    "abs_gaze_in_aoi_duration", "abs_gaze_out_aoi_duration", "abs_gaze_recorded_duration",
    "abs_fix_in_aoi_duration", "abs_fix_out_aoi_duration", "abs_fix_recorded_duration",
    "excluded_100ms", "excluded_3sd", "excluded_fixation",
    "sex", "age", "order", "experimenter"
  )
  
  extra_cols_nonadult <- c("no_siblings", "no_household", "multilingual", "kindergarten_yn", "tagesmutter_yn")
  
  df_tot <- df_tot |>
    select(any_of(c(base_cols, if (folder != "adult") extra_cols_nonadult)))
  
  # [3] Merge DF with ET-DQ and ET-Outcomes ---------------------------------
  saveRDS(df_tot, here("exp1", "data", "preproc", folder, filename), compress = "xz")

  print(i)
  print(df_tot |> colnames() |> sort())
}
