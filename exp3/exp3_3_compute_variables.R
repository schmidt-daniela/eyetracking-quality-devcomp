# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

# Load Functions ----------------------------------------------------------
source(here("exp3", "R", "eyetracking_data_quality.R"))
source(here("exp3", "R", "utils.R"))

# Adjust Parameter --------------------------------------------------------
sample_size <- 24 # if 4mo, sample_size <- 24; if 6-to-18-month, sample_size <- 32
for (i in c(1:sample_size)) {
  folder <- "4mo" # "4mo", "6to18mo"
  filenames <- list.files(path = here("exp3", "data", "raw_clean_blink", folder))
  n <- i
  filename <- filenames[n]

  # Read Data ---------------------------------------------------------------
  raw <- readRDS(here("exp3", "data", "raw_clean_blink", folder, filename))
  df <- raw |> mutate(group_id = str_remove(filename, ".rds"))

  # Add Fixation Duration ---------------------------------------------------
  # Why? Because gaze_event_duration refers to the duration of a fixation, irrespective of
  # whether the duration was within one trial or across two trials. The analyses, however,
  # are conducted on a trial level. Therefore, we need the fixation duration in one trial
  # and cut off the duration of the fixation in a previous or subsequent trial.
  df <- df |>
    group_by(trial, eye_movement_type_index, eye_movement_type) |>
    mutate(gaze_event_duration_revised = sum(gaze_sample_duration)) |>
    ungroup()
  
  # Add ID Column -----------------------------------------------------------
  df <- df |> 
    unite(col = "group_id_condition", group_id, recording_name, sep = "_", remove = FALSE) |> 
    unite(col = "group_id_condition_trial", group_id, recording_name, trial, sep = "_", remove = FALSE)

  # [1] Calculate Data Quality ----------------------------------------------

  ## [1.1] Accuracy ----
  
  if ("no" %in% (df |> drop_na(trial_included) |> pull(trial_included) |> unique())) {
    df_accuracy <- df |> filter(trial_included == "yes")
    print("Trials without at least 1 fixation in target AOI were excluded.")
  } else {
    df_accuracy <- df
    print("No trials without at least 1 fixation, therefore, no trials were excluded.")
  }

  ### Attention Getter ----
  df_acc_at <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "pinwheel"),
    xmin = 783,
    xmax = 1137,
    ymin = 363,
    ymax = 717,
    stimulus_vec = "pinwheel",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 354,
    stimulus_width = 354,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax -> this does not mean that all fixations within AOI + 80px buffer are considered; we only included fixations within AOI + 40px buffer; the xmin etc. values only matter to determine the center of the stimulus, which, in turn, matters to calculate accuracy (as accuracy is the offset from the stimulus center)
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "pinwheel") |>
    mutate(position = "center")

  ### Top Object ----
  df_acc_objtop <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "object" & position == "top"),
    xmin = 790,
    xmax = 1130,
    ymin = 0,
    ymax = 340,
    stimulus_vec = "object",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
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
    df_accuracy |> filter(stimulus == "object" & position == "bottom"),
    xmin = 790,
    xmax = 1130,
    ymin = 740,
    ymax = 1080,
    stimulus_vec = "object",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
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
    df_accuracy |> filter(stimulus == "popflake" & position == "topleft"),
    xmin = 300,
    xmax = 660,
    ymin = 90,
    ymax = 450,
    stimulus_vec = "popflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "popflake")

  ### Popflake Top Right ----
  df_acc_poptopright <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "popflake" & position == "topright"),
    xmin = 1260,
    xmax = 1620,
    ymin = 90,
    ymax = 450,
    stimulus_vec = "popflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "popflake")

  ### Popflake Bottom Left ----
  df_acc_popbotleft <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "popflake" & position == "botleft"),
    xmin = 300,
    xmax = 660,
    ymin = 630,
    ymax = 990,
    stimulus_vec = "popflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "popflake") |>
    mutate(position = "botleft")

  ### Popflake Bottom Right ----
  df_acc_popbotright <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "popflake" & position == "botright"),
    xmin = 1260,
    xmax = 1620,
    ymin = 630,
    ymax = 990,
    stimulus_vec = "popflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "popflake") |>
    mutate(position = "botright")

  ### Popflake Center ----
  df_acc_popcenter <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "popflake" & position == "center"),
    xmin = 780,
    xmax = 1140,
    ymin = 360,
    ymax = 720,
    stimulus_vec = "popflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    stimulus_height = 360,
    stimulus_width = 360,
    aoi_buffer_px_x = 0, # aoi buffer of 80px is already in xyminmax
    aoi_buffer_px_y = 0 # aoi buffer of 80px is already in xyminmax
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "popflake") |>
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

  ## Attention Getter ----
  df_precrms_at <- calculate_precision_rms(
    df |> filter(stimulus == "pinwheel"),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
    stimulus_vec = "pinwheel",
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
    AOI_only = FALSE
  ) |>
    mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "pinwheel") |>
    mutate(position = "center")

  ## Popflakes and Objects ----
  param_precrms_objpop <- data.frame(
    stimulus = c(
      "object",
      "object",
      "popflake",
      "popflake",
      "popflake",
      "popflake",
      "popflake"
    ),
    position = c(
      "top",
      "bottom",
      "topleft",
      "botleft",
      "topright",
      "botright",
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
      id_col = "group_id_condition",
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
      AOI_only = FALSE
    ) |>
      mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |>
      left_join(df |> select(group_id_condition, stimulus, position, trial) |> distinct(), by = c("trial", "group_id_condition"))

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
    df |> filter(stimulus == "pinwheel"),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id_condition",
    stimulus_vec = "pinwheel",
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
    AOI_only = FALSE
  ) |>
    mutate(precsd_visd = precsd * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "pinwheel") |>
    mutate(position = "center")

  ## Popflakes and Objects ----
  param_precsd_objpop <- data.frame(
    stimulus = c(
      "object",
      "object",
      "popflake",
      "popflake",
      "popflake",
      "popflake",
      "popflake"
    ),
    position = c(
      "top",
      "bottom",
      "topleft",
      "botleft",
      "topright",
      "botright",
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
      id_col = "group_id_condition",
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
      AOI_only = FALSE
    ) |>
      mutate(precsd_visd = precsd * onepx_in_visd(60, 92)) |>
      left_join(df |> select(stimulus, position, trial, group_id_condition) |> distinct(), by = c("trial","group_id_condition"))

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
    trial_col             = "group_id_condition_trial",
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
    rename(group_id_condition_trial = trial) |> 
    left_join(df |> select(group_id, stimulus, position, group_id_condition_trial) |> distinct(), by = "group_id_condition_trial") |>
    mutate(data_quality = "robustness") |> 
    drop_na(stimulus)
  
  print("----- ----- ----- ADULTS CALIBRATION ----- ----- -----")
  print(unique(df$recording_name)[1])
  
  df_robustness_tot_2_adult <- calculate_robustness_2(
    df = df |> filter(recording_name == unique(df$recording_name)[1]),
    gaze_x_col              = "gaze_point_x",
    gaze_y_col              = "gaze_point_y",
    sample_duration_col     = "gaze_sample_duration",
    blink_left_col          = "blink_detection.left",
    blink_right_col         = "blink_detection.right",
    blink_removal           = TRUE,
    blink_label             = "blink",
    blink_replacement_value = 99999,
    robustness_check_col    = "robustness_check",
    cum_duration_col        = "cum_duration",
    truncate_at_t_ms        = 15946,
    print_max_cum           = TRUE) |>
    mutate(group_id = df$group_id |> unique()) |> 
    mutate(data_quality = "robustness_2")   |> 
    mutate(condition = "adult") |> 
    mutate(recording_name = unique(df$recording_name)[1])
  
  print("----- ----- ----- INFANT CALIBRATION ----- ----- -----")
  print(unique(df$recording_name)[2])
  
  df_robustness_tot_2_infant <- calculate_robustness_2(
    df |> filter(recording_name == unique(df$recording_name)[2]),
    gaze_x_col              = "gaze_point_x",
    gaze_y_col              = "gaze_point_y",
    sample_duration_col     = "gaze_sample_duration",
    blink_left_col          = "blink_detection.left",
    blink_right_col         = "blink_detection.right",
    blink_removal           = TRUE,
    blink_label             = "blink",
    blink_replacement_value = 99999,
    robustness_check_col    = "robustness_check",
    cum_duration_col        = "cum_duration",
    truncate_at_t_ms        = 15946,
    print_max_cum           = TRUE) |>
    mutate(group_id = df$group_id |> unique()) |> 
    mutate(data_quality = "robustness_2") |> 
    mutate(condition = "infant") |> 
    mutate(recording_name = unique(df$recording_name)[2])
  
  print("----- ----- ----- OWN CALIBRATION ----- ----- -----")
  print(unique(df$recording_name)[3])
  
  df_robustness_tot_2_own <- calculate_robustness_2(
    df |> filter(recording_name == unique(df$recording_name)[3]),
    gaze_x_col              = "gaze_point_x",
    gaze_y_col              = "gaze_point_y",
    sample_duration_col     = "gaze_sample_duration",
    blink_left_col          = "blink_detection.left",
    blink_right_col         = "blink_detection.right",
    blink_removal           = TRUE,
    blink_label             = "blink",
    blink_replacement_value = 99999,
    robustness_check_col    = "robustness_check",
    cum_duration_col        = "cum_duration",
    truncate_at_t_ms        = 15946,
    print_max_cum           = TRUE) |>
    mutate(group_id = df$group_id |> unique()) |> 
    mutate(data_quality = "robustness_2") |> 
    mutate(condition = "own") |> 
    mutate(recording_name = unique(df$recording_name)[3])
  
  df_robustness_tot_2 <- df_robustness_tot_2_own |> 
    bind_rows(df_robustness_tot_2_infant) |> 
    bind_rows(df_robustness_tot_2_adult)

  # Merge All ---------------------------------------------------------------
  df_tot <- expand.grid(group_id_condition = df$group_id_condition |> unique(), trial = 1:24) |> 
    arrange(group_id_condition) |> 
    left_join(
      df_precrms_tot |>
        group_by(group_id_condition, trial, stimulus, position) |>
        summarise(
          precrms = mean(precrms, na.rm = TRUE),
          precrms_visd = mean(precrms_visd, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("group_id_condition", "trial")
    ) |>
    left_join(
      df_precsd_tot |>
        group_by(group_id_condition, trial, stimulus, position) |>
        summarise(
          precsd = mean(precsd, na.rm = TRUE),
          precsd_visd = mean(precsd_visd, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("group_id_condition", "trial", "stimulus", "position")
    ) |>
    left_join(df_robustness_tot |> extract(
          col = group_id_condition_trial, 
          into = c("group_id_condition", "trial"), 
          regex = "(.*)_(\\d+)",
          convert = TRUE, 
          remove = TRUE
        ) |> select(group_id_condition, trial, robustness_ms, robustness_prop, stimulus, position),
        by = c("group_id_condition", "trial", "stimulus", "position")) |>
    left_join(df_robustness_tot_2 |> 
                unite(
                  col = "group_id_condition",
                  group_id, recording_name,
                  sep = "_",
                  remove = TRUE
                ) |> 
                select(robustness_ms_2, group_id_condition), by = "group_id_condition") |>
    left_join(df_acc_tot |> select(group_id_condition, trial, accuracy, acc_visd), by = c("trial", "group_id_condition")) |> 
    add_demo_cols(df = df, folder = folder) |> 
    mutate(condition = str_extract(group_id_condition, "[^_]+$"),
           age_group = str_split_i(group_id_condition, "_", 1) |> str_replace("mo", "M"),
           id = as.integer(str_split_i(group_id_condition, "_", 2)))
  
  # [3] Merge DF with ET-DQ and ET-Outcomes ---------------------------------
  saveRDS(df_tot, here("exp3", "data", "preproc", folder, filename), compress = "xz")

  print(i)
}
