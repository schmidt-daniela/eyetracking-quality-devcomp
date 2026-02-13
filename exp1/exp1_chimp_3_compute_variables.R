# Clear Workspace ---------------------------------------------------------
rm(list = ls())
# options(warn = -1) # -1 = hide warnings

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Load Functions ----------------------------------------------------------
source(here("exp1", "R", "eyetracking_data_quality.R"))
source(here("exp1", "R", "eyetracking_outcomes.R"))
source(here("exp1", "R", "utils.R"))

# Adjust Parameter --------------------------------------------------------
for (i in c(1:17)) {
  folder <- "chimps" # "chimps"
  filenames <- list.files(path = here("exp1", "data", "raw_clean_blink", folder))
  n <- i
  filename <- filenames[n]
  
  # Read Data ---------------------------------------------------------------
  raw <- readRDS(here("exp1", "data", "raw_clean_blink", folder, filename))
  df <- raw |> mutate(group_id = str_remove(filename, ".rds")) |> drop_na(trial)
  
  # Add Fixation Duration ---------------------------------------------------
  # Why? Because gaze_event_duration refers to the duration of a fixation, irrespective of
  # whether the duration was within one trial or across two trials. The analyses, however,
  # are conducted on a trial level. Therefore, we need the fixation duration in one trial
  # and cut off the duration of the fixation in a previous or subsequent trial.
  df <- df |>
    group_by(trial, eye_movement_type_index, eye_movement_type) |>
    mutate(gaze_event_duration_revised = sum(gaze_sample_duration)) |>
    ungroup()
  
  # Generate Session_Trial as Time Variable ---------------------------------
  df <- df |> 
    unite(col = "session_trial", c("recording_name", "trial"), remove = F)
  
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
  
  ### Exclude Trials Without Fixation in Target AOI ----
  if ("excluded" %in% (df |> drop_na(excluded_fixation) |> pull(excluded_fixation) |> unique())) {
    df_accuracy <- df |> filter(excluded_fixation == "included")
    print("Trials without at least 1 fixation in target AOI were excluded.")
  }
  
  ### Attention Getter ----
  df_acc_at <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "at"),
    xmin = 743, # contains aoi buffer of 120px
    xmax = 1177, # contains aoi buffer of 120px
    ymin = 323, # contains aoi buffer of 120px
    ymax = 757, # contains aoi buffer of 120px
    stimulus_vec = "at",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    stimulus_height = 434, # contains aoi buffer of 120px
    stimulus_width = 434, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "at") |>
    mutate(position = "center")
  
  ### Top Object ----
  df_acc_objtop <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "object" & position == "top"),
    xmin = 750, # contains aoi buffer of 120px
    xmax = 1170, # contains aoi buffer of 120px
    ymin = -40, # contains aoi buffer of 120px
    ymax = 380, # contains aoi buffer of 120px
    stimulus_vec = "object",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    stimulus_height = 420, # contains aoi buffer of 120px
    stimulus_width = 420, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "object") |>
    mutate(position = "top")
  
  ### Bottom Object ----
  df_acc_objbot <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "object" & position == "bottom"),
    xmin = 750, # contains aoi buffer of 120px
    xmax = 1170, # contains aoi buffer of 120px
    ymin = 700, # contains aoi buffer of 120px
    ymax = 1120, # contains aoi buffer of 120px
    stimulus_vec = "object",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    stimulus_height = 420, # contains aoi buffer of 120px
    stimulus_width = 420, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "object") |>
    mutate(position = "bottom")
  
  ### Popflake Top Left ----
  df_acc_poptopleft <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "checkflake" & position == "top_left"),
    xmin = 260, # contains aoi buffer of 120px
    xmax = 700, # contains aoi buffer of 120px
    ymin = 50, # contains aoi buffer of 120px
    ymax = 490, # contains aoi buffer of 120px
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    stimulus_height = 440, # contains aoi buffer of 120px
    stimulus_width = 440, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0, 
    aoi_buffer_px_y = 0 
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "checkflake") |>
    mutate(position = "top_left")
  
  ### Popflake Top Right ----
  df_acc_poptopright <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "checkflake" & position == "top_right"),
    xmin = 1220, # contains aoi buffer of 120px
    xmax = 1660, # contains aoi buffer of 120px
    ymin = 50, # contains aoi buffer of 120px
    ymax = 490, # contains aoi buffer of 120px
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    stimulus_height = 440, # contains aoi buffer of 120px
    stimulus_width = 440, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "checkflake") |>
    mutate(position = "top_right")
  
  ### Popflake Bottom Left ----
  df_acc_popbotleft <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "checkflake" & position == "bot_left"),
    xmin = 260, # contains aoi buffer of 120px
    xmax = 700, # contains aoi buffer of 120px
    ymin = 590, # contains aoi buffer of 120px
    ymax = 1030, # contains aoi buffer of 120px
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    stimulus_height = 440, # contains aoi buffer of 120px
    stimulus_width = 440, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "checkflake") |>
    mutate(position = "bot_left")
  
  ### Popflake Bottom Right ----
  df_acc_popbotright <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "checkflake" & position == "bot_right"),
    xmin = 1220, # contains aoi buffer of 120px
    xmax = 1660, # contains aoi buffer of 120px
    ymin = 590, # contains aoi buffer of 120px
    ymax = 1030, # contains aoi buffer of 120px
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    stimulus_height = 440, # contains aoi buffer of 120px
    stimulus_width = 440, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0, 
    aoi_buffer_px_y = 0 
  ) |> 
    mutate(acc_visd = accuracy * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "checkflake") |>
    mutate(position = "bot_right")
  
  ### Popflake Center ----
  df_acc_popcenter <- calculate_accuracy(
    df_accuracy |> filter(stimulus == "checkflake" & position == "center"),
    xmin = 740, # contains aoi buffer of 120px
    xmax = 1180, # contains aoi buffer of 120px
    ymin = 320, # contains aoi buffer of 120px
    ymax = 760, # contains aoi buffer of 120px
    stimulus_vec = "checkflake",
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    gaze_event_index_col = "eye_movement_type_index",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    stimulus_height = 440, # contains aoi buffer of 120px
    stimulus_width = 440, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0, 
    aoi_buffer_px_y = 0 
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
    df_acc_poptopright,
    df_accuracy
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
    df = df |> filter(stimulus == "at"),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    stimulus_vec = "at",
    gaze_event_index_col = "eye_movement_type_index",
    gaze_event_dur_col = "gaze_event_duration_revised",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    x = "gaze_point_x",
    y = "gaze_point_y",
    screen_height_min = 0 - 120, # contains aoi buffer of 120px
    screen_width_min = 0 - 120, # contains aoi buffer of 120px
    screen_height_max = 1080 + 120, # contains aoi buffer of 120px
    screen_width_max = 1920 + 120, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0,
    xmin = 783, # contains aoi buffer of 120px
    xmax = 1137, # contains aoi buffer of 120px
    ymin = 363, # contains aoi buffer of 120px
    ymax = 717, # contains aoi buffer of 120px
    off_exclude = TRUE,
    longest_fix_only = FALSE,
    AOI_only = FALSE
  ) |>
    mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "at") |>
    mutate(position = "center") |> 
    drop_na(precrms_visd)
  
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
    xmin = c(750, 750, 260, 260, 1220, 1220, 740),
    xmax = c(1170, 1170, 700, 700, 1660, 1660, 1180),
    ymin = c(-40, 700, 50, 590, 50, 590, 320),
    ymax = c(380, 1120, 490, 1030, 490, 1030, 760),
    df_name = c(
      "df_precrms_objup",
      "df_precrms_objdown",
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
      trial = "session_trial",
      screen_height_min = 0 - 120, # contains aoi buffer of 120px
      screen_width_min = 0 - 120, # contains aoi buffer of 120px
      screen_height_max = 1080 + 120, # contains aoi buffer of 120px
      screen_width_max = 1920 + 120, # contains aoi buffer of 120px
      aoi_buffer_px_x = 0,
      aoi_buffer_px_y = 0,
      xmin = param_precrms_objpop$xmin[j],
      xmax = param_precrms_objpop$xmax[j],
      ymin = param_precrms_objpop$ymin[j],
      ymax = param_precrms_objpop$ymax[j],
      off_exclude = TRUE,
      longest_fix_only = FALSE,
      AOI_only = FALSE
    ) |>
      mutate(precrms_visd = precrms * onepx_in_visd(60, 92)) |>
      left_join(df |> select(stimulus, position, session_trial) |> distinct(), by = "session_trial") |> 
      drop_na(precrms_visd)
    
    assign(param_precrms_objpop$df_name[j], df_precrms_objpop_temp)
    rm(df_precrms_objpop_temp)
  }
  
  ## Merge All Stimuli ----
  df_precrms_tot <- df_precrms_at |>
    bind_rows(
      df_precrms_objup,
      df_precrms_objdown,
      df_precrms_poptopleft,
      df_precrms_popbotleft,
      df_precrms_poptopright,
      df_precrms_popbotright,
      df_precrms_popcenter
    ) |>
    mutate(data_quality = "precisionrms") |> 
    drop_na(precrms_visd)
  
  rm(
    df_precrms_at,
    df_precrms_objup,
    df_precrms_objdown,
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
    df = df |> filter(stimulus == "at"),
    media_col = "stimulus",
    gaze_event_col = "eye_movement_type",
    id_col = "group_id",
    stimulus_vec = "at",
    gaze_event_index_col = "eye_movement_type_index",
    gaze_event_dur_col = "gaze_event_duration_revised",
    x_fix = "fixation_point_x",
    y_fix = "fixation_point_y",
    trial = "session_trial",
    x = "gaze_point_x",
    y = "gaze_point_y",
    screen_height_min = 0 - 120, # contains aoi buffer of 120px
    screen_width_min = 0 - 120, # contains aoi buffer of 120px
    screen_height_max = 1080 + 120, # contains aoi buffer of 120px
    screen_width_max = 1920 + 120, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0,
    xmin = 783, # contains aoi buffer of 120px
    xmax = 1137, # contains aoi buffer of 120px
    ymin = 363, # contains aoi buffer of 120px
    ymax = 717, # contains aoi buffer of 120px
    off_exclude = TRUE,
    longest_fix_only = FALSE,
    AOI_only = FALSE
  ) |>
    mutate(precsd_visd = precsd * onepx_in_visd(60, 92)) |>
    mutate(stimulus = "at") |>
    mutate(position = "center") |> 
    drop_na(precsd_visd)
  
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
    xmin = c(750, 750, 260, 260, 1220, 1220, 740),
    xmax = c(1170, 1170, 700, 700, 1660, 1660, 1180),
    ymin = c(-40, 700, 50, 590, 50, 590, 320),
    ymax = c(380, 1120, 490, 1030, 490, 1030, 760),
    df_name = c(
      "df_precsd_objup",
      "df_precsd_objdown",
      "df_precsd_poptopleft",
      "df_precsd_popbotleft",
      "df_precsd_poptopright",
      "df_precsd_popbotright",
      "df_precsd_popcenter"
    )
  )
  
  for (j in c(1:7)) {
    df_precsd_objpop_temp <- calculate_precision_sd(
      df |> filter(
        stimulus == param_precsd_objpop$stimulus[j] &
          position == param_precsd_objpop$position[j]
      ),
      media_col = "stimulus",
      gaze_event_col = "eye_movement_type",
      id_col = "group_id",
      stimulus_vec = param_precsd_objpop$stimulus[j],
      gaze_event_index_col = "eye_movement_type_index",
      gaze_event_dur_col = "gaze_event_duration_revised",
      x_fix = "fixation_point_x",
      y_fix = "fixation_point_y",
      x = "gaze_point_x",
      y = "gaze_point_y",
      trial = "session_trial",
      screen_height_min = 0 - 120, # contains aoi buffer of 120px
      screen_width_min = 0 - 120, # contains aoi buffer of 120px
      screen_height_max = 1080 + 120, # contains aoi buffer of 120px
      screen_width_max = 1920 + 120, # contains aoi buffer of 120px
      aoi_buffer_px_x = 0,
      aoi_buffer_px_y = 0,
      xmin = param_precsd_objpop$xmin[j],
      xmax = param_precsd_objpop$xmax[j],
      ymin = param_precsd_objpop$ymin[j],
      ymax = param_precsd_objpop$ymax[j],
      off_exclude = TRUE,
      longest_fix_only = FALSE,
      AOI_only = FALSE
    ) |>
      mutate(precsd_visd = precsd * onepx_in_visd(60, 92)) |>
      left_join(df |> select(stimulus, position, session_trial) |> distinct(), by = "session_trial") |> 
      drop_na(precsd_visd)
    
    assign(param_precsd_objpop$df_name[j], df_precsd_objpop_temp)
    rm(df_precsd_objpop_temp)
  }
  
  ## Merge All Stimuli ----
  df_precsd_tot <- df_precsd_at |>
    bind_rows(
      df_precsd_objup,
      df_precsd_objdown,
      df_precsd_poptopleft,
      df_precsd_popbotleft,
      df_precsd_poptopright,
      df_precsd_popbotright,
      df_precsd_popcenter
    ) |>
    mutate(data_quality = "precisionsd") |> 
    drop_na(precsd_visd)
  
  rm(
    df_precsd_at,
    df_precsd_objup,
    df_precsd_objdown,
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
    trial_col             = "session_trial",
    gaze_x_col            = "gaze_point_x",
    gaze_y_col            = "gaze_point_y",
    sample_duration_col   = "gaze_sample_duration",
    blink_left_col        = "blink_detection.left",
    blink_right_col       = "blink_detection.right",
    blink_removal         = TRUE,
    blink_label           = "blink",
    blink_replacement_value = 99999,
    validity_col          = "sample_validity"
  ) |>
    rename(session_trial = trial) |> 
    left_join(df |>
                select(group_id, stimulus, position, session_trial) |>
                distinct(),
              by = "session_trial") |>
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
    trial = "session_trial",
    screen_height_min = 0 - 120, # contains aoi buffer of 120px
    screen_width_min = 0 - 120, # contains aoi buffer of 120px
    screen_height_max = 1080 + 120, # contains aoi buffer of 120px
    screen_width_max = 1920 + 120, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0,
    off_exclude = TRUE,
    longest_fix_only = FALSE
  ) |>
    left_join(df |> select(stimulus, position, session_trial) |> distinct(), by = "session_trial") |>
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
    trial = "session_trial",
    screen_height_min = 0 - 120, # contains aoi buffer of 120px
    screen_width_min = 0 - 120, # contains aoi buffer of 120px
    screen_height_max = 1080 + 120, # contains aoi buffer of 120px
    screen_width_max = 1920 + 120, # contains aoi buffer of 120px
    aoi_buffer_px_x = 0,
    aoi_buffer_px_y = 0,
    off_exclude = TRUE
  )
  
  df_fixnum_tot <- df_fixnum_tot |>
    left_join(df |> select(stimulus, position, session_trial) |> distinct(), by = "session_trial") |>
    mutate(eyetracking_outcome = "fixationnumber") |>
    rename(mean_fixation_number = n)
  
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
    trial = "session_trial",
    aoi_left_upper = c(757, 323),
    aoi_right_lower = c(1177, 743),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 120px is already in aoi_left_upper and aoi_right_lower
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
    trial = "session_trial",
    aoi_left_upper = c(750, -40),
    aoi_right_lower = c(1170, 380),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 120px is already in aoi_left_upper and aoi_right_lower
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
    trial = "session_trial",
    aoi_left_upper = c(750, 700),
    aoi_right_lower = c(1170, 1120),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 120px is already in aoi_left_upper and aoi_right_lower
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
    trial = "session_trial",
    aoi_left_upper = c(260, 50),
    aoi_right_lower = c(700, 490),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 120px is already in aoi_left_upper and aoi_right_lower
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
    trial = "session_trial",
    aoi_left_upper = c(1220, 50),
    aoi_right_lower = c(1660, 490),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 120px is already in aoi_left_upper and aoi_right_lower
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
    trial = "session_trial",
    aoi_left_upper = c(260, 590),
    aoi_right_lower = c(700, 1030),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 120px is already in aoi_left_upper and aoi_right_lower
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
    trial = "session_trial",
    aoi_left_upper = c(1220, 590),
    aoi_right_lower = c(1660, 1030),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 120px is already in aoi_left_upper and aoi_right_lower
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
    trial = "session_trial",
    aoi_left_upper = c(740, 320),
    aoi_right_lower = c(1180, 760),
    is_00_upleftcorner = TRUE
  ) |> # aoi buffer of 120px is already in aoi_left_upper and aoi_right_lower
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
          session_trial,
          abs_fix_in_aoi_duration,
          abs_fix_out_aoi_duration,
          abs_fix_recorded_duration,
          rel_fix_in_aoi
        ),
      by = "session_trial"
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
  # 0) Harmonise key column names
  if ("id" %in% names(df_rlt_tot) && !"group_id" %in% names(df_rlt_tot)) {
    df_rlt_tot <- df_rlt_tot |> rename(group_id = id)
  }
  
  # 1) Average precrms / precsd within session_trial
  precrms_sess <- df_precrms_tot |>
    group_by(group_id, session_trial, stimulus, position) |>
    summarise(
      precrms      = mean(precrms, na.rm = TRUE),
      precrms_visd = mean(precrms_visd, na.rm = TRUE),
      .groups = "drop"
    )
  
  precsd_sess <- df_precsd_tot |>
    group_by(group_id, session_trial, stimulus, position) |>
    summarise(
      precsd      = mean(precsd, na.rm = TRUE),
      precsd_visd = mean(precsd_visd, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 2) Build a master key table
  keys <- bind_rows(
    df_acc_tot        |> select(group_id, session_trial, stimulus, position),
    df_fixdur_tot     |> select(group_id, session_trial, stimulus, position),
    df_fixnum_tot     |> select(group_id, session_trial, stimulus, position),
    precrms_sess      |> select(group_id, session_trial, stimulus, position),
    precsd_sess       |> select(group_id, session_trial, stimulus, position),
    df_rlt_tot        |> select(group_id, session_trial, stimulus, position),
    df_robustness_tot |> select(group_id, session_trial, stimulus, position)
  ) |> distinct()
  
  # 3) Prepare tables
  tables <- list(
    df_acc_tot |> select(group_id, session_trial, stimulus, position, accuracy, acc_visd),
    
    precrms_sess,
    precsd_sess,
    
    df_fixdur_tot |> select(group_id, session_trial, stimulus, position, mean_fixation_duration),
    df_fixnum_tot |> select(group_id, session_trial, stimulus, position, mean_fixation_number),
    
    df_rlt_tot |>
      select(
        group_id, session_trial, stimulus, position,
        starts_with("abs_gaze_"), starts_with("rel_gaze_"),
        starts_with("abs_fix_"),  starts_with("rel_fix_")
      ),
    
    df_robustness_tot |> select(group_id, session_trial, stimulus, position, robustness_ms)
  )
  
  # 4) Merge everything
  df_tot <- reduce(
    tables,
    ~ left_join(.x, .y, by = c("group_id", "session_trial", "stimulus", "position")),
    .init = keys
  )
  
  # 5) Add ape infos
  df_tot <- df_tot |> 
    left_join(df |> select(session_trial, excluded_fixation, age_y, age_classification, sex, group) |> distinct(), by = "session_trial") |> 
    distinct()
  
  # 6) Save file
  saveRDS(df_tot, here("exp1", "data", "preproc", folder, filename), compress = "xz")
  
  print(i)
}
