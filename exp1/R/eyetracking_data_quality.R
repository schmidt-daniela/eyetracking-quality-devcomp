# Accuracy ----
calculate_accuracy <- function(df, xmin = 849, xmax = 1072, ymin = 417, ymax = 672, stimulus_vec = c("ATTENTION_Familiarization.mp4", "ATTENTION_Preflooking.mp4"),
                               media_col = "Presented.Media.name", gaze_event_col = "Eye.movement.type", id_col = "Recording.name", trial = "trial",
                               gaze_event_index_col = "Eye.movement.type.index", x_fix = "Fixation.point.X", y_fix = "Fixation.point.Y",
                               stimulus_height = 255, stimulus_width = 223, aoi_buffer_px_x = 80, aoi_buffer_px_y = 80){
  
  stimulus_center_x <- mean(c(xmax, xmin))
  stimulus_center_y <- mean(c(ymax, ymin))
  
  df_proc <- df |> 
    filter(.data[[gaze_event_col]] == "Fixation") |> 
    filter(.data[[media_col]] %in% stimulus_vec)
  
  df_proc <- df_proc |>
    select({{trial}}, {{id_col}}, {{gaze_event_index_col}}, {{x_fix}}, {{y_fix}}) |> 
    mutate(stimulus_center_x = stimulus_center_x,
           stimulus_center_y = stimulus_center_y) |> 
    mutate(distance_x = abs(.data[[x_fix]] - stimulus_center_x),
           distance_y = abs(.data[[y_fix]] - stimulus_center_y)) |> 
    mutate(accuracy = sqrt(distance_x^2 + distance_y^2))
  
  acc <- df_proc |> 
    filter(distance_x <= ((stimulus_width/2) + aoi_buffer_px_x) & distance_y <= ((stimulus_height/2) + aoi_buffer_px_y)) |> 
    group_by(.data[[id_col]], .data[[trial]]) |>
    filter(accuracy == min(accuracy, na.rm = T)) |>
    select({{trial}}, {{id_col}}, accuracy) |> 
    distinct() |> 
    ungroup()
  
  return(acc)
}

# Precision RMS ----
# Formula: https://dl.acm.org/doi/pdf/10.1145/2168556.2168563
calculate_precision_rms <- function(df, media_col = "Presented.Media.name", gaze_event_col = "Eye.movement.type", id_col = "Recording.name", 
                                    stimulus_vec = c("ATTENTION_Familiarization.mp4", "ATTENTION_Preflooking.mp4"),
                                    gaze_event_index_col = "Eye.movement.type.index", gaze_event_dur_col = "Gaze.event.duration",
                                    x_fix = "Fixation.point.X", y_fix = "Fixation.point.Y", x = "Gaze.point.X", y = "Gaze.point.Y", 
                                    screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 40, aoi_buffer_px_y = 40,
                                    xmin = 849, xmax = 1072, ymin = 417, ymax = 672,
                                    off_exclude = F, longest_fix_only = F, AOI_only = F) {
  coordinates_df_sub <- df |> 
    filter(.data[[media_col]] %in% stimulus_vec) |> 
    filter(.data[[gaze_event_col]] == "Fixation")
  
  if(off_exclude == T){
      # Exclude all fixations whose coordinates are outside the screen AOI
      exclude_rows <- which(coordinates_df_sub[[x_fix]] < 0 | coordinates_df_sub[[x_fix]] > screen_width + aoi_buffer_px_x |
                              coordinates_df_sub[[y_fix]] < 0 | coordinates_df_sub[[y_fix]] > screen_height + aoi_buffer_px_y)
      if(exclude_rows |> length() > 0){coordinates_df_sub <- coordinates_df_sub[-exclude_rows,]}
      }
  
  if(AOI_only == T){
    # Exclude all fixations whose coordinates are outside the target AOI
    exclude_rows <- which(coordinates_df_sub[[x_fix]] < xmin - aoi_buffer_px_x | coordinates_df_sub[[x_fix]] > xmax + aoi_buffer_px_x |
                            coordinates_df_sub[[y_fix]] < ymin -  aoi_buffer_px_y | coordinates_df_sub[[y_fix]] > ymax + aoi_buffer_px_y )
    if(exclude_rows |> length() > 0){coordinates_df_sub <- coordinates_df_sub[-exclude_rows,]}
    }
  
  if(longest_fix_only == T){
      coordinates_df_sub <- coordinates_df_sub |> 
        group_by(.data[[id_col]], trial) |>
        filter(.data[[gaze_event_dur_col]] == max(.data[[gaze_event_dur_col]], na.rm = T)) |> 
        ungroup()
    }

    prec_rms <- coordinates_df_sub |> 
      group_by(.data[[id_col]], .data[[gaze_event_index_col]], trial) |> 
      mutate(diff_coord_x = c(diff(.data[[x]]), NA),
             diff_coord_y = c(diff(.data[[y]]), NA)) |>
      mutate(eucl_dist = sqrt(diff_coord_x^2 + diff_coord_y^2)) |> 
      slice(-n()) |> 
      drop_na(eucl_dist) |>
      summarize(precrms = sqrt(sum(eucl_dist^2) / n())) |>  # manually checked for one data example (in former version, but should still be fine)
      ungroup()

  return(prec_rms)
}

# Precision SD ----
# Formula: https://dl.acm.org/doi/pdf/10.1145/2168556.2168563
# Default setting correspond to REJOINT
calculate_precision_sd <- function(df, media_col = "Presented.Media.name", gaze_event_col = "Eye.movement.type", id_col = "Recording.name",
                                   stimulus_vec = c("ATTENTION_Familiarization.mp4", "ATTENTION_Preflooking.mp4"),
                                   gaze_event_dur_col = "Gaze.event.duration", gaze_event_index_col = "Eye.movement.type.index",
                                   x_fix = "Fixation.point.X", y_fix = "Fixation.point.Y", x = "Gaze.point.X", y = "Gaze.point.Y", 
                                   screen_height = 1080, screen_width = 1920, aoi_buffer_px_x = 40, aoi_buffer_px_y = 40,
                                   xmin = 849, xmax = 1072, ymin = 417, ymax = 672,
                                   off_exclude = F, longest_fix_only = F, AOI_only = F){
  
  coordinates_df_sub <- df |> 
    filter(.data[[media_col]] %in% stimulus_vec) |> 
    filter(.data[[gaze_event_col]] == "Fixation")

  if(off_exclude == T){
    # Exclude all fixations whose coordinates are outside the screen AOI
    exclude_rows <- which(coordinates_df_sub[[x_fix]] < 0 | coordinates_df_sub[[x_fix]] > screen_width + aoi_buffer_px_x |
                            coordinates_df_sub[[y_fix]] < 0 | coordinates_df_sub[[y_fix]] > screen_height + aoi_buffer_px_y)
    if(exclude_rows |> length() > 0){coordinates_df_sub <- coordinates_df_sub[-exclude_rows,]}
  }
  
  if(AOI_only == T){
    # Exclude all fixations whose coordinates are outside the target AOI
    exclude_rows <- which(coordinates_df_sub[[x_fix]] < xmin - aoi_buffer_px_x | coordinates_df_sub[[x_fix]] > xmax + aoi_buffer_px_x |
                            coordinates_df_sub[[y_fix]] < ymin -  aoi_buffer_px_y | coordinates_df_sub[[y_fix]] > ymax + aoi_buffer_px_y )
    if(exclude_rows |> length() > 0){coordinates_df_sub <- coordinates_df_sub[-exclude_rows,]}
  }
  
  if(longest_fix_only == T){
    coordinates_df_sub <- coordinates_df_sub |> 
      group_by(.data[[id_col]], trial) |>
      filter(.data[[gaze_event_dur_col]] == max(.data[[gaze_event_dur_col]], na.rm = T)) |> 
      ungroup()
  }
    
    prec_sd <- coordinates_df_sub |> 
      select({{x}}, {{y}}, {{gaze_event_index_col}}, {{id_col}}, trial) |> 
      group_by(.data[[id_col]], .data[[gaze_event_index_col]], trial) |> 
      mutate(diff_coord_x = c(diff(.data[[x]]), NA),
             diff_coord_y = c(diff(.data[[y]]), NA)) |>
      mutate(eucl_dist = sqrt(diff_coord_x^2 + diff_coord_y^2)) |> 
      summarize(precsd = sd(eucl_dist, na.rm = T)) |> # manually checked for one data example (in former version, but should still be fine)
      ungroup()

  return(prec_sd)
}

# Robustness ----
#' Calculate robustness per trial.
#'
#' Robustness is defined as the mean length (ms) of consecutive valid gaze samples.
#' A "valid chunk" is a run of consecutive rows classified as valid.
#' Each chunk duration is the sum of `gaze_sample_duration` within that chunks.
#' The robustness per trial is the mean duration of all valid chunks within that trial.
#'
#' @param df A data frame (or tibble) with gaze samples.
#' @param trial_col Name of the trial column.
#' @param gaze_x_col Name of the x gaze column (numeric).
#' @param gaze_y_col Name of the y gaze column (numeric).
#' @param sample_duration_col Name of the gaze sample duration column (ms; numeric).
#' @param blink_left_col Name of the left blink-detection column.
#' @param blink_right_col Name of the right blink-detection column.
#' @param blink_removal Logical. If `TRUE`, blink samples are "removed" by setting
#'   x/y to `blink_replacement_value` where a blink is detected.
#' @param blink_label Value in blink columns indicating a blink (default: `"blink"`).
#' @param blink_replacement_value Numeric value written into x/y during blink removal
#'   (default: `99999`).
#' @param validity_col Name of the new validity column to create (default: `"sample_validity"`).
#'
#' @return A tibble with exactly one row per trial: `trial` and `robustness_ms`.
#'
#' @examples
#' # robustness_tbl <- calculate_robustness(df)
#'
calculate_robustness <- function(
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
    validity_col          = "sample_validity"
) {
  
  # Basic Checks
  needed <- c(trial_col, gaze_x_col, gaze_y_col, sample_duration_col, blink_left_col, blink_right_col)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Blink Removal
  if (isTRUE(blink_removal)) {
    # Identify blink rows
    bl_left  <- df[[blink_left_col]]
    bl_right <- df[[blink_right_col]]
    is_blink <- (!is.na(bl_left)  & bl_left == blink_label) |
      (!is.na(bl_right) & bl_right == blink_label)
    
    # Set x/y to replacement value on blink rows
    df[[gaze_x_col]] <- ifelse(is_blink, blink_replacement_value, df[[gaze_x_col]])
    df[[gaze_y_col]] <- ifelse(is_blink, blink_replacement_value, df[[gaze_y_col]])
  }
  
  # Add Validity Column
  x <- df[[gaze_x_col]]
  y <- df[[gaze_y_col]]
  
  valid_flag <- !is.na(x) | !is.na(y)
  
  df[[validity_col]] <- ifelse(valid_flag, "valid", "missing")
  
  # Calculate Robustness
  trial_vec <- df[[trial_col]]
  
  run_id <- integer(nrow(df))
  run_id[] <- NA_integer_
  
  split_idx <- split(seq_len(nrow(df)), trial_vec)
  for (idx in split_idx) {
    v <- valid_flag[idx]
    starts <- v & !dplyr::lag(v, default = FALSE)
    rid <- cumsum(starts)
    run_id[idx] <- ifelse(v, rid, NA_integer_)
  }
  df[[".valid_run_id"]] <- run_id
  
  robustness_tbl <- df |>
    dplyr::filter(.data[[validity_col]] == "valid") |>
    dplyr::group_by(.data[[trial_col]], .data[[".valid_run_id"]]) |>
    dplyr::summarise(run_duration_ms = sum(.data[[sample_duration_col]], na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::group_by(.data[[trial_col]]) |>
    dplyr::summarise(
      robustness_ms = if (dplyr::n() == 0) NA_real_ else mean(run_duration_ms, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(trial = !!trial_col)
  
  all_trials <- dplyr::tibble(trial = unique(df[[trial_col]]))
  robustness_tbl <- dplyr::left_join(all_trials, robustness_tbl, by = "trial") |>
    dplyr::arrange(trial)
  
  robustness_tbl
}
