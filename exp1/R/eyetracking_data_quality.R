# todo: check in the end whether all arguments are used in the function
# todo: discuss drop_na(eucl_dist) mit Maleen

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
