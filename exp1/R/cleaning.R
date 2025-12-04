#' Create sequential trial IDs based on changes in stimulus position.
#' This function walks through a vector of stimuli and their corresponding
#' positions and assigns a trial ID that increases whenever the
#' \code{stimulus_position} changes. Rows with \code{NA} in \code{stimulus}
#' do not start a new trial and simply inherit the current trial ID.
#'
#' @param stimulus A vector (character, factor, or similar) indicating the stimulus type per row. \code{NA} values are allowed and will *not* start a new trial.
#' @param stimulus_position A vector (same length as \code{stimulus}) specifying the position/category that defines a trial boundary. 
#' Whenever this value changes (compared to the previous non-\code{NA} stimulus), a new trial ID is started.
#' @param start_at Integer, the value of the first trial ID. Defaults to 1.
#'
#' @return An integer vector of trial IDs with the same length as \code{stimulus}. Rows before the first non-\code{NA} stimulus will
#'   receive \code{start_at - 1} (often 0); you can turn those into \code{NA} afterwards if desired.
#'
#' @examples
#' # Simple example
#' stim  <- c(NA, "at", "cueing", "object", "object", "at", "object")
#' pos   <- c(NA, "center", "top", "top", "top", "center", "bottom")
#' make_trial_num(stim, pos)
#' #' ## Nov 20 2025 – Daniela Schmidt
make_trial_num <- function(stimulus, stimulus_position, start_at = 1L) {
  if (length(stimulus) != length(stimulus_position)) {
    stop("`stimulus` and `stimulus_position` must have the same length.")
  }
  
  n          <- length(stimulus)
  trial      <- integer(n)
  current_id <- start_at - 1L
  last_pos   <- NA_character_
  
  for (i in seq_len(n)) {
    this_stim <- stimulus[i]
    this_pos  <- stimulus_position[i]
    
    if (is.na(this_stim)) {
      # NA-stimuli: do not start a new trial, keep current_id
      trial[i] <- current_id
      
    } else if (is.na(last_pos) || is.na(this_pos) || this_pos != last_pos) {
      # first valid stimulus OR position change (incl. -> NA)
      current_id <- current_id + 1L
      trial[i]   <- current_id
      last_pos   <- this_pos
      
    } else {
      # same position as previous -> same trial
      trial[i] <- current_id
    }
  }
  
  trial
}

#' Mark an area of interest (AOI) in gaze data.
#' Assigns an AOI label to all rows in a data frame where gaze coordinates
#' fall within a specified rectangular region and the stimulus matches
#' one of the given stimulus labels.
#'
#' @param df A data frame containing at least the columns for x/y gaze coordinates, `stimulus`, and an AOI column.
#' @param name A character scalar giving the AOI label to assign (e.g., `"top_left"`).
#' @param x_min,x_max Numeric values specifying the minimum and maximum x-coordinates of the AOI.
#' @param y_min,y_max Numeric values specifying the minimum and maximum y-coordinates of the AOI.
#' @param stimulus_name A character vector of stimulus names for which the AOI should be applied.
#' @param position_name A character vector of position labels for which the AOI should be applied.
#' @param x_col,y_col Character scalars giving the column names in `df` that contain the x- and y-coordinates, respectively. 
#' Defaults are `"fixation_point_x"` and `"fixation_point_y"` (determine AOIs based on fixations), 
#' but you can set them to e.g. `"gaze_point_x"` / `"gaze_point_y"` (determine AOIs based on gaze samples).
#' @param aoi_col A character scalar giving the name of the AOI column to update/create. Defaults to `"aoi"`.
#'
#' @return The input data frame `df` with the AOI column (`aoi_col`) updated: all rows
#'   falling within the specified region (and with `stimulus` in `stimulus_name`
#'   and `position` in `position_name`) are set to `name`.
#'
#' @examples
#' df$aoi <- "not_in_aoi"
#' df <- mark_aoi(
#'   df,
#'   name          = "top_left",
#'   x_min         = 800,
#'   x_max         = 1000,
#'   y_min         = 100,
#'   y_max         = 300,
#'   stimulus_name = "checkflake",
#'   position_name = "top_left",
#'   aoi_col = "aoi"
#' )
#' ## Nov 21 2025 – Daniela Schmidt
mark_aoi <- function(df, name, x_min, x_max, y_min, y_max, stimulus_name, position_name,
                     x_col  = "fixation_point_x", y_col  = "fixation_point_y", aoi_col = "aoi") {
  
  x <- df[[x_col]]
  y <- df[[y_col]]
  
  idx <- x >= x_min & x <= x_max &
    y >= y_min & y <= y_max &
    df$stimulus %in% stimulus_name &
    df$position %in% position_name
  
  df[[aoi_col]][idx] <- name
  df
}

#' Standardize session names in a recording column
#'
#' Converts e.g. "session1" to "session01", analogously up to "session9"
#' → "session09". In contrast to `correct_export_mistakes()`, this function
#' is potentially reusable in other projects.
#'
#' @param df            Data frame with a column that contains session names.
#' @param recording_col Name of that column as a string (default: "Recording.name").
#' @return Data frame with standardized session labels.
correct_session_name <- function(df, recording_col = "Recording.name") {
  col_sym <- rlang::sym(recording_col)
  
  df |>
    dplyr::mutate(
      !!col_sym := stringr::str_replace(!!col_sym, "\\bsession1\\b", "session01"),
      !!col_sym := stringr::str_replace(!!col_sym, "\\bsession2\\b", "session02"),
      !!col_sym := stringr::str_replace(!!col_sym, "\\bsession3\\b", "session03"),
      !!col_sym := stringr::str_replace(!!col_sym, "\\bsession4\\b", "session04"),
      !!col_sym := stringr::str_replace(!!col_sym, "\\bsession5\\b", "session05"),
      !!col_sym := stringr::str_replace(!!col_sym, "\\bsession6\\b", "session06"),
      !!col_sym := stringr::str_replace(!!col_sym, "\\bsession7\\b", "session07"),
      !!col_sym := stringr::str_replace(!!col_sym, "\\bsession8\\b", "session08"),
      !!col_sym := stringr::str_replace(!!col_sym, "\\bsession9\\b", "session09")
    )
}

#' Detect and interpolate blinks in pupil data
#'
#' @param df A data frame with at least the columns:
#'   `recording_name`, `trial`, `timeline_trial`, `pupil_diameter_filtered`.
#' @param neg_velocity_thresh Negative velocity threshold for blink onset.
#' @param pos_velocity_thresh Positive velocity threshold for blink offset.
#' @param fillback Number of samples to extend the blink backwards.
#' @param fillforward Number of samples to extend the blink forwards.
#' @param hz Sampling rate in Hz (passed to extend_blinks()).
#'
#' @return The original data frame plus:
#'   `smooth_pupil`, `velocity_pupil`, `blinks_onset_offset`,
#'   `blinks_pupil`, `extendpupil`, `interp`.
#'
#' @details
#' Requires the packages `dplyr`, `zoo`, and `gazer`.
#' Uses `gazer::moving_average_pupil()` and `gazer::extend_blinks()`.
blink_velocity_adj <- function(df, neg_velocity_thresh = -5, pos_velocity_thresh = 5,
                               fillback = 0, fillforward = 0, hz = 120) {
  
  # Dependency check
  required_pkgs <- c("dplyr", "zoo", "gazer")
  
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (pkg == "gazer") {
        stop(
          "Package 'gazer' is required but not installed.\n",
          "Please install it first, e.g.:\n",
          "  install.packages('remotes')\n",
          "  remotes::install_github('dmirman/gazer')\n",
          call. = FALSE
        )
      } else {
        stop(
          "Package '", pkg, "' is required but not installed.\n",
          "Please install it, e.g.: install.packages('", pkg, "')",
          call. = FALSE
        )
      }
    }
  }
  
  # Blink detection and interpolation
  pupil_blink_algo <- df |>
    dplyr::group_by(recording_name, trial) |>
    dplyr::mutate(dp = pupil_diameter_filtered - dplyr::lag(pupil_diameter_filtered),
                  dt = timeline_trial - dplyr::lag(timeline_trial),
                  velocity_pupil = dplyr::case_when(is.na(dp) | is.na(dt) ~ NA_real_, dt == 0 ~ NA_real_, TRUE ~ dp / dt),
                  blinks_onset_offset = ifelse(!is.na(velocity_pupil) & (velocity_pupil <= neg_velocity_thresh | velocity_pupil >= pos_velocity_thresh), 1L, 0L),
                  blinks_pupil = ifelse(blinks_onset_offset == 1L, NA_real_, pupil_diameter_filtered)) |>
    dplyr::ungroup() |>
    dplyr::select(-dp, -dt)
  
  pup_extend <- pupil_blink_algo |>
    dplyr::group_by(recording_name, trial) |>
    dplyr::mutate(
      extendpupil = gazer::extend_blinks(
        blinks_pupil,
        fillback   = fillback,
        fillforward = fillforward,
        hz         = hz
      ),
      interp = zoo::na.approx(
        extendpupil,
        na.rm = FALSE,
        rule  = 2
      )
    ) |>
    dplyr::ungroup()
  
  return(pup_extend)
}

