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

#' Detect blink-related samples and blink events in pupil data
#'
#' @param df A data frame with at least the columns:
#'   `recording_name`, `trial`, `timeline_trial_units`, `pupil_diameter_average`.
#' @param nxmedian_neg_velocity_thresh Multiplier for the dataset-wide median
#'   absolute velocity to define the negative velocity threshold
#'   (threshold = -nxmedian_neg_velocity_thresh * median(|velocity|)).
#' @param nxmedian_pos_velocity_thresh Multiplier for the dataset-wide median
#'   absolute velocity to define the positive velocity threshold
#'   (threshold =  nxmedian_pos_velocity_thresh * median(|velocity|)).
#' @param max_blink_dur Maximum allowed blink duration, operationalised as
#'   the duration of the NA block in `pupil_diameter_average`, in the same
#'   time unit as `timeline_trial_units` (e.g., 0.25 = 250 ms if in seconds).
#' @param min_blink_dur Minimum blink duration (duration of the NA block)
#'   in the same time unit as `timeline_trial_units`
#'   (e.g., 0.01 = 10 ms if in seconds).
#' @param n_window Integer. Number of samples directly before and after an
#'   NA block within which velocity must cross the negative (onset) and
#'   positive (offset) thresholds for the NA block to be classified as a blink.
#' @param apply_moving_average Logical. If TRUE, apply a moving-average
#'   filter to `pupil_diameter_average` for the velocity calculation.
#'   NA values remain NA and are not imputed.
#' @param ma_window Integer. Window size (in samples) for the moving-average
#'   filter used when `apply_moving_average = TRUE`.
#'
#' @return The original data frame plus:
#'   `velocity_pupil`, `blinks_onset_offset`, `blink_detected`.
#'
#' @details
#' Requires the package `dplyr`.
#' Velocity thresholds are computed data-driven as multiples of the
#' median absolute velocity over the entire input data frame.
#' If `apply_moving_average = TRUE`, velocity is computed on an
#' NA-preserving moving-average version of `pupil_diameter_average`.
annotate_blinks <- function(df,
                            nxmedian_neg_velocity_thresh,
                            nxmedian_pos_velocity_thresh,
                            max_blink_dur = 272,
                            min_blink_dur = 10,
                            n_window = 5,
                            apply_moving_average = FALSE,
                            ma_window = 5) {
  
  # Dependency check
  required_pkgs <- c("dplyr")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required but not installed.\n",
        "Please install it, e.g.: install.packages('", pkg, "')",
        call. = FALSE
      )
    }
  }
  
  # --- Helper: NA-preserving moving average -------------------------------
  ma_na_preserve <- function(x, k) {
    if (k < 1L) stop("ma_window must be >= 1.")
    n <- length(x)
    if (k == 1L) return(x)
    
    half <- floor((k - 1L) / 2L)
    res  <- rep(NA_real_, n)
    
    for (i in seq_len(n)) {
      if (is.na(x[i])) {
        # Do not impute NAs
        res[i] <- NA_real_
      } else {
        from   <- max(1L, i - half)
        to     <- min(n, i + half)
        window <- x[from:to]
        window <- window[!is.na(window)]
        if (length(window) == 0L) {
          res[i] <- NA_real_
        } else {
          res[i] <- mean(window)
        }
      }
    }
    res
  }
  
  # 1) Velocity (no blink flag yet)
  pupil_blink_base <- df |>
    dplyr::group_by(recording_name, trial) |>
    dplyr::mutate(
      # store the moving-average *explicitly* for inspection
      pupil_ma = if (apply_moving_average) {
        ma_na_preserve(pupil_diameter_average, ma_window)
      } else {
        NA_real_
      },
      # use either the moving average (if requested) or the raw pupil for velocity
      pupil_for_velocity = if (apply_moving_average) {
        pupil_ma
      } else {
        pupil_diameter_average
      },
      dp = pupil_for_velocity - dplyr::lag(pupil_for_velocity),
      dt = timeline_trial_units    - dplyr::lag(timeline_trial_units),
      velocity_pupil = dplyr::case_when(
        is.na(dp) | is.na(dt) ~ NA_real_,
        dt == 0               ~ NA_real_,
        TRUE                  ~ dp / dt
      )
    ) |>
    dplyr::ungroup()
  
  # 2) Data-driven thresholds
  vel_abs_median <- stats::median(abs(pupil_blink_base$velocity_pupil),
                                  na.rm = TRUE)
  
  message(
    "Threshold for ",
    pupil_blink_base$participant_name |> unique(),
    ": ",
    nxmedian_neg_velocity_thresh * vel_abs_median
  )
  
  if (is.na(vel_abs_median) || vel_abs_median == 0) {
    stop(
      "Cannot compute median absolute velocity (all NA or zero); ",
      "data-driven thresholds unavailable."
    )
  }
  
  neg_velocity_thresh <- -nxmedian_neg_velocity_thresh * vel_abs_median
  pos_velocity_thresh <-  nxmedian_pos_velocity_thresh * vel_abs_median
  
  # 3) Mark large |velocity| samples (diagnostic only)
  pupil_blink_base <- pupil_blink_base |>
    dplyr::mutate(
      blinks_onset_offset = ifelse(
        !is.na(velocity_pupil) &
          (velocity_pupil <= neg_velocity_thresh |
             velocity_pupil >= pos_velocity_thresh),
        1L, 0L
      )
    ) |>
    dplyr::select(-dp, -dt)  # keep pupil_ma and pupil_for_velocity
  
  
  # 4) Blink event detection, per recording_name × trial
  detect_blinks_in_group <- function(df_grp,
                                     max_blink_dur,
                                     min_blink_dur,
                                     neg_velocity_thresh,
                                     pos_velocity_thresh,
                                     n_window) {
    n <- nrow(df_grp)
    blink_detected <- rep("no", n)
    
    i <- 1L
    while (i <= n) {
      
      if (is.na(df_grp$pupil_diameter_average[i])) {
        # start of an NA block
        first_na <- i
        last_na  <- i
        while (last_na + 1L <= n &&
               is.na(df_grp$pupil_diameter_average[last_na + 1L])) {
          last_na <- last_na + 1L
        }
        
        # --- look BACK n_window samples for NEGATIVE velocity ---
        pre_from <- max(1L, first_na - n_window)
        pre_to   <- first_na - 1L
        
        has_neg_peak <- FALSE
        if (pre_from <= pre_to) {
          idx_pre <- pre_from:pre_to
          has_neg_peak <- any(
            !is.na(df_grp$velocity_pupil[idx_pre]) &
              df_grp$velocity_pupil[idx_pre] <= neg_velocity_thresh
          )
        }
        
        # --- look FORWARD n_window samples for POSITIVE velocity ---
        post_from <- last_na + 1L
        post_to   <- min(n, last_na + n_window)
        
        has_pos_peak <- FALSE
        if (post_from <= post_to) {
          idx_post <- post_from:post_to
          has_pos_peak <- any(
            !is.na(df_grp$velocity_pupil[idx_post]) &
              df_grp$velocity_pupil[idx_post] >= pos_velocity_thresh
          )
        }
        
        # --- Blink duration = duration of NA block ---
        blink_dur <- df_grp$timeline_trial_units[last_na] -
          df_grp$timeline_trial_units[first_na]
        
        # --- Accept blink if all conditions met ---
        if (has_neg_peak &&
            has_pos_peak &&
            !is.na(blink_dur) &&
            blink_dur >= min_blink_dur &&
            blink_dur <= max_blink_dur) {
          
          # mark NA rows
          blink_detected[first_na:last_na] <- "yes"
          
          # also mark the first valid sample after the NA block as blink
          first_non_na <- last_na + 1L
          if (first_non_na <= n &&
              !is.na(df_grp$pupil_diameter_average[first_non_na])) {
            blink_detected[first_non_na] <- "yes"
          }
        }
        
        # jump to first row after this NA block
        i <- last_na + 1L
        next
      }
      
      i <- i + 1L
    }
    
    df_grp$blink_detected <- blink_detected
    df_grp
  }
  
  result <- pupil_blink_base |>
    dplyr::group_by(recording_name, trial) |>
    dplyr::group_modify(~ detect_blinks_in_group(
      .x,
      max_blink_dur       = max_blink_dur,
      min_blink_dur       = min_blink_dur,
      neg_velocity_thresh = neg_velocity_thresh,
      pos_velocity_thresh = pos_velocity_thresh,
      n_window            = n_window
    )) |>
    dplyr::ungroup()
  
  return(result)
}
