#' Mark an area of interest (AOI) in gaze data
#' Assigns an AOI label to all rows in a data frame where gaze coordinates
#' fall within a specified rectangular region and the stimulus matches
#' one of the given stimulus labels.
#' @param df A data frame containing at least the columns `fix_x`, `fix_y`,`stimulus`, and `aoi`.
#' @param name A character scalar giving the AOI label to assign (e.g., `"top_left"`).
#' @param x_min,x_max Numeric values specifying the minimum and maximum x-coordinates of the AOI.
#' @param y_min,y_max Numeric values specifying the minimum and maximum y-coordinates of the AOI.
#' @param stimuli A character vector of stimulus names for which the AOI should be applied (e.g., `"checkflake"` or `c("move", "still")`).
#' @return The input data frame `df` with the `aoi` column updated: all rows
#'   falling within the specified region (and with `stimulus` in `stimuli`) are set to `name`.
#' @examples
#' df$aoi <- "not_in_aoi"
#' df <- mark_aoi(df,
#'                name   = "top_left",
#'                x_min  = 800,
#'                x_max  = 1000,
#'                y_min  = 100,
#'                y_max  = 300,
#'                stimuli = "checkflake")
#' ## Nov 17 2025 – Daniela Schmidt
mark_aoi <- function(df, name, x_min, x_max, y_min, y_max, stimuli) {
  idx <- df$fix_x >= x_min & df$fix_x <= x_max &
    df$fix_y >= y_min & df$fix_y <= y_max &
    df$stimulus %in% stimuli
  
  df$aoi[idx] <- name
  df
}
