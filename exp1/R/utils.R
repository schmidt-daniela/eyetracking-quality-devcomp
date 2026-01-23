onepx_in_visd <- function(screen_distance_cm, screen_dpi) { # If you do not know the dpi, see: https://dpi.lv
  
  # Convert pixel to centimeter (Remark: 1 inch = 2.54 cm)
  numberofpixel_per_cm <- screen_dpi / 2.54 # The screen has n pixel per inch. 
                                            # If we divide it by 2.54, we know how many pixel the screen has per cm.
  onepixel_in_cm <- 1 / numberofpixel_per_cm # One cm has n pixel. We use Dreisatz to know how long one pixel is in cm.
  
  # Calculate the viewing angle in radians for one pixel
  # Formula from: https://rechneronline.de/sehwinkel/
  onepixel_in_rad <- 2 * atan(onepixel_in_cm / (2 * screen_distance_cm))
  
  # Convert radians for one pixel to degrees
  # Formula from: https://www.rapidtables.com/convert/number/radians-to-degrees.html 
  onepixel_in_visd <- round(onepixel_in_rad * (180 / pi), 5)
  
  return(onepixel_in_visd)
}

add_demo_cols <- function(d, df, folder) {
  if (folder != "adults") {
    d |>
      left_join(
        df |>
          select(
            trial, excluded_100ms, excluded_3sd, excluded_fixation,
            sex, age_ddd, order, experimenter,
            no_siblings, no_household, multilingual, kindergarten_yn, tagesmutter_yn
          ) |>
          distinct(),
        by = "trial"
      ) |>
      rename(age = age_ddd)
  } else {
    d |>
      left_join(
        df |>
          select(
            trial, excluded_100ms, excluded_3sd, excluded_fixation,
            age_md, sex, order, experimenter
          ) |>
          distinct(),
        by = "trial"
      ) |>
      rename(age = age_md)
  }
}

read_one_participant <- function(file_path) {
  filename <- basename(file_path)
  
  raw <- readRDS(file_path)
  
  raw |>
    mutate(
      group_id = str_remove(filename, "\\.rds$"),
      source_file = filename
    )
}

read_folder <- function(folder,
                        base_dir = here("exp1", "data", "preproc")) {
  folder_path <- file.path(base_dir, folder)
  
  files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(files) == 0) {
    warning("No .rds files found in: ", folder_path)
    return(tibble())
  }
  
  files |>
    sort() |>
    map_dfr(read_one_participant) |>
    mutate(folder = folder)
}

count_valid_trials <- function(dat, trial_id_col = "trial") {
  dat |>
    group_by(group_id) |>
    summarise(
      n_trials_total        = n_distinct(.data[[trial_id_col]]),
      n_valid_acc_visd      = n_distinct(.data[[trial_id_col]][!is.na(acc_visd)]),
      n_valid_precrms_visd  = n_distinct(.data[[trial_id_col]][!is.na(precrms_visd)]),
      n_valid_precsd_visd   = n_distinct(.data[[trial_id_col]][!is.na(precsd_visd)]),
      n_valid_robustness_ms = n_distinct(.data[[trial_id_col]][!is.na(robustness_ms)]),
      .groups = "drop"
    )
}