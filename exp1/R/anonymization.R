#' Anonymize identifier columns in a single file
#' @param file Path to the input file (TSV).
#' @param id A single ID (used for all id_cols) or a vector of IDs with the same length as `id_cols`.
#' @param id_cols Character vector of column names to be anonymized.
#' @param drop_cols Character vector of column names to drop (e.g., columns containing personally identifiable information).
#' @return A tibble with anonymized IDs (and optionally dropped columns).
#' @export
#' ## Nov 17 2025 – Daniela Schmidt

anonymize_file <- function(file = here("folder", "file.tsv"),
                           id = 1,
                           id_cols  = c("Recording.name", "Participant.name"),
                           drop_cols = character()) {
  # read data
  df <- read.table(file, header = T, sep = "\t")
  
  # basic checks
  if (length(id_cols) < 1) {
    stop("Please provide at least one column name in `id_cols`.")
  }
  
  # build a named list of replacement values
  #
  # case 1: `id` is length 1 -> use the same id for all id_cols
  if (length(id) == 1L) {
    id_list <- setNames(as.list(rep(id, length(id_cols))), id_cols)
    
    # case 2: `id` has same length as id_cols -> one id per column
  } else if (length(id) == length(id_cols)) {
    id_list <- setNames(as.list(id), id_cols)
    
  } else {
    stop("`id` must be either length 1 or the same length as `id_cols`.")
  }
  
  df <- df |>
    mutate(!!!id_list) |>
    select(-any_of(drop_cols))
  
  message("Done: ", basename(file))
  df
}