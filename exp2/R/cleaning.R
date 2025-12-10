#' Project-specific fix for export/naming mistakes
#'
#' NOTE: This function is NOT meant to be reusable across projects.
#' It implements a very specific fix for THIS project only:
#' - For the file "alex.tsv", one particular recording should be removed.
#' - For that same file, "session" should be renamed to "session2".
#'
#' @param df        Data frame containing a column `Recording.name`.
#' @param j File name as a string, e.g., "alex.tsv".
#' @return Data frame with project-specific corrections applied.
# correct_export_mistakes <- function(df, j) {
#   if (identical(j, "carolatsv")) {
#     df <- df |>
#       dplyr::filter(Recording.name != "session1_carola_alex") |>
#       dplyr::mutate(
#         Recording.name = stringr::str_replace(
#           Recording.name,
#           "\\bsession\\b",
#           "session2"
#         )
#       ) |>
#       dplyr::arrange(Recording.name)
#   }
#   df
# }