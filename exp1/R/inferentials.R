#' Compute RQ1 marginal predictions and pairwise contrasts on the response scale
#'
#' Fits post-estimation summaries for a \code{brms} model used in RQ1 by
#' extracting (a) model-based marginal predictions per group level and
#' (b) pairwise contrasts between group levels, both on the response scale.
#'
#' The function currently assumes that the grouping variable is \code{folder}
#' (e.g., 4m, 6m, 9m, 18m, adults, chimps) and returns population-level
#' estimates (\code{re_formula = NA}).
#'
#' @param model A fitted model object (typically a \code{brmsfit}) compatible
#'   with \code{marginaleffects::avg_predictions()} and
#'   \code{marginaleffects::comparisons()}.
#' @param outcome_name A character string used to label the outcome in the
#'   returned tables (e.g., \code{"Accuracy"}, \code{"Precision_RMS"}).
#'
#' @return A named list with two tibbles:
#'   \describe{
#'     \item{\code{avg}}{Model-based marginal predictions by \code{folder}
#'       on the response scale, with an added column \code{outcome}.}
#'     \item{\code{cmp}}{Pairwise contrasts between \code{folder} levels
#'       on the response scale, with an added column \code{outcome}.}
#'   }
#'
#' @details
#' \code{type = "response"} returns estimates on the outcome scale (not the
#' link scale). \code{re_formula = NA} excludes group-level effects and thus
#' yields population-level marginal summaries.
#'
#' @examples
#' \dontrun{
#' res_acc <- get_rq1_marginals(full_rq1_acc, "Accuracy")
#' res_acc$avg
#' res_acc$cmp
#' }
#'
#' @seealso
#' \code{\link[marginaleffects:avg_predictions]{marginaleffects::avg_predictions}},
#' \code{\link[marginaleffects:comparisons]{marginaleffects::comparisons}}
#'
#' @export
get_rq1_marginals <- function(model, outcome_name) {
  avg <- marginaleffects::avg_predictions(
    model,
    by = "folder",
    type = "response",
    re_formula = NA
  ) |>
    as_tibble() |>
    mutate(outcome = outcome_name)
  
  cmp <- marginaleffects::comparisons(
    model,
    variables = "folder",
    type = "response",
    re_formula = NA
  ) |>
    as_tibble() |>
    mutate(outcome = outcome_name)
  
  list(avg = avg, cmp = cmp)
}
