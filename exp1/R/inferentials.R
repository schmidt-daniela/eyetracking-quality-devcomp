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

#' Extract model-implied group means or contrasts on the response scale
#'
#' Unified helper for brms models that encode groups as separate coefficients
#' (e.g., via `0 + folder` leading to parameters like `b_folder4m`, `b_folderadults`).
#' Works for common links used in your pipeline:
#' - Gamma with log link: back-transform via exponential function
#' - Beta with logit link: back-transform via inverse-logit (logistic) function
#'
#' Supports two output types:
#' - `type = "means"`: group-wise model-implied means (Gamma) or mean parameter μ (Beta)
#' - `type = "contrasts"`: pairwise contrasts vs a reference or all pairwise contrasts
#'
#' For contrasts:
#' - log link: returns ratios of response-scale means by default
#' - logit link: returns differences in μ by default (risk difference), or odds ratios if requested
#'
#' @param fit A `brmsfit` object.
#' @param groups Character vector of group level names (e.g., c("4m","6m","9m","18m","adults","chimps")).
#' @param group_prefix Prefix of the coefficient name in the brms fit (default: "folder").
#'   Coefficients are assumed to be named `b_<group_prefix><group>`.
#' @param type One of `"means"` or `"contrasts"`.
#' @param ref Optional single reference group name. If provided, contrasts are computed as `g vs ref`.
#'   If `NULL` and `type = "contrasts"`, all pairwise contrasts are returned (excluding self-comparisons).
#' @param link Link function to use: `"log"` or `"logit"`. If `NULL`, attempts to infer from `fit`.
#'   For robustness (Beta), you typically want `"logit"`. For accuracy/precision (Gamma), `"log"`.
#' @param contrast_scale For `type = "contrasts"`:
#'   - If link = "log": `"ratio"` (default) or `"difference"` (difference on response scale).
#'   - If link = "logit": `"difference"` (default; difference in μ) or `"odds_ratio"` (exp of Δη).
#' @param probs Numeric vector of length 2 with lower/upper interval probs (default: c(.025, .975)).
#' @param summary_fn Function used to summarize posterior draws (default: median).
#'
#' @return A tibble.
#' - If `type = "means"`: columns `group`, `estimate`, `lo`, `hi`
#' - If `type = "contrasts"`: columns depend on `contrast_scale`:
#'   e.g., `ratio_median`, `ratio_lo`, `ratio_hi` or `diff_median`, ...
#'
#' @examples
#' \dontrun{
#' # Gamma(log): contrasts vs adults as ratios
#' brms_group_effects_response(full_rq1_acc,
#'   groups = c("4m","6m","9m","18m","adults","chimps"),
#'   type = "contrasts", ref = "adults", link = "log", contrast_scale = "ratio"
#' )
#'
#' # Beta(logit): robustness contrasts vs adults as differences in mu
#' brms_group_effects_response(full_rq1_rob,
#'   groups = c("4m","6m","9m","18m","adults","chimps"),
#'   type = "contrasts", ref = "adults", link = "logit", contrast_scale = "difference"
#' )
#'
#' # Beta(logit): robustness as odds ratios (less intuitive; optional)
#' brms_group_effects_response(full_rq1_rob,
#'   groups = c("4m","6m","9m","18m","adults","chimps"),
#'   type = "contrasts", ref = "adults", link = "logit", contrast_scale = "odds_ratio"
#' )
#'
#' # Model-implied means / mu by group
#' brms_group_effects_response(full_rq1_acc,
#'   groups = c("4m","6m","9m","18m","adults","chimps"),
#'   type = "means", link = "log"
#' )
#' }
#' @export
brms_group_effects_response <- function(
    fit,
    groups,
    group_prefix = "folder",
    type = c("means", "contrasts"),
    ref = NULL,
    link = NULL,
    contrast_scale = NULL,
    probs = c(.025, .975),
    summary_fn = stats::median
) {
  stopifnot(inherits(fit, "brmsfit"))
  stopifnot(is.character(groups), length(groups) >= 2)
  stopifnot(length(probs) == 2)
  
  type <- match.arg(type)
  
  # --- helper: infer link (best effort) ---
  infer_link <- function(fit) {
    # brms stores link in family; for some families it's accessible as fit$family$link
    lk <- tryCatch(fit$family$link, error = function(e) NA_character_)
    if (!is.na(lk) && nzchar(lk)) return(lk)
    NA_character_
  }
  
  if (is.null(link)) link <- infer_link(fit)
  if (!link %in% c("log", "logit")) {
    stop("`link` must be 'log' or 'logit' (or inferable from the brms fit). Got: ", link)
  }
  
  # defaults for contrast_scale
  if (is.null(contrast_scale)) {
    contrast_scale <- if (link == "log") "ratio" else "difference"
  }
  if (type == "means" && !is.null(ref)) {
    stop("`ref` is only used for `type = 'contrasts'`.")
  }
  
  # --- helper: inverse link ---
  inv_link <- function(eta) {
    if (link == "log") return(exp(eta))
    if (link == "logit") return(stats::plogis(eta))
    stop("Unsupported link.")
  }
  
  # --- pull posterior draws ---
  draws <- posterior::as_draws_df(fit)
  coef_names <- paste0("b_", group_prefix, groups)
  missing <- setdiff(coef_names, names(draws))
  if (length(missing) > 0) {
    stop("Missing coefficients in posterior draws: ", paste(missing, collapse = ", "))
  }
  
  # --- means ---
  if (type == "means") {
    out <- lapply(groups, function(g) {
      eta <- draws[[paste0("b_", group_prefix, g)]]
      y   <- inv_link(eta)
      tibble::tibble(
        group = g,
        estimate = summary_fn(y),
        lo = stats::quantile(y, probs[1]),
        hi = stats::quantile(y, probs[2])
      )
    })
    return(dplyr::bind_rows(out))
  }
  
  # --- contrasts ---
  if (!is.null(ref)) {
    if (!ref %in% groups) stop("`ref` must be one of `groups`.")
    pairs <- tibble::tibble(g1 = setdiff(groups, ref), g2 = ref)
  } else {
    pairs <- tidyr::expand_grid(g1 = groups, g2 = groups) |>
      dplyr::filter(g1 != g2) |>
      dplyr::distinct()
  }
  
  # validate contrast_scale
  valid_scales <- if (link == "log") c("ratio", "difference") else c("difference", "odds_ratio")
  if (!contrast_scale %in% valid_scales) {
    stop("For link='", link, "', contrast_scale must be one of: ",
         paste(valid_scales, collapse = ", "),
         ". Got: ", contrast_scale)
  }
  
  out <- pairs |>
    dplyr::rowwise() |>
    dplyr::do({
      g1 <- .$g1; g2 <- .$g2
      eta1 <- draws[[paste0("b_", group_prefix, g1)]]
      eta2 <- draws[[paste0("b_", group_prefix, g2)]]
      
      if (link == "log") {
        y1 <- exp(eta1); y2 <- exp(eta2)
        if (contrast_scale == "ratio") {
          z <- y1 / y2
          tibble::tibble(
            contrast = paste0(g1, " vs ", g2),
            ratio_median = summary_fn(z),
            ratio_lo = stats::quantile(z, probs[1]),
            ratio_hi = stats::quantile(z, probs[2])
          )
        } else { # difference
          z <- y1 - y2
          tibble::tibble(
            contrast = paste0(g1, " vs ", g2),
            diff_median = summary_fn(z),
            diff_lo = stats::quantile(z, probs[1]),
            diff_hi = stats::quantile(z, probs[2])
          )
        }
        
      } else { # link == "logit" (Beta)
        if (contrast_scale == "odds_ratio") {
          # odds ratio on the mean-odds scale: exp(eta1 - eta2)
          z <- exp(eta1 - eta2)
          tibble::tibble(
            contrast = paste0(g1, " vs ", g2),
            odds_ratio_median = summary_fn(z),
            odds_ratio_lo = stats::quantile(z, probs[1]),
            odds_ratio_hi = stats::quantile(z, probs[2])
          )
        } else {
          # difference in mu on response scale (risk difference)
          mu1 <- stats::plogis(eta1)
          mu2 <- stats::plogis(eta2)
          z <- mu1 - mu2
          tibble::tibble(
            contrast = paste0(g1, " vs ", g2),
            diff_median = summary_fn(z),
            diff_lo = stats::quantile(z, probs[1]),
            diff_hi = stats::quantile(z, probs[2])
          )
        }
      }
    }) |>
    dplyr::ungroup()
  
  out
}