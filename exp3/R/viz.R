# This function was coded by Luke Maurits in 2024 and adjusted by Daniela Schmidt in 2026.
plot_prior_vs_poster <- function(
    m,
    pars = c("b_folderchimps", "prior_b_folderchimps"),
    title = NULL,
    facet_label = "Chimpanzees"
) {
  posterior_draws <- as_draws_df(m)
  plot <- posterior_draws %>%
    select(-lprior, -`lp__`, -starts_with(".")) %>%
    select(all_of(pars)) |>
    pivot_longer(everything(), names_to="parameter", values_to="x") %>%
    mutate(distribution = if_else(str_starts(parameter, "prior_"), "prior", "posterior")) %>%
    mutate(parameter = if_else(str_starts(parameter, "prior_"), str_sub(parameter, 7, str_length(parameter)), parameter )) %>%
    mutate(parameter = if_else(parameter == "b_Intercept", "Intercept", parameter),
           parameter = if_else(parameter == "b", "b_x", parameter),
           parameter = facet_label
    ) %>% 
    ggplot() +
    geom_histogram(aes(x=x, fill=distribution), position="identity", alpha=0.5, bins=50) +
    facet_wrap(~parameter) +
    labs(
      title = title,
      fill  = "Distribution"
    ) +
    scale_fill_discrete(labels = c(prior = "Prior", posterior = "Posterior"))
  return(plot)
}

# Plot showing data quality across time in all tested groups
plot_rq2 <- function(
    df,
    # output
    png_name = "rq2_precsd_all.png",
    out_dir = here::here("exp1", "img"),
    width  = 2480 * 1.5,
    height = 3508,
    res    = 300,
    
    # variables
    group_var = "folder",
    x_var   = "time_1",
    y_var      = "precsd_visd",
    
    # chimp filtering + axis limits
    xmax_chimps = 8,
    filter_chimps_time_gt = TRUE,
    ymin_humans = 0,
    ymax_humans = 2,
    ymin_chimps = 3,
    ymax_chimps = 5,
    
    # labels
    ytitle = "Precision (SD)\nin visual degrees",
    x_label = "Time\n(trials in humans; sessions in apes)",

    lvl = c("4m", "6m", "9m", "18m", "adults", "chimps"),
    facet_labs = c(
      "4m"     = "4-Month-Olds",
      "6m"     = "6-Month-Olds",
      "9m"     = "9-Month-Olds",
      "18m"    = "18-Month-Olds",
      "adults" = "Adults",
      "chimps" = "Chimpanzees"
    )
) {
  # check dependencies
  stopifnot(is.data.frame(df))
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("rlang", quietly = TRUE)) {
    stop("Please install/load: dplyr, ggplot2, tidyr, rlang")
  }
  
  folder_sym <- rlang::sym(group_var)
  time_sym   <- rlang::sym(x_var)
  y_sym      <- rlang::sym(y_var)
  
  # basic checks
  needed <- c(group_var, x_var, y_var)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in df: ", paste(missing_cols, collapse = ", "))
  }
  if (!("chimps" %in% lvl)) {
    stop("lvl must include 'chimps' because limits/filtering depend on it.")
  }
  
  # build plotting data
  df_plot <- df |>
    dplyr::mutate(
      !!folder_sym := trimws(as.character(!!folder_sym)),
      !!folder_sym := factor(!!folder_sym, levels = lvl)
    ) |>
    dplyr::filter(!is.na(!!time_sym), !is.na(!!y_sym))
  
  if (isTRUE(filter_chimps_time_gt)) {
    df_plot <- df_plot |>
      dplyr::filter(!(!!folder_sym == "chimps" & !!time_sym > xmax_chimps))
  }
  
  # summarise means + CI per time point
  gg_sum <- df_plot |>
    dplyr::group_by(!!folder_sym, !!time_sym) |>
    dplyr::summarise(
      n    = sum(!is.na(!!y_sym)),
      mean = mean(!!y_sym, na.rm = TRUE),
      sd   = stats::sd(!!y_sym, na.rm = TRUE),
      se   = sd / sqrt(n),
      tcrit   = dplyr::if_else(n > 1, stats::qt(0.975, df = n - 1), as.numeric(NA)),
      ci_low  = mean - tcrit * se,
      ci_high = mean + tcrit * se,
      .groups = "drop"
    )
  
  # dummy limits for facet-specific x/y ranges (free scales)
  # humans (all except chimps): x from 0 to max observed, y fixed by args
  # chimps: x from 0 to xmax_chimps, y fixed by args
  humans_levels <- setdiff(lvl, "chimps")
  max_x_humans <- gg_sum |>
    dplyr::filter(!!folder_sym %in% humans_levels) |>
    dplyr::summarise(max_x = suppressWarnings(max(!!time_sym, na.rm = TRUE))) |>
    dplyr::pull(max_x)
  
  if (!is.finite(max_x_humans)) max_x_humans <- 0
  
  dummy_limits <- dplyr::bind_rows(
    dplyr::tibble(
      !!folder_sym := factor(humans_levels, levels = lvl),
      x_min = 0, x_max = max_x_humans,
      y_min = ymin_humans, y_max = ymax_humans
    ),
    dplyr::tibble(
      !!folder_sym := factor("chimps", levels = lvl),
      x_min = 0, x_max = xmax_chimps,
      y_min = ymin_chimps, y_max = ymax_chimps
    )
  ) |>
    tidyr::pivot_longer(
      cols = c(x_min, x_max, y_min, y_max),
      names_to = c(".value", "which"),
      names_pattern = "([xy])_(min|max)"
    ) |>
    dplyr::transmute(
      !!folder_sym,
      !!time_sym := x,
      mean = y
    )
  
  # plot
  p <- ggplot2::ggplot(gg_sum, ggplot2::aes(x = !!time_sym, y = mean, group = 1)) +
    ggplot2::geom_blank(data = dummy_limits, ggplot2::aes(x = !!time_sym, y = mean)) +
    ggplot2::geom_errorbar(
      data = gg_sum |> dplyr::filter(!is.na(ci_low), !is.na(ci_high)),
      ggplot2::aes(ymin = ci_low, ymax = ci_high),
      width = 0.2,
      linewidth = 0.4
    ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::facet_wrap(
      ggplot2::vars(!!folder_sym),
      ncol = 2, nrow = 3, scales = "free",
      labeller = ggplot2::as_labeller(facet_labs)
    ) +
    ggplot2::labs(
      y = ytitle,
      x = x_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  # save
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_path <- file.path(out_dir, png_name)
  
  grDevices::png(filename = out_path, width = width, height = height, res = res)
  print(p)
  grDevices::dev.off()
  
  invisible(list(
    plot = p,
    summary = gg_sum,
    path = out_path
  ))
}

# Plot shows data quality against eye-tracking outcome per group.
plot_rq3 <- function(
    df,
    x_var,
    y_var,
    png_name,
    out_dir = here::here("exp1", "img"),
    width  = 2480 * 1.5,
    height = 3508,
    res    = 300,
    folder_var = "folder",
    group_var  = "group_id",
    lvl = c("4m", "6m", "9m", "18m", "adults", "chimps"),
    facet_labs = c(
      "4m"     = "4-Month-Olds",
      "6m"     = "6-Month-Olds",
      "9m"     = "9-Month-Olds",
      "18m"    = "18-Month-Olds",
      "adults" = "Adults",
      "chimps" = "Chimpanzees"
    ),
    x_lab = NULL,
    y_lab = NULL,
    # "auto" = robustness_prop_2 -> first_non_na, sonst mean
    x_mode = c("auto", "mean", "first_non_na"),
    base_size = 12,
    point_size = 1.4,
    point_alpha = 0.9,
    line_width = 0.7
) {
  stopifnot(is.data.frame(df))
  
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("rlang", quietly = TRUE)) {
    stop("Please install/load: dplyr, ggplot2, rlang")
  }
  
  x_mode <- match.arg(x_mode)
  
  folder_sym <- rlang::sym(folder_var)
  group_sym  <- rlang::sym(group_var)
  x_sym      <- rlang::sym(x_var)
  y_sym      <- rlang::sym(y_var)
  
  needed <- c(folder_var, group_var, x_var, y_var)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in df: ", paste(missing_cols, collapse = ", "))
  }
  
  df_plot <- df |>
    dplyr::mutate(
      !!folder_sym := trimws(as.character(!!folder_sym)),
      !!folder_sym := factor(!!folder_sym, levels = lvl)
    )
  
  # decide x aggregation
  x_mode_eff <- x_mode
  if (identical(x_mode, "auto")) {
    x_mode_eff <- if (identical(x_var, "robustness_prop_2")) "first_non_na" else "mean"
  }
  
  # x per subject (keep chimps; do NOT filter by y here)
  x_subj <- df_plot |>
    dplyr::group_by(!!group_sym, !!folder_sym) |>
    dplyr::summarise(
      !!x_sym := if (identical(x_mode_eff, "first_non_na")) {
        v <- !!x_sym
        v <- v[!is.na(v)]
        if (length(v) == 0) NA_real_ else v[1]
      } else {
        mean(!!x_sym, na.rm = TRUE)
      },
      .groups = "drop"
    )
  
  # y per subject (mean across trials/samples)
  y_subj <- df_plot |>
    dplyr::group_by(!!group_sym, !!folder_sym) |>
    dplyr::summarise(
      !!y_sym := mean(!!y_sym, na.rm = TRUE),
      .groups = "drop"
    )
  
  # combine (only subjects with both x and y)
  df_subj <- dplyr::inner_join(
    x_subj, y_subj,
    by = setNames(c(folder_var, group_var), c(folder_var, group_var))
  ) |>
    dplyr::filter(!is.na(!!x_sym), !is.na(!!y_sym))
  
  if (is.null(x_lab)) x_lab <- x_var
  if (is.null(y_lab)) y_lab <- y_var
  
  p <- ggplot2::ggplot(df_subj, ggplot2::aes(x = !!x_sym, y = !!y_sym)) +
    ggplot2::geom_point(color = "black", size = point_size, alpha = point_alpha) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = line_width) +
    ggplot2::facet_wrap(
      ggplot2::vars(!!folder_sym),
      ncol = 2, nrow = 3, scales = "free",
      labeller = ggplot2::as_labeller(facet_labs)
    ) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_path <- file.path(out_dir, png_name)
  
  grDevices::png(out_path, width = width, height = height, res = res)
  print(p)
  grDevices::dev.off()
  
  invisible(list(plot = p, data = df_subj, path = out_path, x_mode = x_mode_eff))
}

plot_rq3_all <- function(
    df,
    x_var,
    y_var,
    png_name = NULL,                 # if NULL: no file saved, just return plot
    out_dir = here::here("exp1", "img"),
    width  = 2480 * 1.5,
    height = 3508,
    res    = 300,
    folder_var = "folder",
    group_var  = "group_id",
    x_lab = NULL,
    y_lab = NULL,
    x_mode = c("auto", "mean", "first_non_na"),
    base_size = 12,
    point_size = 1.4,
    point_alpha = 0.9,
    line_width = 0.7
) {
  stopifnot(is.data.frame(df))
  
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("rlang", quietly = TRUE)) {
    stop("Please install/load: dplyr, ggplot2, rlang")
  }
  
  x_mode <- match.arg(x_mode)
  
  folder_sym <- rlang::sym(folder_var)
  group_sym  <- rlang::sym(group_var)
  x_sym      <- rlang::sym(x_var)
  y_sym      <- rlang::sym(y_var)
  
  needed <- c(folder_var, group_var, x_var, y_var)
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in df: ", paste(missing_cols, collapse = ", "))
  }
  
  # default behaviour: robustness_prop_2 = first non-NA; everything else = mean
  x_mode_eff <- x_mode
  if (identical(x_mode, "auto")) {
    x_mode_eff <- if (identical(x_var, "robustness_prop_2")) "first_non_na" else "mean"
  }
  
  df_plot <- df |>
    dplyr::mutate(!!folder_sym := trimws(as.character(!!folder_sym)))
  
  # x per subject within folder (don't filter by y here)
  x_subj <- df_plot |>
    dplyr::group_by(!!group_sym, !!folder_sym) |>
    dplyr::summarise(
      !!x_sym := if (identical(x_mode_eff, "first_non_na")) {
        v <- !!x_sym
        v <- v[!is.na(v)]
        if (length(v) == 0) NA_real_ else v[1]
      } else {
        mean(!!x_sym, na.rm = TRUE)
      },
      .groups = "drop"
    )
  
  # y per subject within folder
  y_subj <- df_plot |>
    dplyr::group_by(!!group_sym, !!folder_sym) |>
    dplyr::summarise(
      !!y_sym := mean(!!y_sym, na.rm = TRUE),
      .groups = "drop"
    )
  
  # merge, then pool across folders (one point per subject-folder)
  df_subj <- dplyr::inner_join(
    x_subj, y_subj,
    by = setNames(c(group_var, folder_var), c(group_var, folder_var))
  ) |>
    dplyr::filter(!is.na(!!x_sym), !is.na(!!y_sym))
  
  if (is.null(x_lab)) x_lab <- x_var
  if (is.null(y_lab)) y_lab <- y_var
  
  p <- ggplot2::ggplot(df_subj, ggplot2::aes(x = !!x_sym, y = !!y_sym)) +
    ggplot2::geom_point(color = "black", size = point_size, alpha = point_alpha) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "darkred", linewidth = line_width) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  
  # save optional
  if (!is.null(png_name)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    out_path <- file.path(out_dir, png_name)
    grDevices::png(out_path, width = width, height = height, res = res)
    print(p)
    grDevices::dev.off()
  } else {
    out_path <- NULL
  }
  
  invisible(list(plot = p, data = df_subj, path = out_path, x_mode = x_mode_eff))
}
