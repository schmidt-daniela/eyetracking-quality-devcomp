mean_sd <- function(data, group_by_columns, col_name) {
  data |>
    group_by(across(all_of(group_by_columns))) |>
    dplyr::summarize(
      !!paste0("sd_", col_name) := sd(.data[[col_name]], na.rm = TRUE),
      !!paste0("mean_", col_name) := mean(.data[[col_name]], na.rm = TRUE)
    ) |>
    ungroup()
}

plot_distribution_rq1 <- function(data, mean_col, sd_col, plot_title, x_label, x_lim) {
  ggplot() +
    stat_function(fun = dnorm, aes(color = as.factor(data$age_group[1])), 
                  args = list(mean = data[[mean_col]][1], sd = data[[sd_col]][1]), size = 1) +
    stat_function(fun = dnorm, aes(color = as.factor(data$age_group[2])), 
                  args = list(mean = data[[mean_col]][2], sd = data[[sd_col]][2]), size = 1) +
    stat_function(fun = dnorm, aes(color = as.factor(data$age_group[3])), 
                  args = list(mean = data[[mean_col]][3], sd = data[[sd_col]][3]), size = 1) +
    stat_function(fun = dnorm, aes(color = as.factor(data$age_group[4])), 
                  args = list(mean = data[[mean_col]][4], sd = data[[sd_col]][4]), size = 1) +
    stat_function(fun = dnorm, aes(color = as.factor(data$age_group[5])), 
                  args = list(mean = data[[mean_col]][5], sd = data[[sd_col]][5]), size = 1) +
    stat_function(fun = dnorm, aes(color = as.factor(data$age_group[6])), 
                  args = list(mean = data[[mean_col]][6], sd = data[[sd_col]][6]), size = 1) +
    stat_function(fun = dnorm, aes(color = as.factor(data$age_group[7])), 
                  args = list(mean = data[[mean_col]][7], sd = data[[sd_col]][7]), size = 1) +
    scale_color_manual(values = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2","#000000","#891101" )) +
    labs(title = plot_title, x = x_label, y = "Density", color = "Group") +
    xlim(x_lim[1], x_lim[2]) +
    theme_minimal()
}

plot_regression_rq2 <- function(data, x_var, y_var, x_label, ylabel, plot_title, age_group) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    facet_wrap(as.formula(paste("~", age_group)), scales = "free") +
    theme_minimal() +
    labs(title = plot_title, x = x_label, y = ylabel)
}