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
