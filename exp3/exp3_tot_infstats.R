# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(brms)
library(posterior)
library(marginaleffects)
library(readxl)
library(bayesplot)
library(ggdist)
library(ggforce)
library(bayestestR)

# Load Functions ----------------------------------------------------------
source(here("exp3", "R", "descriptives.R"))
source(here("exp3", "R", "inferentials.R"))
source(here("exp3", "R", "utils.R"))
source(here("exp3", "R", "viz.R"))

# Read Data ---------------------------------------------------------------
folders <- c("4mo", "6to18mo")

dfs <- folders |>
  set_names() |>
  map(read_folder)

df_4m     <- dfs[["4mo"]] |> mutate(age = as.numeric(age), 
                                    no_siblings = as.numeric(no_siblings),
                                    no_household = as.numeric(no_household)) #|> mutate(no_siblings  = as.character(no_siblings), no_household = as.character(no_household))
df_6to18m     <- dfs[["6to18mo"]] #|> mutate(no_siblings  = as.character(no_siblings), no_household = as.character(no_household))

# Prepare Data ------------------------------------------------------------
df_tot <- df_4m |> 
  bind_rows(df_6to18m) |> 
  mutate(
    folder   = factor(folder),
    position = factor(position),
    group_id = factor(group_id)
  )

table(df_tot$acc_visd == 0, useNA = "ifany") # Check for exact zeros - there are none (Gamma can't take zeros, but gamma_hurdle can)
table(df_tot$precrms_visd == 0, useNA = "ifany") # Check for exact zeros - there are none (Gamma can't take zeros, but gamma_hurdle can)
table(df_tot$precsd_visd == 0, useNA = "ifany") # Check for exact zeros - there are none (Gamma can't take zeros, but gamma_hurdle can)

# Order levels of position (in order to make "center" the reference category)
position_levels <- c("center", "topleft", "topright", "botleft", "botright", "top", "bottom")

df_tot <- df_tot |> 
  mutate(position = factor(position, levels = position_levels))

# Scale robustness
df_tot <- df_tot |> 
  mutate(robustness_prop_2 = robustness_ms_2 / 15946)

# Trial Contribution ------------------------------------------------------

## Accuracy ----
df_tot |> 
  filter(trial_included == "yes") |> 
  drop_na(acc_visd) |> 
  group_by(folder, id, condition) |> 
  count() |> 
  group_by(folder, condition) |> 
  summarize(min = min(n, na.rm = T),
            max = max(n, na.rm = T),
            M = mean(n, na.rm = T),
            SD = sd (n, na.rm = T)) |> 
  ungroup() |> 
  slice(3,2,1,6,5,4)

## Precision (RMS) ----
df_tot |> 
  drop_na(precrms_visd) |> 
  group_by(folder, id, condition) |> 
  count() |> 
  group_by(folder, condition) |> 
  summarize(min = min(n, na.rm = T),
            max = max(n, na.rm = T),
            M = mean(n, na.rm = T),
            SD = sd (n, na.rm = T)) |> 
  ungroup() |> 
  slice(3,2,1,6,5,4)

## Precision (SD) ----
df_tot |> 
  drop_na(precsd_visd) |> 
  group_by(folder, id, condition) |> 
  count() |> 
  group_by(folder, condition) |> 
  summarize(min = min(n, na.rm = T),
            max = max(n, na.rm = T),
            M = mean(n, na.rm = T),
            SD = sd (n, na.rm = T)) |> 
  ungroup() |> 
  slice(3,2,1,6,5,4)

# Accuracy ----------------------------------------------------------------

# Models (one per age group)
# Dependent variables: accuracy, precision RMS, precision SD, and robustness (one per model).
# Fixed effect variables of interest: condition (own 5p calibration, peer 9p calibration, adult 9p calibration)
# Fixed control variables: stimulus position on screen
# Random intercept: subject id
# Random slopes: maximal random effect structure
# 
# Full model: data quality ~ condition + position on screen + random effects
# Reduced model: data quality ~ position on screen + random effects

## Priors of Full Model (4M) ----
prior_acc_4m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  # that allows for a broad range of plausible effects while still 
  # providing some regularization to prevent extreme values unless strongly supported by the data.
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Full Model (4M) ----
full_acc_4m <- brm(
  acc_visd ~ 0 + condition + position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "4M"),
  family = Gamma(link="log"),
  prior  = prior_acc_4m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123
)

## Priors of Full Model (6-18M) ----
prior_acc_6to18m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  # that allows for a broad range of plausible effects while still 
  # providing some regularization to prevent extreme values unless strongly supported by the data.
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Full Model (6-18M) ----
full_acc_6to18m <- brm(
  acc_visd ~ 0 + condition + position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "6to18M"),
  family = Gamma(link="log"),
  prior  = prior_acc_6to18m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model (4M) ----
prior_acc_red_4m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  # that allows for a broad range of plausible effects while still 
  # providing some regularization to prevent extreme values unless strongly supported by the data.
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Reduced Model (4M) ----
red_acc_4m <- brm(
  acc_visd ~ position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "4M"),
  family = Gamma(link="log"),
  prior  = prior_acc_red_4m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123,
)

## Define Priors of Reduced Model (6-18M) ----
prior_acc_red_6to18m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  # that allows for a broad range of plausible effects while still 
  # providing some regularization to prevent extreme values unless strongly supported by the data.
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Reduced Model (6-18M) ----
red_acc_6to18m <- brm(
  acc_visd ~ position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "6to18M"),
  family = Gamma(link="log"),
  prior  = prior_acc_red_6to18m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123,
)

## Model Comparison (4M) ----
loo_full_acc_4m <- loo(full_acc_4m)
loo_red_acc_4m <- loo(red_acc_4m)
loo_compare(loo_full_acc_4m, loo_red_acc_4m)

## Model Comparison (6-18M) ----
loo_full_acc_6to18m <- loo(full_acc_6to18m)
loo_red_acc_6to18m <- loo(red_acc_6to18m)
loo_compare(loo_full_acc_6to18m, loo_red_acc_6to18m)

## Contrasts (4M) ----
## Group
groups <- unique(df_tot$condition)
acc_contr_all <- brms_group_effects_response(
  fit   = full_acc_4m,
  groups = groups,
  group_prefix = "condition",
  type  = "contrasts",
  ref   = NULL,          # = all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

acc_contr_all |> 
  arrange(desc(ratio_median))

## Position (4M)
positions <- levels(df_tot$position)[2:7]
acc_contr_all_pos <- brms_group_effects_response(
  fit   = full_acc_4m,
  groups = positions,
  group_prefix = "position",
  type  = "contrasts",
  ref   = NULL,
  link  = "log",
  contrast_scale = "ratio"
)

acc_contr_all_pos |> 
  arrange(desc(ratio_median))

## Contrasts (6-18M) ----
## Group
groups <- unique(df_tot$condition)
acc_contr_all <- brms_group_effects_response(
  fit   = full_acc_6to18m,
  groups = groups,
  group_prefix = "condition",
  type  = "contrasts",
  ref   = NULL,          # = all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

acc_contr_all |> 
  arrange(desc(ratio_median))

## Position (6-18M)
positions <- levels(df_tot$position)[2:7]
acc_contr_all_pos <- brms_group_effects_response(
  fit   = full_acc_6to18m,
  groups = positions,
  group_prefix = "position",
  type  = "contrasts",
  ref   = NULL,
  link  = "log",
  contrast_scale = "ratio"
)

acc_contr_all_pos |> 
  arrange(desc(ratio_median))

## Posterior Probability Comparisons (4M) ----
draws <- as_draws_df(full_acc_4m)
groups <- c("conditionown", "conditionadult", "conditioninfant")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_acc_4m <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_acc_4m |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Posterior Probability Comparisons (6-18M) ----
draws <- as_draws_df(full_acc_6to18m)
groups <- c("conditionown", "conditionadult", "conditioninfant")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_acc_6to18m <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_acc_6to18m |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Model Fit: Posterior Predictive Check (4M) ----
# Check whether model is "match to the data"

png(here("exp3", "img", "acc_4m_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_acc_4m, ndraws = 100)
#pp_check(full_rq1_acc, type = "hist") 
dev.off()

png(here("exp3", "img", "acc_4m_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_acc_4m, type = "intervals_grouped", group = "condition")
dev.off()

## Model Fit: Posterior Predictive Check (6-18M) ----
# Check whether model is "match to the data"

png(here("exp3", "img", "acc_6to18m_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_acc_6to18m, ndraws = 100)
#pp_check(full_rq1_acc, type = "hist")
dev.off()

png(here("exp3", "img", "acc_6to18m_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_acc_6to18m, type = "intervals_grouped", group = "condition")
dev.off()

## Posterior Distribution (4M) ----
# Preparation
condition_order  <- c("own", "infant", "adult")
condition_labels <- c("own"= "Own 5-Point Calibration", "infant" = "Infant 9-Point Calibration", "adult" = "Adult 9-Point Calibration")

pos_order <- c("center","topright","botright","bottom","topleft","botleft","top")
pos_labels <- c(
  "center"="Center",
  "topright"="Top Right",
  "botright"="Bottom Right",
  "bottom"="Bottom",
  "topleft"="Top Left",
  "botleft"="Bottom Left",
  "top"="Top"
)

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  condition   = factor(condition_order, levels = condition_order),
  position = factor(pos_order, levels = pos_order)
) |>
  mutate(position = factor(position, levels = levels(full_acc_4m$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_acc   <- posterior_epred(full_acc_4m, newdata = nd_pos, re_formula = NA)
acc_long <- epred_to_long(ep_acc, nd_pos)

# Create Plot
posterior_plot_acc_4m <- ggplot(
  acc_long,
  aes(x = .epred,
      y = factor(condition, levels = rev(condition_order)),
      # fill = position,
      # colour = position
      )
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05, 
    adjust = 1.0
  ) +
  scale_y_discrete(labels = condition_labels) +
  # scale_fill_discrete(name = "Position", labels = pos_labels) +
  # scale_colour_discrete(name = "Position", labels = pos_labels) +
  labs(x = "Predicted Accuracy", y = NULL) +
  theme_bw(base_size = 14) +
  # theme(legend.position = "bottom",
  #       legend.box = "horizontal",
  #       legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))


png(here("exp3", "img", "acc_4m_posterior_withoutposition.png"), width = 2480/2, height = 3508/2.5, res = 250)
posterior_plot_acc_4m
dev.off()

## Posterior Distribution (6-18M) ----
# Preparation
condition_order  <- c("own", "infant", "adult")
condition_labels <- c("own"= "Own 5-Point Calibration", "infant" = "Infant 9-Point Calibration", "adult" = "Adult 9-Point Calibration")

pos_order <- c("center","topright","botright","bottom","topleft","botleft","top")
pos_labels <- c(
  "center"="Center",
  "topright"="Top Right",
  "botright"="Bottom Right",
  "bottom"="Bottom",
  "topleft"="Top Left",
  "botleft"="Bottom Left",
  "top"="Top"
)

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  condition   = factor(condition_order, levels = condition_order),
  position = factor(pos_order, levels = pos_order)
) |>
  mutate(position = factor(position, levels = levels(full_acc_6to18m$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_acc   <- posterior_epred(full_acc_6to18m, newdata = nd_pos, re_formula = NA)
acc_long <- epred_to_long(ep_acc, nd_pos)

# Create Plot
posterior_plot_acc_6to18m <- ggplot(
  acc_long,
  aes(x = .epred,
      y = factor(condition, levels = rev(condition_order)),
      # fill = position,
      # colour = position
  )
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05, 
    adjust = 1.0
  ) +
  scale_y_discrete(labels = condition_labels) +
  # scale_fill_discrete(name = "Position", labels = pos_labels) +
  # scale_colour_discrete(name = "Position", labels = pos_labels) +
  labs(x = "Predicted Accuracy", y = NULL) +
  theme_bw(base_size = 14) +
  # theme(legend.position = "bottom",
  #       legend.box = "horizontal",
  #       legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))


png(here("exp3", "img", "acc_6to18m_posterior_withoutposition.png"), width = 2480/2, height = 3508/2.5, res = 250)
posterior_plot_acc_6to18m
dev.off()

## Posterior Versus Prior Plots (4M) ----
png(here("exp3", "img", "acc_4m_posteriorprior_own.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_acc_4m, pars = c("b_conditionown", "prior_b"), facet_label = "Own 5-Point Calibration")
dev.off()

png(here("exp3", "img", "acc_4m_posteriorprior_adult.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_acc_4m, pars = c("b_conditionadult", "prior_b"), facet_label = "Adult 9-Point Calibration")
dev.off()

png(here("exp3", "img", "acc_4m_posteriorprior_peer.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_acc_4m, pars = c("b_conditioninfant", "prior_b"), facet_label = "Peer 9-Point Calibration")
dev.off()

## Posterior Versus Prior Plots (6-18M) ----
png(here("exp3", "img", "acc_6to18m_posteriorprior_own.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_acc_6to18m, pars = c("b_conditionown", "prior_b"), facet_label = "Own 5-Point Calibration")
dev.off()

png(here("exp3", "img", "acc_6to18m_posteriorprior_adult.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_acc_6to18m, pars = c("b_conditionadult", "prior_b"), facet_label = "Adult 9-Point Calibration")
dev.off()

png(here("exp3", "img", "acc_6to18m_posteriorprior_peer.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_acc_6to18m, pars = c("b_conditioninfant", "prior_b"), facet_label = "Peer 9-Point Calibration")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(age_group, condition, id) |> 
  summarize(acc_visd = mean(acc_visd, na.rm = T)) |> 
  group_by(age_group, condition) |> 
  summarize(mean_acc_visd = mean(acc_visd, na.rm = T),
            sd_acc_visd = sd(acc_visd, na.rm = T)) |> 
  ungroup() |> 
  slice(3,1,2,6,4,5)

## Paper Plot ----
# Aggregate to subject level (mean over trials)
df_subj <- df_tot |>
  filter(!is.na(acc_visd)) |>
  group_by(condition, age_group, id) |>
  summarise(
    acc_visd = mean(acc_visd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      age_group,
      levels = c("4M", "6to18M"),
      labels = c("4 Months", "6 to 18 Months")
    ),
    Condition = factor(
      condition,
      levels = c("own", "adult", "infant"),
      labels = c("Own 5-Point Calibration", 
                 "Adult 9-Point Calibration", 
                 "Infant 9-Point Calibration")
    )
  )

# Mean + 95% CI across subjects (per group & condition)
sum_df <- df_subj |>
  group_by(Group, Condition) |>
  summarise(
    n = n(),
    mean = mean(acc_visd),
    sd = sd(acc_visd),
    se = sd / sqrt(n),
    tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    ci_low = mean - tcrit * se,
    ci_high = mean + tcrit * se,
    .groups = "drop"
  )

pd <- position_dodge(width = 0.8)
pd_jitter <- position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)

p_acc <- ggplot(df_subj, aes(x = Group, y = acc_visd, group = interaction(Group, Condition))) +
  geom_violin(trim = FALSE, color = "black", fill = NA, position = pd) +
  
  geom_point(
    aes(color = Condition),
    position = pd_jitter,
    size = 0.5,
    alpha = 0.7
  ) +
  
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high, group = Condition),
    width = 0.15,
    linewidth = 0.5,
    color = "black",
    position = pd,
    inherit.aes = FALSE
  ) +
  
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean, group = Condition),
    size = 1.5,
    color = "black",
    position = pd,
    inherit.aes = FALSE
  ) +
  
  labs(
    x = "Group",
    y = "Accuracy\nin visual degrees",
    color = "Condition"
  ) +
  
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp3", "img", "acc_paperplot.png"), width = 2480, height = 3508/4, res = 200)
p_acc
dev.off()

# Precision (RMS) ---------------------------------------------------------

## Priors of Full Model (4M) ----
prior_precrms_4m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Full Model (4M) ----
full_precrms_4m <- brm(
  precrms_visd ~ 0 + condition + position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "4M"),
  family = Gamma(link="log"),
  prior  = prior_precrms_4m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123,
)

## Priors of Full Model (6-18M) ----
prior_precrms_6to18m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Full Model (6-18M) ----
full_precrms_6to18m <- brm(
  precrms_visd ~ 0 + condition + position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "6to18M"),
  family = Gamma(link="log"),
  prior  = prior_precrms_6to18m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model (4M) ----
prior_precrms_red_4m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Reduced Model (4M) ----
red_precrms_4m <- brm(
  precrms_visd ~ position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "4M"),
  family = Gamma(link="log"),
  prior  = prior_precrms_red_4m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model (6-18M) ----
prior_precrms_red_6to18m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Reduced Model (6-18M) ----
red_precrms_6to18m <- brm(
  precrms_visd ~ position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "6to18M"),
  family = Gamma(link="log"),
  prior  = prior_precrms_red_6to18m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123
)

## Model Comparison (4M) ----
loo_full_precrms_4m <- loo(full_precrms_4m)
loo_red_precrms_4m <- loo(red_precrms_4m)
loo_compare(loo_full_precrms_4m, loo_red_precrms_4m)

## Model Comparison (6-18M) ----
loo_full_precrms_6to18m <- loo(full_precrms_6to18m)
loo_red_precrms_6to18m <- loo(red_precrms_6to18m)
loo_compare(loo_full_precrms_6to18m, loo_red_precrms_6to18m)

## Contrasts (4M) ----
groups <- unique(df_tot$condition)
precrms_contr_all <- brms_group_effects_response(
  fit   = full_precrms_4m,
  groups = groups,
  group_prefix = "condition",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

precrms_contr_all |> 
  arrange(desc(ratio_median)) |> 
  print(n = 30)

## Contrasts (6-18M) ----
groups <- unique(df_tot$condition)
precrms_contr_all <- brms_group_effects_response(
  fit   = full_precrms_6to18m,
  groups = groups,
  group_prefix = "condition",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

precrms_contr_all |> 
  arrange(desc(ratio_median)) |> 
  print(n = 30)

## Posterior Probability Comparisons (4M) ----
draws <- as_draws_df(full_precrms_4m)
groups <- c("conditionown", "conditionadult", "conditioninfant")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_precrms_4m <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_precrms_4m |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Posterior Probability Comparisons (6-18M) ----
draws <- as_draws_df(full_precrms_6to18m)
groups <- c("conditionown", "conditionadult", "conditioninfant")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_precrms_6to18m <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_precrms_6to18m |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Model Fit: Posterior Predictive Check (4M) ----
# Check whether model is "match to the data"

png(here("exp3", "img", "precrms_4m_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precrms_4m, ndraws = 100)
#pp_check(full_rq1_precrms, type = "hist")
dev.off()

png(here("exp3", "img", "precrms_4m_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precrms_4m, type = "intervals_grouped", group = "condition")
dev.off()

## Model Fit: Posterior Predictive Check (6-18M) ----
# Check whether model is "match to the data"

png(here("exp3", "img", "precrms_6to18m_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precrms_6to18m, ndraws = 100)
#pp_check(full_rq1_precrms, type = "hist")
dev.off()

png(here("exp3", "img", "precrms_6to18m_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precrms_6to18m, type = "intervals_grouped", group = "condition")
dev.off()

## Posterior Distribution (4M) ----
# Preparation
condition_order  <- c("own", "infant", "adult")
condition_labels <- c("own"= "Own 5-Point Calibration", "infant" = "Infant 9-Point Calibration", "adult" = "Adult 9-Point Calibration")

pos_order <- c("center","topright","botright","bottom","topleft","botleft","top")
pos_labels <- c(
  "center"="Center",
  "topright"="Top Right",
  "botright"="Bottom Right",
  "bottom"="Bottom",
  "topleft"="Top Left",
  "botleft"="Bottom Left",
  "top"="Top"
)

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  condition   = factor(condition_order, levels = condition_order),
  position = factor(pos_order, levels = pos_order)
) |>
  mutate(position = factor(position, levels = levels(full_precrms_4m$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_precrms   <- posterior_epred(full_precrms_4m, newdata = nd_pos, re_formula = NA)
precrms_long <- epred_to_long(ep_precrms, nd_pos)

# Create Plot
posterior_plot_precrms_4m <- ggplot(
  precrms_long,
  aes(x = .epred,
      y = factor(condition, levels = rev(condition_order)),
      # fill = position,
      # colour = position
  )
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05, 
    adjust = 1.0
  ) +
  scale_y_discrete(labels = condition_labels) +
  # scale_fill_discrete(name = "Position", labels = pos_labels) +
  # scale_colour_discrete(name = "Position", labels = pos_labels) +
  labs(x = "Predicted Precision (RMS)", y = NULL) +
  theme_bw(base_size = 14) +
  # theme(legend.position = "bottom",
  #       legend.box = "horizontal",
  #       legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))


png(here("exp3", "img", "precrms_4m_posterior_withoutposition.png"), width = 2480/2, height = 3508/2.5, res = 250)
posterior_plot_precrms_4m
dev.off()

## Posterior Distribution (6-18M) ----
# Preparation
condition_order  <- c("own", "infant", "adult")
condition_labels <- c("own"= "Own 5-Point Calibration", "infant" = "Infant 9-Point Calibration", "adult" = "Adult 9-Point Calibration")

pos_order <- c("center","topright","botright","bottom","topleft","botleft","top")
pos_labels <- c(
  "center"="Center",
  "topright"="Top Right",
  "botright"="Bottom Right",
  "bottom"="Bottom",
  "topleft"="Top Left",
  "botleft"="Bottom Left",
  "top"="Top"
)

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  condition   = factor(condition_order, levels = condition_order),
  position = factor(pos_order, levels = pos_order)
) |>
  mutate(position = factor(position, levels = levels(full_precrms_6to18m$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_precrms   <- posterior_epred(full_precrms_6to18m, newdata = nd_pos, re_formula = NA)
precrms_long <- epred_to_long(ep_precrms, nd_pos)

# Create Plot
posterior_plot_precrms_6to18m <- ggplot(
  precrms_long,
  aes(x = .epred,
      y = factor(condition, levels = rev(condition_order)),
      # fill = position,
      # colour = position
  )
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05, 
    adjust = 1.0
  ) +
  scale_y_discrete(labels = condition_labels) +
  # scale_fill_discrete(name = "Position", labels = pos_labels) +
  # scale_colour_discrete(name = "Position", labels = pos_labels) +
  labs(x = "Predicted Precision (RMS)", y = NULL) +
  theme_bw(base_size = 14) +
  # theme(legend.position = "bottom",
  #       legend.box = "horizontal",
  #       legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))


png(here("exp3", "img", "precrms_6to18m_posterior_withoutposition.png"), width = 2480/2, height = 3508/2.5, res = 250)
posterior_plot_precrms_6to18m
dev.off()

## Posterior Versus Prior Plots (4M) ----
png(here("exp3", "img", "precrms_4m_posteriorprior_own.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precrms_4m, pars = c("b_conditionown", "prior_b"), facet_label = "Own 5-Point Calibration")
dev.off()

png(here("exp3", "img", "precrms_4m_posteriorprior_adult.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precrms_4m, pars = c("b_conditionadult", "prior_b"), facet_label = "Adult 9-Point Calibration")
dev.off()

png(here("exp3", "img", "precrms_4m_posteriorprior_peer.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precrms_4m, pars = c("b_conditioninfant", "prior_b"), facet_label = "Peer 9-Point Calibration")
dev.off()

## Posterior Versus Prior Plots (6-18M) ----
png(here("exp3", "img", "precrms_6to18m_posteriorprior_own.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precrms_6to18m, pars = c("b_conditionown", "prior_b"), facet_label = "Own 5-Point Calibration")
dev.off()

png(here("exp3", "img", "precrms_6to18m_posteriorprior_adult.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precrms_6to18m, pars = c("b_conditionadult", "prior_b"), facet_label = "Adult 9-Point Calibration")
dev.off()

png(here("exp3", "img", "precrms_6to18m_posteriorprior_peer.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precrms_6to18m, pars = c("b_conditioninfant", "prior_b"), facet_label = "Peer 9-Point Calibration")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(age_group, condition, id) |> 
  summarize(precrms_visd = mean(precrms_visd, na.rm = T)) |> 
  group_by(age_group, condition) |> 
  summarize(mean_precrms_visd = mean(precrms_visd, na.rm = T),
            sd_precrms_visd = sd(precrms_visd, na.rm = T)) |> 
  ungroup() |> 
  slice(3,1,2,6,4,5)

## Paper Plot ----
# Aggregate to subject level (mean over trials)
df_subj <- df_tot |>
  filter(!is.na(precrms_visd)) |>
  group_by(condition, age_group, id) |>
  summarise(
    precrms_visd = mean(precrms_visd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      age_group,
      levels = c("4M", "6to18M"),
      labels = c("4 Months", "6 to 18 Months")
    ),
    Condition = factor(
      condition,
      levels = c("own", "adult", "infant"),
      labels = c("Own 5-Point Calibration", 
                 "Adult 9-Point Calibration", 
                 "Infant 9-Point Calibration")
    )
  )

# Mean + 95% CI across subjects (per group & condition)
sum_df <- df_subj |>
  group_by(Group, Condition) |>
  summarise(
    n = n(),
    mean = mean(precrms_visd),
    sd = sd(precrms_visd),
    se = sd / sqrt(n),
    tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    ci_low = mean - tcrit * se,
    ci_high = mean + tcrit * se,
    .groups = "drop"
  )

pd <- position_dodge(width = 0.8)
pd_jitter <- position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)

p_acc <- ggplot(df_subj, aes(x = Group, y = precrms_visd, group = interaction(Group, Condition))) +
  geom_violin(trim = FALSE, color = "black", fill = NA, position = pd) +
  
  geom_point(
    aes(color = Condition),
    position = pd_jitter,
    size = 0.5,
    alpha = 0.7
  ) +
  
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high, group = Condition),
    width = 0.15,
    linewidth = 0.5,
    color = "black",
    position = pd,
    inherit.aes = FALSE
  ) +
  
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean, group = Condition),
    size = 1.5,
    color = "black",
    position = pd,
    inherit.aes = FALSE
  ) +
  
  labs(
    x = "Group",
    y = "Precision (RMS)\nin visual degrees",
    color = "Condition"
  ) +
  
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp3", "img", "precrmspaperplot.png"), width = 2480, height = 3508/4, res = 200)
p_acc
dev.off()

# Precision (SD) ----------------------------------------------------------

## Priors of Full Model (4M) ----
prior_precsd_4m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Full Model (4M) ----
full_precsd_4m <- brm(
  precsd_visd ~ 0 + condition + position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "4M"),
  family = Gamma(link="log"),
  prior  = prior_precsd_4m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123,
)

## Priors of Full Model (6-18M) ----
prior_precsd_6to18m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Full Model (6-18M) ----
full_precsd_6to18m <- brm(
  precsd_visd ~ 0 + condition + position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "6to18M"),
  family = Gamma(link="log"),
  prior  = prior_precsd_6to18m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model (4M) ----
prior_precsd_red_4m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Reduced Model (4M) ----
red_precsd_4m <- brm(
  precsd_visd ~ position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "4M"),
  family = Gamma(link="log"),
  prior  = prior_precsd_red_4m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model (6-18M) ----
prior_precsd_red_6to18m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Reduced Model (6-18M) ----
red_precsd_6to18m <- brm(
  precsd_visd ~ position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)) |> filter(age_group == "6to18M"),
  family = Gamma(link="log"),
  prior  = prior_precsd_red_6to18m,
  chains = 4, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123
)

## Model Comparison (4M) ----
loo_full_precsd_4m <- loo(full_precsd_4m)
loo_red_precsd_4m <- loo(red_precsd_4m)
loo_compare(loo_full_precsd_4m, loo_red_precsd_4m)

## Model Comparison (6-18M) ----
loo_full_precsd_6to18m <- loo(full_precsd_6to18m)
loo_red_precsd_6to18m <- loo(red_precsd_6to18m)
loo_compare(loo_full_precsd_6to18m, loo_red_precsd_6to18m)

## Contrasts (4M) ----
groups <- unique(df_tot$condition)
precsd_contr_all <- brms_group_effects_response(
  fit   = full_precsd_4m,
  groups = groups,
  group_prefix = "condition",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

precsd_contr_all |> 
  arrange(desc(ratio_median)) |> 
  print(n = 30)

## Contrasts (6-18M) ----
groups <- unique(df_tot$condition)
precsd_contr_all <- brms_group_effects_response(
  fit   = full_precsd_6to18m,
  groups = groups,
  group_prefix = "condition",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

precsd_contr_all |> 
  arrange(desc(ratio_median)) |> 
  print(n = 30)

## Posterior Probability Comparisons (4M) ----
draws <- as_draws_df(full_precsd_4m)
groups <- c("conditionown", "conditionadult", "conditioninfant")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_precsd_4m <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_precsd_4m |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Posterior Probability Comparisons (6-18M) ----
draws <- as_draws_df(full_precsd_6to18m)
groups <- c("conditionown", "conditionadult", "conditioninfant")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_precsd_6to18m <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_precsd_6to18m |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Model Fit: Posterior Predictive Check (4M) ----
# Check whether model is "match to the data"

png(here("exp3", "img", "precsd_4m_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precsd_4m, ndraws = 100)
#pp_check(full_rq1_precsd, type = "hist")
dev.off()

png(here("exp3", "img", "precsd_4m_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precsd_4m, type = "intervals_grouped", group = "condition")
dev.off()

## Model Fit: Posterior Predictive Check (6-18M) ----
# Check whether model is "match to the data"

png(here("exp3", "img", "precsd_6to18m_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precsd_6to18m, ndraws = 100)
#pp_check(full_rq1_precsd, type = "hist")
dev.off()

png(here("exp3", "img", "precsd_6to18m_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precsd_6to18m, type = "intervals_grouped", group = "condition")
dev.off()

## Posterior Distribution (4M) ----
# Preparation
condition_order  <- c("own", "infant", "adult")
condition_labels <- c("own"= "Own 5-Point Calibration", "infant" = "Infant 9-Point Calibration", "adult" = "Adult 9-Point Calibration")

pos_order <- c("center","topright","botright","bottom","topleft","botleft","top")
pos_labels <- c(
  "center"="Center",
  "topright"="Top Right",
  "botright"="Bottom Right",
  "bottom"="Bottom",
  "topleft"="Top Left",
  "botleft"="Bottom Left",
  "top"="Top"
)

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  condition   = factor(condition_order, levels = condition_order),
  position = factor(pos_order, levels = pos_order)
) |>
  mutate(position = factor(position, levels = levels(full_precsd_4m$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_precsd   <- posterior_epred(full_precsd_4m, newdata = nd_pos, re_formula = NA)
precsd_long <- epred_to_long(ep_precsd, nd_pos)

# Create Plot
posterior_plot_precsd_4m <- ggplot(
  precsd_long,
  aes(x = .epred,
      y = factor(condition, levels = rev(condition_order)),
      # fill = position,
      # colour = position
  )
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05, 
    adjust = 1.0
  ) +
  scale_y_discrete(labels = condition_labels) +
  # scale_fill_discrete(name = "Position", labels = pos_labels) +
  # scale_colour_discrete(name = "Position", labels = pos_labels) +
  labs(x = "Predicted Precision (SD)", y = NULL) +
  theme_bw(base_size = 14) +
  # theme(legend.position = "bottom",
  #       legend.box = "horizontal",
  #       legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))


png(here("exp3", "img", "precsd_4m_posterior_withoutposition.png"), width = 2480/2, height = 3508/2.5, res = 250)
posterior_plot_precsd_4m
dev.off()

## Posterior Distribution (6-18M) ----
# Preparation
condition_order  <- c("own", "infant", "adult")
condition_labels <- c("own"= "Own 5-Point Calibration", "infant" = "Infant 9-Point Calibration", "adult" = "Adult 9-Point Calibration")

pos_order <- c("center","topright","botright","bottom","topleft","botleft","top")
pos_labels <- c(
  "center"="Center",
  "topright"="Top Right",
  "botright"="Bottom Right",
  "bottom"="Bottom",
  "topleft"="Top Left",
  "botleft"="Bottom Left",
  "top"="Top"
)

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  condition   = factor(condition_order, levels = condition_order),
  position = factor(pos_order, levels = pos_order)
) |>
  mutate(position = factor(position, levels = levels(full_precsd_6to18m$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_precsd   <- posterior_epred(full_precsd_6to18m, newdata = nd_pos, re_formula = NA)
precsd_long <- epred_to_long(ep_precsd, nd_pos)

# Create Plot
posterior_plot_precsd_6to18m <- ggplot(
  precsd_long,
  aes(x = .epred,
      y = factor(condition, levels = rev(condition_order)),
      # fill = position,
      # colour = position
  )
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05, 
    adjust = 1.0
  ) +
  scale_y_discrete(labels = condition_labels) +
  # scale_fill_discrete(name = "Position", labels = pos_labels) +
  # scale_colour_discrete(name = "Position", labels = pos_labels) +
  labs(x = "Predicted Precision (SD)", y = NULL) +
  theme_bw(base_size = 14) +
  # theme(legend.position = "bottom",
  #       legend.box = "horizontal",
  #       legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))


png(here("exp3", "img", "precsd_6to18m_posterior_withoutposition.png"), width = 2480/2, height = 3508/2.5, res = 250)
posterior_plot_precsd_6to18m
dev.off()

## Posterior Versus Prior Plots (4M) ----
png(here("exp3", "img", "precsd_4m_posteriorprior_own.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precsd_4m, pars = c("b_conditionown", "prior_b"), facet_label = "Own 5-Point Calibration")
dev.off()

png(here("exp3", "img", "precsd_4m_posteriorprior_adult.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precsd_4m, pars = c("b_conditionadult", "prior_b"), facet_label = "Adult 9-Point Calibration")
dev.off()

png(here("exp3", "img", "precsd_4m_posteriorprior_peer.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precsd_4m, pars = c("b_conditioninfant", "prior_b"), facet_label = "Peer 9-Point Calibration")
dev.off()

## Posterior Versus Prior Plots (6-18M) ----
png(here("exp3", "img", "precsd_6to18m_posteriorprior_own.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precsd_6to18m, pars = c("b_conditionown", "prior_b"), facet_label = "Own 5-Point Calibration")
dev.off()

png(here("exp3", "img", "precsd_6to18m_posteriorprior_adult.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precsd_6to18m, pars = c("b_conditionadult", "prior_b"), facet_label = "Adult 9-Point Calibration")
dev.off()

png(here("exp3", "img", "precsd_6to18m_posteriorprior_peer.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precsd_6to18m, pars = c("b_conditioninfant", "prior_b"), facet_label = "Peer 9-Point Calibration")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(age_group, condition, id) |> 
  summarize(precsd_visd = mean(precsd_visd, na.rm = T)) |> 
  group_by(age_group, condition) |> 
  summarize(mean_precsd_visd = mean(precsd_visd, na.rm = T),
            sd_precsd_visd = sd(precsd_visd, na.rm = T)) |> 
  ungroup() |> 
  slice(3,1,2,6,4,5)

## Paper Plot ----
# Aggregate to subject level (mean over trials)
df_subj <- df_tot |>
  filter(!is.na(precsd_visd)) |>
  group_by(condition, age_group, id) |>
  summarise(
    precsd_visd = mean(precsd_visd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      age_group,
      levels = c("4M", "6to18M"),
      labels = c("4 Months", "6 to 18 Months")
    ),
    Condition = factor(
      condition,
      levels = c("own", "adult", "infant"),
      labels = c("Own 5-Point Calibration", 
                 "Adult 9-Point Calibration", 
                 "Infant 9-Point Calibration")
    )
  )

# Mean + 95% CI across subjects (per group & condition)
sum_df <- df_subj |>
  group_by(Group, Condition) |>
  summarise(
    n = n(),
    mean = mean(precsd_visd),
    sd = sd(precsd_visd),
    se = sd / sqrt(n),
    tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    ci_low = mean - tcrit * se,
    ci_high = mean + tcrit * se,
    .groups = "drop"
  )

pd <- position_dodge(width = 0.8)
pd_jitter <- position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)

p_acc <- ggplot(df_subj, aes(x = Group, y = precsd_visd, group = interaction(Group, Condition))) +
  geom_violin(trim = FALSE, color = "black", fill = NA, position = pd) +
  
  geom_point(
    aes(color = Condition),
    position = pd_jitter,
    size = 0.5,
    alpha = 0.7
  ) +
  
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high, group = Condition),
    width = 0.15,
    linewidth = 0.5,
    color = "black",
    position = pd,
    inherit.aes = FALSE
  ) +
  
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean, group = Condition),
    size = 1.5,
    color = "black",
    position = pd,
    inherit.aes = FALSE
  ) +
  
  labs(
    x = "Group",
    y = "Precision (SD)\nin visual degrees",
    color = "Condition"
  ) +
  
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp3", "img", "precsdpaperplot.png"), width = 2480, height = 3508/4, res = 200)
p_acc
dev.off()

# Robustness --------------------------------------------------------------

## Define Priors of Full Model (4M) ----
priors_rob_4m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  prior(exponential(1), class = "phi"), # same as in Exp1+2, for Beta distribution
  prior(exponential(1), class = "sd")   # same as in Exp1+2, for random effects regularization
)

## Full Model (4M) ----
full_rob_4m <- brm(
  robustness_prop_2 ~ 0 + condition + (1 | group_id),
  data   = df_tot |> filter(!is.na(robustness_prop_2)) |> select(age_group, condition, group_id,robustness_prop_2) |> distinct() |> filter(age_group == "4M"),
  family = Beta(link = "logit"),
  prior  = priors_rob_4m,
  chains = 4, iter = 5000, warmup = 4000,
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Full Model (6-18M) ----
priors_rob_6to18m <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  prior(exponential(1), class = "phi"), # same as in Exp1+2, for Beta distribution
  prior(exponential(1), class = "sd")   # same as in Exp1+2, for random effects regularization
)

## Full Model (6-18M) ----
full_rob_6to18m <- brm(
  robustness_prop_2 ~ 0 + condition + (1 | group_id),
  data   = df_tot |> filter(!is.na(robustness_prop_2)) |> select(age_group, condition, group_id,robustness_prop_2) |> distinct() |> filter(age_group == "6to18M"),
  family = Beta(link = "logit"),
  prior  = priors_rob_6to18m,
  chains = 4, iter = 7000, warmup = 6000,
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model (4M) ----
priors_rob_4m_red <- c(
  prior(exponential(1), class = "phi"), # same as in Exp1+2, for Beta distribution
  prior(exponential(1), class = "sd")   # same as in Exp1+2, for random effects regularization
)

## Reduced Model (4M) ----
red_rob_4m <- brm(
  robustness_prop_2 ~ (1 | group_id),
  data   = df_tot |> filter(!is.na(robustness_prop_2)) |> select(age_group, condition, group_id,robustness_prop_2) |> distinct() |> filter(age_group == "4M"),
  family = Beta(link = "logit"),
  prior  = priors_rob_4m_red,
  chains = 4, iter = 5000, warmup = 4000,
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model (6-18M) ----
priors_rob_6to18m_red <- c(
  prior(exponential(1), class = "phi"), # same as in Exp1+2, for Beta distribution
  prior(exponential(1), class = "sd")   # same as in Exp1+2, for random effects regularization
)

## Reduced Model (6-18M) ----
red_rob_6to18m <- brm(
  robustness_prop_2 ~ (1 | group_id),
  data   = df_tot |> filter(!is.na(robustness_prop_2)) |> select(age_group, condition, group_id,robustness_prop_2) |> distinct() |> filter(age_group == "6to18M"),
  family = Beta(link = "logit"),
  prior  = priors_rob_6to18m_red,
  chains = 4, iter = 5000, warmup = 4000,
  sample_prior = "yes",
  seed = 123
)

## Model Comparison (4M) ----
loo_full_rob_4m <- loo(full_rob_4m)
loo_red_rob_4m <- loo(red_rob_4m)
loo_compare(loo_full_rob_4m, loo_red_rob_4m)

## Model Comparison (6-18M) ----
loo_full_rob_6to18m <- loo(full_rob_6to18m)
loo_red_rob_6to18m <- loo(red_rob_6to18m)
loo_compare(loo_full_rob_6to18m, loo_red_rob_6to18m)

## Contrasts (4M) ----
groups <- unique(df_tot$condition)
rob_contr_all <- brms_group_effects_response(
  fit   = full_rob_4m,
  groups = groups,
  group_prefix = "condition",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

rob_contr_all |> 
  arrange(desc(ratio_median)) |> 
  print(n = 30)

## Contrasts (6-18M) ----
groups <- unique(df_tot$condition)
rob_contr_all <- brms_group_effects_response(
  fit   = full_rob_6to18m,
  groups = groups,
  group_prefix = "condition",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

rob_contr_all |> 
  arrange(desc(ratio_median)) |> 
  print(n = 30)

## Posterior Probability Comparisons (4M) ----
draws <- as_draws_df(full_rob_4m)
groups <- c("conditionown", "conditionadult", "conditioninfant")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_rob_4m <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_rob_4m |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Posterior Probability Comparisons (6-18M) ----
draws <- as_draws_df(full_rob_6to18m)
groups <- c("conditionown", "conditionadult", "conditioninfant")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_rob_6to18m <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_rob_6to18m |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Model Fit: Posterior Predictive Check (4M) ----
# Check whether model is "match to the data"

png(here("exp3", "img", "rob_4m_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rob_4m, ndraws = 100)
#pp_check(full_rq1_rob, type = "hist")
dev.off()

png(here("exp3", "img", "rob_4m_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rob_4m, type = "intervals_grouped", group = "condition")
dev.off()

## Model Fit: Posterior Predictive Check (6-18M) ----
# Check whether model is "match to the data"

png(here("exp3", "img", "rob_6to18m_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rob_6to18m, ndraws = 100)
#pp_check(full_rq1_rob, type = "hist")
dev.off()

png(here("exp3", "img", "rob_6to18m_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rob_6to18m, type = "intervals_grouped", group = "condition")
dev.off()

## Posterior Distribution (4M) ----
# Preparation
condition_order  <- c("own", "infant", "adult")
condition_labels <- c("own"= "Own 5-Point Calibration", "infant" = "Infant 9-Point Calibration", "adult" = "Adult 9-Point Calibration")

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  condition   = factor(condition_order, levels = condition_order))

# Create Predictions
ep_rob   <- posterior_epred(full_rob_4m, newdata = nd_pos, re_formula = NA)
rob_long <- epred_to_long(ep_rob, nd_pos)

# Create Plot
posterior_plot_rob_4m <- ggplot(
  rob_long,
  aes(x = .epred,
      y = factor(condition, levels = rev(condition_order))
  )
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05, 
    adjust = 1.0
  ) +
  scale_y_discrete(labels = condition_labels) +
  labs(x = "Predicted Robustness", y = NULL) +
  theme_bw(base_size = 14) +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))


png(here("exp3", "img", "rob_4m_posterior.png"), width = 2480/2, height = 3508/2.5, res = 250)
posterior_plot_rob_4m
dev.off()

## Posterior Distribution (6-18M) ----
# Preparation
condition_order  <- c("own", "infant", "adult")
condition_labels <- c("own"= "Own 5-Point Calibration", "infant" = "Infant 9-Point Calibration", "adult" = "Adult 9-Point Calibration")

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  condition   = factor(condition_order, levels = condition_order))

# Create Predictions
ep_rob   <- posterior_epred(full_rob_6to18m, newdata = nd_pos, re_formula = NA)
rob_long <- epred_to_long(ep_rob, nd_pos)

# Create Plot
posterior_plot_rob_6to18m <- ggplot(
  rob_long,
  aes(x = .epred,
      y = factor(condition, levels = rev(condition_order))
  )
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05, 
    adjust = 1.0
  ) +
  scale_y_discrete(labels = condition_labels) +
  labs(x = "Predicted Robustness", y = NULL) +
  theme_bw(base_size = 14) +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))

png(here("exp3", "img", "rob_6to18m_posterior.png"), width = 2480/2, height = 3508/2.5, res = 250)
posterior_plot_rob_6to18m
dev.off()

## Posterior Versus Prior Plots (4M) ----
png(here("exp3", "img", "rob_4m_posteriorprior_own.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rob_4m, pars = c("b_conditionown", "prior_b"), facet_label = "Own 5-Point Calibration")
dev.off()

png(here("exp3", "img", "rob_4m_posteriorprior_adult.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rob_4m, pars = c("b_conditionadult", "prior_b"), facet_label = "Adult 9-Point Calibration")
dev.off()

png(here("exp3", "img", "rob_4m_posteriorprior_peer.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rob_4m, pars = c("b_conditioninfant", "prior_b"), facet_label = "Peer 9-Point Calibration")
dev.off()

## Posterior Versus Prior Plots (6-18M) ----
png(here("exp3", "img", "rob_6to18m_posteriorprior_own.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rob_6to18m, pars = c("b_conditionown", "prior_b"), facet_label = "Own 5-Point Calibration")
dev.off()

png(here("exp3", "img", "rob_6to18m_posteriorprior_adult.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rob_6to18m, pars = c("b_conditionadult", "prior_b"), facet_label = "Adult 9-Point Calibration")
dev.off()

png(here("exp3", "img", "rob_6to18m_posteriorprior_peer.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rob_6to18m, pars = c("b_conditioninfant", "prior_b"), facet_label = "Peer 9-Point Calibration")
dev.off()

## Descriptives ----
df_tot |> 
  filter(!is.na(robustness_prop_2)) |> 
  select(age_group, condition, group_id, robustness_prop_2) |> 
  distinct() |>
  group_by(age_group, condition) |> 
  summarize(mean_robustness_prop_2 = mean(robustness_prop_2, na.rm = T),
            sd_robustness_prop_2 = sd(robustness_prop_2, na.rm = T)) |> 
  ungroup() |> 
  slice(3,1,2,6,4,5)

## Paper Plot ----
# Aggregate to subject level (mean over trials)
df_subj <- df_tot |>
  filter(!is.na(robustness_prop_2)) |> 
  select(age_group, condition, group_id, id, robustness_prop_2) |> 
  distinct() |>
  group_by(condition, age_group, id) |>
  summarise(
    robustness_prop_2 = mean(robustness_prop_2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      age_group,
      levels = c("4M", "6to18M"),
      labels = c("4 Months", "6 to 18 Months")
    ),
    Condition = factor(
      condition,
      levels = c("own", "adult", "infant"),
      labels = c("Own 5-Point Calibration", 
                 "Adult 9-Point Calibration", 
                 "Infant 9-Point Calibration")
    )
  )

# Mean + 95% CI across subjects (per group & condition)
sum_df <- df_subj |>
  group_by(Group, Condition) |>
  summarise(
    n = n(),
    mean = mean(robustness_prop_2),
    sd = sd(robustness_prop_2),
    se = sd / sqrt(n),
    tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    ci_low = mean - tcrit * se,
    ci_high = mean + tcrit * se,
    .groups = "drop"
  )

pd <- position_dodge(width = 0.8)
pd_jitter <- position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8)

p_rob <- ggplot(df_subj, aes(x = Group, y = robustness_prop_2, group = interaction(Group, Condition))) +
  geom_violin(trim = FALSE, color = "black", fill = NA, position = pd) +
  
  geom_point(
    aes(color = Condition),
    position = pd_jitter,
    size = 0.5,
    alpha = 0.7
  ) +
  
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high, group = Condition),
    width = 0.15,
    linewidth = 0.5,
    color = "black",
    position = pd,
    inherit.aes = FALSE
  ) +
  
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean, group = Condition),
    size = 1.5,
    color = "black",
    position = pd,
    inherit.aes = FALSE
  ) +
  
  labs(
    x = "Group",
    y = "Robustness\nin %",
    color = "Condition"
  ) +
  
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp3", "img", "robpaperplot.png"), width = 2480, height = 3508/4, res = 200)
p_rob
dev.off()

