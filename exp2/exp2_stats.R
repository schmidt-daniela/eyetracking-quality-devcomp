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
source(here("exp2", "R", "descriptives.R"))
source(here("exp2", "R", "inferentials.R"))
source(here("exp2", "R", "utils.R"))
source(here("exp2", "R", "viz.R"))

# Set Parameters ----------------------------------------------------------
n_cores <- parallel::detectCores()

# Read Data ---------------------------------------------------------------
folders <- c("ape_calibration_2p", "human_calibration_9p", "alex_calibration_5p")

dfs <- folders |>
  set_names() |>
  map(read_folder)

df_ape_calibration_2p <- dfs[["ape_calibration_2p"]]
df_human_calibration_9p <- dfs[["human_calibration_9p"]]
df_alex_calibration_5p <- dfs[["alex_calibration_5p"]]

# Prepare Data ------------------------------------------------------------
df_tot <- df_ape_calibration_2p |> 
  bind_rows(df_human_calibration_9p, df_alex_calibration_5p) |> 
  mutate(
    folder   = factor(folder),
    position = factor(position),
    group_id = factor(group_id)
  ) |> 
  mutate(folder = fct_relevel(folder, "ape_calibration_2p", "human_calibration_9p"))

table(df_tot$acc_visd == 0, useNA = "ifany") # Check for exact zeros (Gamma can't take zeros, but gamma_hurdle can)

# Order levels of position (in order to make "center" the reference category)
position_levels <- c("center", "top_left", "top_right", "bot_left", "bot_right", "top", "bottom")

df_tot <- df_tot |> 
  mutate(position = factor(position, levels = position_levels))

# Scale robustness
df_tot <- df_tot |> 
  mutate(robustness_prop_2 = robustness_ms_2 / 13698)

# Trial Contribution ------------------------------------------------------

## Accuracy ----
df_tot |> 
  select(folder, group_id, session_trial, excluded_fixation, acc_visd) |>
  filter(excluded_fixation == "included") |> 
  drop_na(acc_visd) |> 
  distinct() |> 
  group_by(group_id, folder) |> 
  count() |> 
  group_by(folder) |> 
  summarize(mean_valid_trials = mean(n),
            sd_valid_trials = sd(n)) |>
  ungroup()

## Precision (RMS & SD) & Robustness ----
variable <- "precsd_visd" # precrms_visd or precsd_visd

df_tot |>
  select(folder, group_id, session_trial, all_of(variable)) |>
  drop_na(all_of(variable)) |>
  distinct() |> 
  group_by(group_id, folder) |>
  count() |>
  group_by(folder) |>
  summarize(
    mean_valid_trials = mean(n),
    sd_valid_trials = sd(n)
  ) |>
  ungroup()

# Chimps Adult versus Non-Adult Plot --------------------------------------
## Accuracy ----
condition <- folders[3]

df_plot_chimpadults_acc <- df_tot |>  
  filter(folder == condition) |> 
  filter(!is.na(acc_visd), !is.na(age_classification)) |> 
  mutate(age_group = case_when(
    grepl("adult", tolower(age_classification)) ~ "Adult",
    TRUE ~ "Non-Adult"),
    age_group = factor(age_group, levels = c("Non-Adult", "Adult"))) |>
  group_by(group_id, age_group) |>
  summarize(acc_visd = mean(acc_visd, na.rm = TRUE), .groups = "drop")

p_chimpadult_acc <- ggplot(df_plot_chimpadults_acc, aes(x = age_group, y = acc_visd)) +
  geom_violin(trim = FALSE, width = 0.9, fill = "white", color = "grey25", alpha = 1, linewidth = 0.8) +
  geom_jitter(aes(color = age_group), width = 0.08, height = 0, alpha = 0.65, size = 2.2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.10, linewidth = 0.8, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 3.2, color = "black") +
  labs(x = "Chimpanzee Age Classification", y = "Accuracy\n(in visual degree)", title = NULL) +
  scale_color_manual(values = c("Non-Adult" = "#F8766D","Adult"     = "#E76BF3")) +
  guides(color = "none") +
  theme_classic(base_size = 12)

png(here("exp2", "img", paste0(condition, "_acc_chimps_adults_nonadults.png")), width = 2480/2, height = 3508/4, res = 210)
p_chimpadult_acc
dev.off()

df_tot |> 
  group_by(folder, group_id) |> 
  summarize(mean_accuracy = mean(acc_visd, na.rm = T), sd_accuracy = sd(acc_visd, na.rm = T)) |> 
  ungroup() |> 
  select(folder, group_id, mean_accuracy) |>
  pivot_wider(
    names_from = folder,
    values_from = mean_accuracy
  )

## Precision (RMS) ----
condition <- folders[3]

df_plot_chimpadults_precrms <- df_tot |>  
  filter(folder == condition) |> 
  filter(!is.na(precrms_visd), !is.na(age_classification)) |> 
  mutate(age_group = case_when(
    grepl("adult", tolower(age_classification)) ~ "Adult",
    TRUE ~ "Non-Adult"),
    age_group = factor(age_group, levels = c("Non-Adult", "Adult"))) |>
  group_by(group_id, age_group) |>
  summarize(precrms_visd = mean(precrms_visd, na.rm = TRUE), .groups = "drop") |> 
  ungroup()

p_chimpadult_precrms <- ggplot(df_plot_chimpadults_precrms, aes(x = age_group, y = precrms_visd)) +
  geom_violin(trim = FALSE, width = 0.9, fill = "white", color = "grey25", alpha = 1, linewidth = 0.8) +
  geom_jitter(aes(color = age_group), width = 0.08, height = 0, alpha = 0.65, size = 2.2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.10, linewidth = 0.8, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 3.2, color = "black") +
  labs(x = "Chimpanzee Age Classification", y = "Precision (RMS)\n(in visual degrees)", title = NULL) +
  scale_color_manual(values = c("Non-Adult" = "#F8766D","Adult"     = "#E76BF3")) +
  guides(color = "none") +
  theme_classic(base_size = 12)

png(here("exp2", "img", paste0(condition, "precrms_chimps_adults_nonadults.png")), width = 2480/2, height = 3508/4, res = 210)
p_chimpadult_precrms
dev.off()

df_tot |> 
  group_by(folder, group_id) |> 
  summarize(mean_precrms = mean(precrms_visd, na.rm = T), sd_precrms = sd(precrms_visd, na.rm = T)) |> 
  ungroup() |> 
  select(folder, group_id, mean_precrms) |>
  pivot_wider(
    names_from = folder,
    values_from = mean_precrms
  ) |> 
  slice(-c(2,3,11,13,16))

## Precision (SD) ----
condition <- folders[3]

df_plot_chimpadults_precsd <- df_tot |>  
  filter(folder == condition) |> 
  filter(!is.na(precsd_visd), !is.na(age_classification)) |> 
  mutate(age_group = case_when(
    grepl("adult", tolower(age_classification)) ~ "Adult",
    TRUE ~ "Non-Adult"),
    age_group = factor(age_group, levels = c("Non-Adult", "Adult"))) |>
  group_by(group_id, age_group) |>
  summarize(precsd_visd = mean(precsd_visd, na.rm = TRUE), .groups = "drop")|> 
  ungroup()

p_chimpadult_precsd <- ggplot(df_plot_chimpadults_precsd, aes(x = age_group, y = precsd_visd)) +
  geom_violin(trim = FALSE, width = 0.9, fill = "white", color = "grey25", alpha = 1, linewidth = 0.8) +
  geom_jitter(aes(color = age_group), width = 0.08, height = 0, alpha = 0.65, size = 2.2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.10, linewidth = 0.8, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 3.2, color = "black") +
  labs(x = "Chimpanzee Age Classification", y = "Precision (SD)\n(in visual degrees)", title = NULL) +
  scale_color_manual(values = c("Non-Adult" = "#F8766D","Adult"     = "#E76BF3")) +
  guides(color = "none") +
  theme_classic(base_size = 12)

png(here("exp2", "img", paste0(condition, "precsd_chimps_adults_nonadults.png")), width = 2480/2, height = 3508/4, res = 210)
p_chimpadult_precsd
dev.off()

## Robustness ----
condition <- folders[3]

df_chimp_agegroup <- df_tot |>
  filter(folder == condition) |>
  filter(!is.na(group_id), !is.na(age_classification)) |>
  mutate(
    age_group = case_when(
      grepl("adult", tolower(age_classification)) ~ "Adult",
      TRUE ~ "Non-Adult"
    ),
    age_group = factor(age_group, levels = c("Non-Adult", "Adult"))
  ) |>
  select(group_id, age_group) |>
  distinct()

df_plot_chimpadults_rob <- df_tot |>
  filter(folder == condition) |>
  filter(!is.na(group_id), !is.na(robustness_prop_2)) |>
  select(group_id, robustness_prop_2) |>
  distinct() |>
  left_join(df_chimp_agegroup, by = "group_id") |>
  filter(!is.na(age_group))

p_chimpadult_rob <- ggplot(df_plot_chimpadults_rob, aes(x = age_group, y = robustness_prop_2)) +
  geom_violin(trim = FALSE, width = 0.9, fill = "white", color = "grey25", alpha = 1, linewidth = 0.8) +
  geom_jitter(aes(color = age_group), width = 0.08, height = 0, alpha = 0.65, size = 2.2) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.10, linewidth = 0.8, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 3.2, color = "black") +
  labs(x = "Chimpanzee Age Classification", y = "Robustness\n(in %)", title = NULL) +
  scale_color_manual(values = c("Non-Adult" = "#F8766D","Adult" = "#E76BF3")) +
  guides(color = "none") +
  theme_classic(base_size = 12)

png(here("exp2", "img", paste0(condition, "robustness_chimps_adults_nonadults.png")), width = 2480/2, height = 3508/4, res = 210)
p_chimpadult_rob
dev.off()

df_tot |> 
  group_by(folder, group_id) |> 
  summarize(mean_rob = mean(robustness_prop_2, na.rm = T), sd_rob = sd(robustness_prop_2, na.rm = T)) |> 
  ungroup() |> 
  select(folder, group_id, mean_rob) |>
  pivot_wider(
    names_from = folder,
    values_from = mean_rob
  ) |> 
  slice(c(2,3,11,13,16))

# RQ1 (Accuracy) ----------------------------------------------------------
# RQ1:  (How) does eye-tracking data quality (accuracy, precision, robustness) 
# vary within and between calibration condition (own 2-point versus human 9-point versus other chimpamzees' 5-point)?

# Dependent variables: accuracy, precision RMS, precision SD, and robustness (in separate GLMMs).
# Fixed effect variables of interest: calibration condition (own 2-point versus human 9-point versus other chimpamzees' 5-point)
# Fixed control variables: stimulus position on screen
# Random intercept: subject id
# Random slopes: maximal random effect structure

## Define Priors of Full Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_acc <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
                                    # that allows for a broad range of plausible effects while still 
                                    # providing some regularization to prevent extreme values unless strongly supported by the data.
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Full Model ----
full_acc <- brm(
  acc_visd ~ 0 + folder + position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)),
  family = Gamma(link="log"),
  prior  = prior_acc,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  seed = 123,
)

## Define Priors of Reduced Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_acc_red <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  # that allows for a broad range of plausible effects while still 
  # providing some regularization to prevent extreme values unless strongly supported by the data.
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Reduced Model ----
red_acc <- brm(
  acc_visd ~ position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)),
  family = Gamma(link="log"),
  prior  = prior_acc_red,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  seed = 123,
)

## Model Comparison ----
loo_full <- loo(full_acc)
loo_red <- loo(red_acc)
loo_compare(loo_full, loo_red) # full is substantially better than reduced red_acc  elpd_diff = -114.3 se_diff = 13.8 

## Contrasts ----
## Group
groups <- levels(df_tot$folder)
acc_contr_all <- brms_group_effects_response(
  fit   = full_acc,
  groups = groups,
  group_prefix = "folder",
  type  = "contrasts",
  ref   = NULL,          # = all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

acc_contr_all
acc_contr_all |> 
  arrange(desc(ratio_median))

## Posterior Probability Comparisons ----
draws <- as_draws_df(full_acc)
groups <- c("folderape_calibration_2p", "folderhuman_calibration_9p", "folderalex_calibration_5p")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_acc <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_acc |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp2", "img", "acc_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_acc, ndraws = 100) # it's ok
#pp_check(full_acc, type = "hist")
dev.off()

png(here("exp2", "img", "acc_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_acc, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----
# Preparation
folder_order  <- c("ape_calibration_2p", "human_calibration_9p", "alex_calibration_5p")
folder_labels <- c(
  "ape_calibration_2p"="Own\n2-Point\nCalibration",
  "human_calibration_9p"="Human\n9-Point\nCalibration",
  "alex_calibration_5p"="Conspecific\n5-Point\nCalibration"
)

pos_order <- c("center","top_right","bot_right","bottom","top_left","bot_left","top")
pos_labels <- c(
  "center"="Center",
  "top_right"="Top Right",
  "bot_right"="Bottom Right",
  "bottom"="Bottom",
  "top_left"="Top Left",
  "bot_left"="Bottom Left",
  "top"="Top"
)

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  folder   = factor(folder_order, levels = folder_order),
  position = factor(pos_order, levels = pos_order)
) |>
  mutate(position = factor(position, levels = levels(full_acc$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_acc   <- posterior_epred(full_acc, newdata = nd_pos, re_formula = NA)
acc_long <- epred_to_long(ep_acc, nd_pos)

# Create Plot
posterior_plot_rq1_acc <- ggplot(
  acc_long,
  aes(x = .epred,
      y = factor(folder, levels = rev(folder_order)),
      fill = position,
      colour = position)
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05, 
    adjust = 1.0
  ) +
  scale_y_discrete(labels = folder_labels) +
  scale_fill_discrete(name = "Position", labels = pos_labels) +
  scale_colour_discrete(name = "Position", labels = pos_labels) +
  labs(x = "Predicted Accuracy", y = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))

png(here("exp2", "img", "acc_posterior.png"),  width = 2480/1.5, height = 3508/2.2, res = 210)
posterior_plot_rq1_acc
dev.off()

## Posterior Versus Prior Plots ----

# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
png(here("exp2", "img", "acc_posteriorprior_apecalibration2p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_acc, pars = c("b_folderape_calibration_2p", "prior_b"), facet_label = "Own 2-Point Calibration")
dev.off()

png(here("exp2", "img", "acc_posteriorprior_humancalibration9p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_acc, pars = c("b_folderhuman_calibration_9p", "prior_b"), facet_label = "Human 9-Point Calibration")
dev.off()

png(here("exp2", "img", "acc_posteriorprior_alexcalibration5p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_acc, pars = c("b_folderalex_calibration_5p", "prior_b"), facet_label = "Other Chimpanzee 5-Point Calibration")
dev.off()

## Descriptives ----
# Relative improvement:
# own 2p versus human 9p: (4.11-3.22)/4.11*100= 21.66% improvement
# own 2p versus alex 5p: (4.11-2.74)/4.11*100= 33.33% improvement
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(acc_visd = mean(acc_visd, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_acc_visd = mean(acc_visd, na.rm = T),
            sd_acc_visd = sd(acc_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_acc_visd)

# Per individual
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(acc_visd = mean(acc_visd, na.rm = T)) |> 
  ungroup() |> 
  pivot_wider(names_from = folder, values_from = acc_visd) |>
  mutate(
    diff_2p_5p = ape_calibration_2p - alex_calibration_5p,
    diff_2p_9p = ape_calibration_2p - human_calibration_9p
  )

## Paper Plot ----
# Aggregate to subject level (mean over time/trials)
df_subj <- df_tot |>
  filter(!is.na(folder), !is.na(group_id), !is.na(acc_visd)) |>
  group_by(folder, group_id) |>
  summarise(
    acc_visd = mean(acc_visd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      folder,
      levels = c("ape_calibration_2p", "human_calibration_9p", "alex_calibration_5p"),
      labels = c(
        "ape_calibration_2p"="Own\n2-Point\nCalibration",
        "human_calibration_9p"="Human\n9-Point\nCalibration",
        "alex_calibration_5p"="Conspecific\n5-Point\nCalibration"
      )
      # labels = c("4M", "6M", "9M", "18M", "Adults", "Chimpanzees")
    )
  )

# Mean + 95% CI across subjects (per group)
sum_df <- df_subj |>
  group_by(Group) |>
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

p_acc <- ggplot(df_subj, aes(x = Group, y = acc_visd)) +
  geom_violin(trim = FALSE, alpha = 0.25) +
  geom_point(
    aes(color = Group),
    position = position_jitter(width = 0.12, height = 0),
    size = 0.5,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high),
    width = 0.12,
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean),
    size = 0.5,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Calibration Condition",
    y = "Accuracy\nin visual degrees"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp2", "img", "acc_paperplot.png"), width = 2480/2, height = 3508/4, res = 230)
p_acc
dev.off()

# RQ1 (Precision RMS) -----------------------------------------------------

## Define Priors of Full Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_precrms <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Full Model ----
full_precrms <- brm(
  precrms_visd ~ 0 + folder + position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)),
  family = Gamma(link="log"),
  prior  = prior_precrms,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  seed = 123,
)

## Define Priors of Reduced Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_precrms_red <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Reduced Model ----
red_precrms <- brm(
  precrms_visd ~ position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)),
  family = Gamma(link="log"),
  prior  = prior_precrms_red,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  seed = 123,
)

## Model Comparison ----
loo_full_precrms <- loo(full_precrms)
loo_red_precrms <- loo(red_precrms)
loo_compare(loo_full_precrms, loo_red_precrms) # The reduced model showed better predictive performance than the full model (ELPD difference = 7.4, SE = 3.2).

## Contrasts ----
groups <- levels(df_tot$folder)
precrms_contr_all <- brms_group_effects_response(
  fit   = full_precrms,
  groups = groups,
  group_prefix = "folder",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

precrms_contr_all
precrms_contr_all |> 
  arrange(desc(ratio_median)) |> 
  print(n = 30)

## Posterior Probability Comparisons ----
draws <- as_draws_df(full_precrms)
groups <- c("folderape_calibration_2p", "folderhuman_calibration_9p", "folderalex_calibration_5p")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_rq1_precrms <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_rq1_precrms |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Posterior Predictive Checks ----
pp_check(full_precrms, ndraws = 100) # A good model will show the observed data (usually a dark line) closely following the distribution of simulated datasets (lighter lines).

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp2", "img", "precrms_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precrms, ndraws = 100)
#pp_check(full_rq1_precrms, type = "hist") 
dev.off()

png(here("exp2", "img", "precrms_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precrms, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----
# Preparation
folder_order  <- c("ape_calibration_2p", "human_calibration_9p", "alex_calibration_5p")
folder_labels <- c(
  "ape_calibration_2p"="Own\n2-Point\nCalibration",
  "human_calibration_9p"="Human\n9-Point\nCalibration",
  "alex_calibration_5p"="Conspecific\n5-Point\nCalibration"
)

pos_order <- c("center","top_right","bot_right","bottom","top_left","bot_left","top")
pos_labels <- c(
  "center"="Center",
  "top_right"="Top Right",
  "bot_right"="Bottom Right",
  "bottom"="Bottom",
  "top_left"="Top Left",
  "bot_left"="Bottom Left",
  "top"="Top"
)

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  folder   = factor(folder_order, levels = folder_order),
  position = factor(pos_order, levels = pos_order)
) |>
  mutate(position = factor(position, levels = levels(full_precrms$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_precrms   <- posterior_epred(full_precrms, newdata = nd_pos, re_formula = NA)
precrms_long <- epred_to_long(ep_precrms, nd_pos)

# Create Plot
posterior_plot_precrms <- ggplot(
  precrms_long,
  aes(x = .epred,
      y = factor(folder, levels = rev(folder_order)), 
      fill = position,
      colour = position)
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05,
    adjust = 1.0
  ) +
  scale_y_discrete(labels = folder_labels) +
  scale_fill_discrete(name = "Position", labels = pos_labels) +
  scale_colour_discrete(name = "Position", labels = pos_labels) +
  labs(x = "Predicted Precision (RMS)", y = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1)) +
  xlim(0,0.9)

png(here("exp2", "img", "precrms_posterior.png"),  width = 2480/1.5, height = 3508/2.2, res = 210)
posterior_plot_precrms
dev.off() 

## Posterior Versus Prior Plots ----
# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
png(here("exp2", "img", "precrms_posteriorprior_apecalibration2p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precrms, pars = c("b_folderape_calibration_2p", "prior_b"), facet_label = "Own 2-Point Calibration")
dev.off()

png(here("exp2", "img", "precrms_posteriorprior_humancalibration9p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precrms, pars = c("b_folderhuman_calibration_9p", "prior_b"), facet_label = "Human 9-Point Calibration")
dev.off()

png(here("exp2", "img", "precrms_posteriorprior_alexcalibration5p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precrms, pars = c("b_folderalex_calibration_5p", "prior_b"), facet_label = "Other Chimpanzee 5-Point Calibration")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(precrms_visd = mean(precrms_visd, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_precrms_visd = mean(precrms_visd, na.rm = T),
            sd_precrms_visd = sd(precrms_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_precrms_visd)

## Paper Plot ----
# Aggregate to subject level (mean over time/trials)
df_subj <- df_tot |>
  filter(!is.na(folder), !is.na(group_id), !is.na(time), !is.na(precrms_visd)) |>
  group_by(folder, group_id) |>
  summarise(
    precrms_visd = mean(precrms_visd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      folder,
      levels = c("ape_calibration_2p", "human_calibration_9p", "alex_calibration_5p"),
      labels = c(
        "ape_calibration_2p"="Own\n2-Point\nCalibration",
        "human_calibration_9p"="Human\n9-Point\nCalibration",
        "alex_calibration_5p"="Conspecific\n5-Point\nCalibration"
      )
    )
  )

# Mean + 95% CI across subjects (per group)
sum_df <- df_subj |>
  group_by(Group) |>
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

p_precrms <- ggplot(df_subj, aes(x = Group, y = precrms_visd)) +
  geom_violin(trim = FALSE, alpha = 0.25) +
  geom_point(
    aes(color = Group),
    position = position_jitter(width = 0.12, height = 0),
    size = 0.5,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high),
    width = 0.12,
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean),
    size = 0.5,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Calibration Condition",
    y = "Precision (RMS)\nin visual degrees"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp2", "img", "precrms_paperplot.png"), width = 2480/2, height = 3508/4, res = 200)
p_precrms
dev.off()

# RQ1 (Precision SD) ------------------------------------------------------

## Define Priors of Full Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_precsd <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Full Model ----
full_precsd <- brm(
  precsd_visd ~ 0 + folder + position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)),
  family = Gamma(link="log"),
  prior  = prior_precsd,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  seed = 123,
)

## Define Priors of Reduced Model----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_precsd_red <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor"), # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
  
  prior(exponential(0.5), class = "shape") # shape parameter of gamma distribution
)

## Reduced Model ----
red_precsd <- brm(
  precsd_visd ~ position + (1 + position | group_id),
  data   = df_tot |> filter(!is.na(position)),
  family = Gamma(link="log"),
  prior  = prior_precsd_red,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  seed = 123,
)

## Model Comparison ----
loo_full_precsd <- loo(full_precsd)
loo_red_precsd <- loo(red_precsd)
loo_compare(loo_full_precsd, loo_red_precsd) # full_precsd -5.1 6.1 

## Contrasts ----
groups <- levels(df_tot$folder)
precsd_contr_all <- brms_group_effects_response(
  fit   = full_precsd,
  groups = groups,
  group_prefix = "folder",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

precsd_contr_all
precsd_contr_all |> 
  arrange(desc(ratio_median)) |> 
  print(n=30)

## Posterior Probability Comparisons ----
draws <- as_draws_df(full_precsd)
groups <- c("folderape_calibration_2p", "folderhuman_calibration_9p", "folderalex_calibration_5p")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_rq1_precsd <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_rq1_precsd |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp2", "img", "precsd_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precsd, ndraws = 100) 
dev.off()

png(here("exp2", "img", "precsd_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_precsd, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----
# Preparation
folder_order  <- c("ape_calibration_2p", "human_calibration_9p", "alex_calibration_5p")
folder_labels <- c(
  "ape_calibration_2p"="Own\n2-Point\nCalibration",
  "human_calibration_9p"="Human\n9-Point\nCalibration",
  "alex_calibration_5p"="Conspecific\n5-Point\nCalibration"
)

pos_order <- c("center","top_right","bot_right","bottom","top_left","bot_left","top")
pos_labels <- c(
  "center"="Center",
  "top_right"="Top Right",
  "bot_right"="Bottom Right",
  "bottom"="Bottom",
  "top_left"="Top Left",
  "bot_left"="Bottom Left",
  "top"="Top"
)

# Create Newdata Grid
nd_pos <- tidyr::expand_grid(
  folder   = factor(folder_order, levels = folder_order),
  position = factor(pos_order, levels = pos_order)
) |>
  mutate(position = factor(position, levels = levels(full_precsd$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_precsd   <- posterior_epred(full_precsd, newdata = nd_pos, re_formula = NA)
precsd_long <- epred_to_long(ep_precsd, nd_pos)

# Create Plot
posterior_plot_precsd <- ggplot(
  precsd_long,
  aes(x = .epred,
      y = factor(folder, levels = rev(folder_order)),
      fill = position,
      colour = position)
) +
  stat_halfeye(
    point_interval = "median_qi", # median_hdi
    position = position_dodge(width = 0.80),
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05,
    adjust = 1.0
  ) +
  scale_y_discrete(labels = folder_labels) +
  scale_fill_discrete(name = "Position", labels = pos_labels) +
  scale_colour_discrete(name = "Position", labels = pos_labels) +
  labs(x = "Predicted Precision (SD)", y = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))

png(here("exp2", "img", "precsd_posterior.png"),  width = 2480/1.5, height = 3508/2.2, res = 210)
posterior_plot_precsd
dev.off() 

## Posterior Versus Prior Plots ----
png(here("exp2", "img", "precsd_posteriorprior_apecalibration2p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precsd, pars = c("b_folderape_calibration_2p", "prior_b"), facet_label = "Own 2-Point Calibration")
dev.off()

png(here("exp2", "img", "precsd_posteriorprior_humancalibration9p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precsd, pars = c("b_folderhuman_calibration_9p", "prior_b"), facet_label = "Human 9-Point Calibration")
dev.off()

png(here("exp2", "img", "precsd_posteriorprior_alexcalibration5p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_precsd, pars = c("b_folderalex_calibration_5p", "prior_b"), facet_label = "Other Chimpanzee 5-Point Calibration")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(precsd_visd = mean(precsd_visd, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_precsd_visd = mean(precsd_visd, na.rm = T),
            sd_precsd_visd = sd(precsd_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_precsd_visd)

## Paper Plot ----

# Aggregate to subject level (mean over time/trials)
df_subj <- df_tot |>
  filter(!is.na(folder), !is.na(group_id), !is.na(time), !is.na(precsd_visd)) |>
  group_by(folder, group_id) |>
  summarise(
    precsd_visd = mean(precsd_visd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      folder,
      levels = c("ape_calibration_2p", "human_calibration_9p", "alex_calibration_5p"),
      labels = c(
        "ape_calibration_2p"="Own\n2-Point\nCalibration",
        "human_calibration_9p"="Human\n9-Point\nCalibration",
        "alex_calibration_5p"="Conspecific\n5-Point\nCalibration"
        )
      )
  )


# Mean + 95% CI across subjects (per group)
sum_df <- df_subj |>
  group_by(Group) |>
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

p_precsd <- ggplot(df_subj, aes(x = Group, y = precsd_visd)) +
  geom_violin(trim = FALSE, alpha = 0.25) +
  geom_point(
    aes(color = Group),
    position = position_jitter(width = 0.12, height = 0),
    size = 0.5,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high),
    width = 0.12,
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean),
    size = 0.5,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Calibration Condition",
    y = "Precision (SD)\nin visual degrees"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp2", "img", "precsd_paperplot.png"), width = 2480/2, height = 3508/4, res = 200)
p_precsd
dev.off()

# RQ1 (Robustness) ----------------------------------------------------------

# Note: Beta() requires outcomes strictly in (0, 1). If exact 0 or 1 occur,
# clamp slightly or use zero_one_inflated_beta().

## Define Priors of Full Model ----
priors_rob <- c(
  prior(normal(0, 3), class = "b"), # all fixed effects: normal(0, 3) on log-mean scale is a wide prior 
  prior(exponential(1), class = "phi"), # same as in Exp1, for Beta distribution
  prior(exponential(1), class = "sd")   # same as in Exp1, for random effects regularization
)

## Full Model ----
full_rob <- brm(
  robustness_prop_2 ~ 0 + folder + (1 | group_id),
  data   = df_tot |> filter(!is.na(robustness_prop_2)) |> select(folder, group_id,robustness_prop_2) |> distinct(),
  family = Beta(link = "logit"),
  prior  = priors_rob,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

## Define Priors of Reduced Model ----
priors_rob_red <- c(
  prior(exponential(1), class = "phi"), # same as in Exp1, for Beta distribution
  prior(exponential(1), class = "sd")   # same as in Exp1, for random effects regularization
)

## Reduced Model ----
red_rob <- brm(
  robustness_prop_2 ~ (1 | group_id),
  data   = df_tot |> filter(!is.na(robustness_prop_2)) |> select(folder, group_id,robustness_prop_2) |> distinct(),
  family = Beta(link = "logit"),
  prior  = priors_rob_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

## Model Comparison ----
loo_full_rob <- loo(full_rob)
loo_red_rob <- loo(red_rob)
loo_compare(loo_full_rob, loo_red_rob) # full_rob -1.6       0.3

## Contrasts ----
groups <- levels(df_tot$folder)
rob_contr_all <- brms_group_effects_response(
  fit   = full_rob,
  groups = groups,
  group_prefix = "folder",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

rob_contr_all |> 
  arrange(desc(ratio_median)) |> 
  print(n=30)

## Posterior Probability Comparisons ----
draws <- as_draws_df(full_rob)
groups <- c("folderape_calibration_2p", "folderhuman_calibration_9p", "folderalex_calibration_5p")
pairs <- t(combn(groups, 2)) |> as.data.frame()
colnames(pairs) <- c("g1", "g2")

results_rob <- pairs |> 
  rowwise() |> 
  do(get_prob(.$g1, .$g2, draws)) |> 
  ungroup()

results_rob |> 
  mutate(
    contrast = gsub("folder", "", contrast),
    prob_g1_greater = round(prob_g1_greater, 3),
    prob_g2_greater = round(prob_g2_greater, 3),
    median = round(median, 2),
    lo = round(lo, 2),
    hi = round(hi, 2)
  )

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp2", "img", "rob_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rob, ndraws = 200) 
dev.off()

png(here("exp2", "img", "rob_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rob, type = "intervals_grouped", group = "folder")
dev.off()

## Posterior Distribution ----

# Preparation
folder_order  <- c("ape_calibration_2p", "human_calibration_9p", "alex_calibration_5p")
folder_labels <- c(
  "ape_calibration_2p"="Own\n2-Point\nCalibration",
  "human_calibration_9p"="Human\n9-Point\nCalibration",
  "alex_calibration_5p"="Conspecific\n5-Point\nCalibration"
)

# Create Newdata Grid (Without Position)
nd_rob <- tibble(folder = factor(folder_order, levels = folder_order)) |>
  mutate(folder = factor(folder, levels = levels(full_rob$data$folder))) |>
  filter(!is.na(folder))

# Create Predictions
ep_rob   <- posterior_epred(full_rob, newdata = nd_rob, re_formula = NA)
rob_long <- epred_to_long(ep_rob, nd_rob)

# Create Plot
posterior_plot_rob <- ggplot(
  rob_long,
  aes(x = .epred,
      y = factor(folder, levels = rev(folder_order)))) +
  stat_halfeye(point_interval = "median_qi", .width = c(0, 0.95), # median_hdi
               alpha = 0.65, height = 1.05, adjust = 1.0) +
  scale_y_discrete(labels = folder_labels) +
  labs(x = "Predicted Robustness", y = NULL) +
  theme_bw(base_size = 14)

# Save

## Posterior Versus Prior Plots ----

# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
png(here("exp2", "img", "rob_posteriorprior_apecalibration2p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rob, pars = c("b_folderape_calibration_2p", "prior_b"), facet_label = "Own 2-Point Calibration")
dev.off()

png(here("exp2", "img", "rob_posteriorprior_humancalibration9p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rob, pars = c("b_folderhuman_calibration_9p", "prior_b"), facet_label = "Human 9-Point Calibration")
dev.off()

png(here("exp2", "img", "rob_posteriorprior_alexcalibration5p.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rob, pars = c("b_folderalex_calibration_5p", "prior_b"), facet_label = "Other Chimpanzee 5-Point Calibration")
dev.off()

## Descriptives ----
df_tot |>
  select(group_id, folder, robustness_prop_2) |>
  drop_na(robustness_prop_2) |>
  group_by(folder, group_id) |> 
  # summarize(robustness_ms_2 = mean(robustness_ms_2, na.rm = T)) |> 
  summarize(robustness_prop_2 = mean(robustness_prop_2, na.rm = T)) |> 
  group_by(folder) |> 
  # summarize(mean_robustness_ms = mean(robustness_ms, na.rm = T),
  #           sd_robustness_ms = sd(robustness_ms, na.rm = T)) |> 
  summarize(mean_robustness_prop_2 = mean(robustness_prop_2, na.rm = T),
            sd_robustness_prop_2 = sd(robustness_prop_2, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_robustness_prop_2)

# df_tot |> 
#   group_by(folder, group_id) |> 
#   summarize(robustness_ms_2 = mean(robustness_ms_2, na.rm = T)) |> 
#   group_by(folder) |> 
#   summarize(mean_robustness_ms_2 = mean(robustness_ms_2, na.rm = T),
#             sd_robustness_ms_2 = sd(robustness_ms_2, na.rm = T)) |>
#   ungroup() |> 
#   arrange(sd_robustness_ms_2)
# 
# df_tot |>
#   group_by(folder, group_id) |>
#   summarize(robustness_prop = mean(robustness_prop, na.rm = T)) |>
#   group_by(folder) |>
#   summarize(mean_robustness_prop = mean(robustness_prop, na.rm = T),
#             sd_robustness_prop = sd(robustness_prop, na.rm = T)) |>
#   ungroup() |>
#   arrange(mean_robustness_prop)

## Paper Plot ----
# Aggregate to subject level (mean over time/trials)
df_subj <- df_tot |>
  select(robustness_prop_2, folder, group_id) |> 
  distinct() |> 
  group_by(folder, group_id) |>
  summarise(
    robustness_prop_2 = mean(robustness_prop_2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      folder,
      levels = c("ape_calibration_2p", "human_calibration_9p", "alex_calibration_5p"),
      labels = c(
        "ape_calibration_2p"="Own\n2-Point\nCalibration",
        "human_calibration_9p"="Human\n9-Point\nCalibration",
        "alex_calibration_5p"="Conspecific\n5-Point\nCalibration"
      )
    )
  )

# Mean + 95% CI across subjects (per group)
sum_df <- df_subj |>  
  group_by(Group) |>
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

p_robust <- ggplot(df_subj, aes(x = Group, y = robustness_prop_2)) +
  geom_violin(trim = FALSE, alpha = 0.25) +
  geom_point(
    aes(color = Group),
    position = position_jitter(width = 0.12, height = 0),
    size = 0.5,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high),
    width = 0.12,
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean),
    size = 0.5,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Calibration Condition",
    y = "Robustness\nin %"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp2", "img", "rob_paperplot.png"), width = 2480/2, height = 3508/4, res = 200)
p_robust
dev.off()


