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

# Load Functions ----------------------------------------------------------
source(here("exp1", "R", "descriptives.R"))
source(here("exp1", "R", "inferentials.R"))
source(here("exp1", "R", "utils.R"))
source(here("exp1", "R", "viz.R"))

# Set Parameters ----------------------------------------------------------
n_cores <- parallel::detectCores()

# Read Data ---------------------------------------------------------------
folders <- c("4m", "6m", "9m", "18m", "adults", "chimps")

dfs <- folders |>
  set_names() |>
  map(read_folder)

df_4m     <- dfs[["4m"]] |> mutate(no_siblings  = as.character(no_siblings), no_household = as.character(no_household))
df_6m     <- dfs[["6m"]] |> mutate(no_siblings  = as.character(no_siblings), no_household = as.character(no_household))
df_9m     <- dfs[["9m"]] |> mutate(no_household = as.character(no_household))
df_18m    <- dfs[["18m"]]
df_adults <- dfs[["adults"]]
df_chimps <- dfs[["chimps"]]

day_session <- read_excel(here("exp1", "doc", "day_session_apes.xlsx"))

# Prepare Data ------------------------------------------------------------
df_tot <- df_4m |> 
  bind_rows(df_6m, df_9m, df_18m, df_adults) |> 
  bind_rows(df_chimps)|> 
  mutate(
    folder   = factor(folder),
    position = factor(position),
    group_id = factor(group_id)
  ) |> 
  mutate(folder = fct_relevel(folder, "chimps"))

table(df_tot$acc_visd == 0, useNA = "ifany") # Check for exact zeros (Gamma can't take zeros, but gamma_hurdle can)

# Create time variable (trial for humans, session_trial for chimps)
df_tot$time <- ifelse(!is.na(df_tot$session_trial),
                      df_tot$session_trial,
                      as.character(df_tot$trial))
df_tot <- df_tot |>
  mutate(
    time_1 = gsub("^session", "", time), # remove leading "session"
    time_1 = sub("_.*$", "", time_1),    # drop "_" and everything after
    time_1 = as.numeric(time_1)          # convert to numeric
  )

df_tot <- df_tot |> 
  mutate(
    time_2 = if_else(grepl("_", time), sub("^.*_", "", time), time), # remove everything until including _
    time_2 = as.numeric(time_2)
  )

# Order levels of position (in order to make "center" the reference category)
position_levels <- c("center", "top_left", "top_right", "bot_left", "bot_right", "top", "bottom")

df_tot <- df_tot |> 
  mutate(position = factor(position, levels = position_levels))

# Scale robustness
df_tot <- df_tot |> 
  mutate(robustness_prop_2 = robustness_ms_2 / 53466)

# Change name of group column
# (in order to make marginaleffects work)
df_tot <- df_tot |>
  rename(species_group = group)

# Add day
df_tot <- df_tot |> 
  separate(session_trial, into = c("session", "trial"), sep = "_", remove = FALSE) |>
  left_join(day_session, by = c("group_id", "session", "species_group"))

df_tot <- df_tot |> 
  mutate(time_3 = if_else(folder == "chimps", as.numeric(day), as.numeric(time)))

# Legend for time:
# In chimps: time_1 = session, time_2 = trial within session, time_3 = day

# Trial Contribution ------------------------------------------------------

## Accuracy ----
df_tot |> 
  select(folder, group_id, excluded_fixation, acc_visd) |>
  filter(excluded_fixation == "included") |> 
  drop_na(acc_visd) |> 
  group_by(group_id, folder) |> 
  count() |> 
  group_by(folder) |> 
  summarize(mean_valid_trials = mean(n),
            sd_valid_trials = sd(n)) |>
  ungroup() |> 
  slice(c(3,4,5,2,6,1))

## Precision (RMS & SD) & Robustness ----
variable <- "precrms_visd" # precrms_visd or precsd_visd

df_tot |>
  select(folder, group_id, time, all_of(variable)) |>
  drop_na(all_of(variable)) |>
  group_by(group_id, folder) |>
  count() |>
  group_by(folder) |>
  summarize(
    mean_valid_trials = mean(n),
    sd_valid_trials = sd(n)
  ) |>
  ungroup() |>
  slice(c(3, 4, 5, 2, 6, 1))


# Chimps Adult versus Non-Adult Plot --------------------------------------
## Accuracy ----
df_plot_chimpadults_acc <- df_tot |>  
  filter(folder == "chimps") |> 
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

png(here("exp1", "img", "acc_chimps_adults_nonadults.png"), width = 2480/2, height = 3508/4, res = 210)
p_chimpadult_acc
dev.off()

## Precision (RMS) ----
df_plot_chimpadults_precrms <- df_tot |>  
  filter(folder == "chimps") |> 
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

png(here("exp1", "img", "precrms_chimps_adults_nonadults.png"), width = 2480/2, height = 3508/4, res = 210)
p_chimpadult_precrms
dev.off()

## Precision (SD) ----
df_plot_chimpadults_precsd <- df_tot |>  
  filter(folder == "chimps") |> 
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

png(here("exp1", "img", "precsd_chimps_adults_nonadults.png"), width = 2480/2, height = 3508/4, res = 210)
p_chimpadult_precsd
dev.off()

## Robustness ----
df_chimp_agegroup <- df_tot |>
  filter(folder == "chimps") |>
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
  filter(folder == "chimps") |>
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

png(here("exp1", "img", "robustness_chimps_adults_nonadults.png"), width = 2480/2, height = 3508/4, res = 210)
p_chimpadult_rob
dev.off()

# RQ1 (Accuracy) ----------------------------------------------------------
# RQ1:  (How) does eye-tracking data quality (accuracy, precision, robustness) 
# vary within and between groups (4-, 6-, 9-, and 18-month-old human infants, 
# human adults, and chimpanzees) (Experiment 1)?

# Preregistered Model:
# Dependent variables: accuracy, precision RMS, precision SD, and robustness (in separate GLMMs).
# Fixed effect variables of interest: group (4-, 6-, 9-, 18-month-old human infants, human adults, chimpanzees)
# Fixed control variables: stimulus position on screen
# Random intercept: subject id
# Random slopes: maximal random effect structure
# 
# Full model: data quality ~ group + position on screen + random effects
# Reduced model: data quality ~ position on screen + random effects
# 
# Priors (accuracy, fixed effect group): 
#   Shape: 2.39.
# Regression coefficients:	
#   M = 0.50 and SD = 0.48 (4-month-olds),
# M = 0.50 and SD = 0.48 (6-month-olds),	
# M = 0.24 and SD = 0.09 (9-month-olds),
# M = 0.00 and SD = 0.09 (18-month-olds),	
# M = -0.41 and SD = 0.08 (adults), and
# M = 0.75 and SD = 0.24 (chimpanzees).

## Define Priors of Full Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_rq1_acc <- c(
  # Group parameters (log-mean scale)
  prior(normal(0.50, 0.48), class = "b", coef = "folder4m"),
  prior(normal(0.50, 0.48), class = "b", coef = "folder6m"),
  prior(normal(0.24, 0.09), class = "b", coef = "folder9m"),
  prior(normal(0.00, 0.09), class = "b", coef = "folder18m"),
  prior(normal(-0.41, 0.08), class = "b", coef = "folderadults"),
  prior(normal(0.75, 0.24), class = "b", coef = "folderchimps"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -0.41 - 0.24 = -0.65, so half of that is 0.325)
  prior(normal(0, 0.325), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.325), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.325), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.325), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.325), class = "b", coef = "positiontop"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_right"),
  
  # Gamma shape (estimate = 2.39 and est. error = 0.06 on a natural scale,
  # which translates to meanlog = 0.87097834 and sdlog = 0.02510065 on log scale,
  # m <- 2.39
  # s <- 0.06
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02510065
  # meanlog <- log(m) - sigma2/2         # 0.87097834
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(0.87097834, 2*0.02510065), class = "shape"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),   # Prior on random-effect SD (log scale with log link): concentrates mass on small-to-moderate heterogeneity
                                         # (median ~0.35, mean ~0.50) while still allowing larger SDs if supported by the data.
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
                               # supported, improving computational stability in random-slope models
)

## Full Model ----
t0 <- proc.time()
full_rq1_acc <- brm(
  acc_visd ~ 0 + folder + position + (1 + position | group_id),
  data   = df_tot,
  family = hurdle_gamma(link = "log"),
  prior  = prior_rq1_acc,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  seed = 123,
)
t1 <- proc.time()
proc_time_rq1_acc <- t1 - t0
rm(t0, t1)

## Define Priors of Reduced Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_rq1_acc_red <- c(
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -0.41 - 0.24 = -0.65, so half of that is 0.325)
  prior(normal(0, 0.325), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.325), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.325), class = "b", coef = "positionbottom"),
  prior(normal(0, 0.325), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.325), class = "b", coef = "positiontop"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_right"),
  
  # Gamma shape (estimate = 2.39 and est. error = 0.06 on a natural scale,
  # which translates to meanlog = 0.87097834 and sdlog = 0.02510065 on log scale,
  # m <- 2.39
  # s <- 0.06
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02510065
  # meanlog <- log(m) - sigma2/2         # 0.87097834
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(0.87097834, 2*0.02510065), class = "shape"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),   # Prior on random-effect SD (log scale with log link): concentrates mass on small-to-moderate heterogeneity
  # (median ~0.35, mean ~0.50) while still allowing larger SDs if supported by the data.
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

## Reduced Model ----
t0 <- proc.time()
red_rq1_acc <- brm(
  acc_visd ~ position + (1 + position | group_id),
  data   = df_tot,
  family = hurdle_gamma(link = "log"),
  prior  = prior_rq1_acc_red,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  # control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 123,
  # save_pars = save_pars(all = TRUE)
)
t1 <- proc.time()
proc_time_rq1_acc <- t1 - t0
rm(t0, t1)

## Model Comparison ----
loo_full <- loo(full_rq1_acc)
loo_red <- loo(red_rq1_acc)
loo_compare(loo_full, loo_red) # red_rq1_acc: elpd_diff = -9.1 & se_diff = 4.4.
                               # moderate evidence that the full model is better (difference is ~2 SE away from 0).

## Contrasts ----
## Group
groups <- levels(df_tot$folder)
acc_contr_all <- brms_group_effects_response(
  fit   = full_rq1_acc,
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

## Position
positions <- levels(df_tot$position)[2:7]
acc_contr_all_pos <- brms_group_effects_response(
  fit   = full_rq1_acc,
  groups = positions,
  group_prefix = "position",
  type  = "contrasts",
  ref   = NULL,
  link  = "log",
  contrast_scale = "ratio"
)

acc_contr_all_pos
acc_contr_all_pos |> 
  arrange(desc(ratio_median))

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"

png(here("exp1", "img", "rq1_acc_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq1_acc, ndraws = 100) # rq1 accuracy check, that's fine
#pp_check(full_rq1_acc, type = "hist") # rq1 accuracy check, that's fine
dev.off()

png(here("exp1", "img", "rq1_acc_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq1_acc, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----
# Preparation
folder_order  <- c("4m","6m","9m","18m","adults","chimps")
folder_labels <- c(
  "4m"="4 Months","6m"="6 Months","9m"="9 Months","18m"="18 Months",
  "adults"="Adults","chimps"="Chimpanzees"
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
  mutate(position = factor(position, levels = levels(full_rq1_acc$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_acc   <- posterior_epred(full_rq1_acc, newdata = nd_pos, re_formula = NA)
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
    point_interval = "median_qi",
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


png(here("exp1", "img", "rq1_acc_posterior.png"), width = 2480, height = 3508/3, res = 180)
posterior_plot_rq1_acc
dev.off()

## Posterior Versus Prior Plots ----

# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
png(here("exp1", "img", "rq1_acc_posteriorprior_chimps.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_acc, pars = c("b_folderchimps", "prior_b_folderchimps"), facet_label = "Chimpanzees")
dev.off()

png(here("exp1", "img", "rq1_acc_posteriorprior_4m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_acc, pars = c("b_folder4m", "prior_b_folder4m"), facet_label = "4-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_acc_posteriorprior_6m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_acc, pars = c("b_folder6m", "prior_b_folder6m"), facet_label = "6-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_acc_posteriorprior_9m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_acc, pars = c("b_folder9m", "prior_b_folder9m"), facet_label = "9-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_acc_posteriorprior_18m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_acc, pars = c("b_folder18m", "prior_b_folder18m"), facet_label = "18-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_acc_posteriorprior_adults.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_acc, pars = c("b_folderadults", "prior_b_folderadults"), facet_label = "Adults")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(acc_visd = mean(acc_visd, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_acc_visd = mean(acc_visd, na.rm = T),
            sd_acc_visd = sd(acc_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_acc_visd)

## Others ----
# Calculate posterior probability that accuracy in A is higher than in B
mean(post_samples_rq1_acc$b_folderchimps > post_samples_rq1_acc$b_folder4m) # 1 # 
# "The posterior probability that chimpanzees have a worse accuracy than 4-month-old infants, is 1."
mean(post_samples_rq1_acc$b_folder4m > post_samples_rq1_acc$b_folder9m) # 96%
mean(post_samples_rq1_acc$b_folder9m > post_samples_rq1_acc$b_folder18m) # 90%
mean(post_samples_rq1_acc$b_folder18m > post_samples_rq1_acc$b_folder6m) # 89%
mean(post_samples_rq1_acc$b_folder9m > post_samples_rq1_acc$b_folder6m) # 99.35%
mean(post_samples_rq1_acc$Yesb_folder6m > post_samples_rq1_acc$b_folderadults) # 99.9875%

## Paper Plot ----
# Aggregate to subject level (mean over time/trials)
df_subj <- df_tot |>
  filter(!is.na(folder), !is.na(group_id), !is.na(time), !is.na(acc_visd)) |>
  group_by(folder, group_id) |>
  summarise(
    acc_visd = mean(acc_visd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      folder,
      levels = c("4m", "6m", "9m", "18m", "adults", "chimps"),
      labels = c("4 Months", "6 Months", "9 Months", "18 Months", "Adults", "Chimpanzees")
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
    x = "Group",
    y = "Accuracy\nin visual degrees"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp1", "img", "rq1_acc_paperplot.png"), width = 2480/2, height = 3508/4, res = 200)
p_acc
dev.off()


# RQ1 (Precision RMS) -----------------------------------------------------

## Define Priors of Full Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_rq1_precrms <- c(
  # Group parameters (log-mean scale)
  prior(normal(-0.88, 0.32), class = "b", coef = "folder4m"),
  prior(normal(-0.88, 0.32), class = "b", coef = "folder6m"),
  prior(normal(-1.64, 0.08), class = "b", coef = "folder9m"),
  prior(normal(-1.82, 0.07), class = "b", coef = "folder18m"),
  prior(normal(-2.03, 0.07), class = "b", coef = "folderadults"),
  prior(normal(-0.11, 0.16), class = "b", coef = "folderchimps"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -2.03 - (-1.64) = -0.39, so half of that is 0.195)
  prior(normal(0, 0.195), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.195), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.195), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.195), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.195), class = "b", coef = "positiontop"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_right"),
  
  # Gamma shape (estimate = 7.42 and est. error = 0.19 on a natural scale,
  # which translates to meanlog = 2.004 and sdlog = 0.0256 on log scale,
  # m <- 7.42
  # s <- 0.19
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.0256
  # meanlog <- log(m) - sigma2/2         # 2.004 (approx)
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(2.004, 2*0.0256), class = "shape"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

## Full Model ----
t0 <- proc.time()
full_rq1_precrms <- brm(
  precrms_visd ~ 0 + folder + position + (1 + position | group_id),
  data   = df_tot,
  family = Gamma(link="log"),
  prior  = prior_rq1_precrms,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  # control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 123,
  # save_pars = save_pars(all = TRUE)
)
t1 <- proc.time()
proc_time_rq1_precrms <- t1 - t0
rm(t0, t1)

## Define Priors of Reduced Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_rq1_precrms_red <- c(
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -2.03 - (-1.64) = -0.39, so half of that is 0.195)
  prior(normal(0, 0.195), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.195), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.195), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.195), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.195), class = "b", coef = "positiontop"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_right"),
  
  # Gamma shape (estimate = 7.42 and est. error = 0.19 on a natural scale,
  # which translates to meanlog = 2.004 and sdlog = 0.0256 on log scale,
  # m <- 7.42
  # s <- 0.19
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.0256
  # meanlog <- log(m) - sigma2/2         # 2.004 (approx)
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(2.004, 2*0.0256), class = "shape"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

## Reduced Model ----
red_rq1_precrms <- brm(
  precrms_visd ~ position + (1 + position | group_id),
  data   = df_tot,
  family = Gamma(link="log"),
  prior  = prior_rq1_precrms_red,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior = "yes",
  seed = 123,
)

## Model Comparison ----
loo_full_precrms <- loo(full_rq1_precrms)
loo_red_precrms <- loo(red_rq1_precrms)
loo_compare(loo_full_precrms, loo_red_precrms) # full_rq1_precrms -4.3       7.4 

## Contrasts ----
precrms_contr_all <- brms_group_effects_response(
  fit   = full_rq1_precrms,
  groups = groups,
  group_prefix = "folder",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

precrms_contr_all
precrms_contr_all |> 
  arrange(desc(ratio_median))

## Posterior Predictive Checks ----
pp_check(full_rq1_precrms, ndraws = 100) # A good model will show the observed data (usually a dark line) closely following the distribution of simulated datasets (lighter lines).

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp1", "img", "rq1_precrms_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq1_precrms, ndraws = 100)
#pp_check(full_rq1_precrms, type = "hist") 
dev.off()

png(here("exp1", "img", "rq1_precrms_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq1_precrms, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----
# Preparation
folder_order  <- c("4m","6m","9m","18m","adults","chimps")
folder_labels <- c(
  "4m"="4 Months","6m"="6 Months","9m"="9 Months","18m"="18 Months",
  "adults"="Adults","chimps"="Chimpanzees"
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
  mutate(position = factor(position, levels = levels(full_rq1_precrms$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_precrms   <- posterior_epred(full_rq1_precrms, newdata = nd_pos, re_formula = NA)
precrms_long <- epred_to_long(ep_precrms, nd_pos)

# Create Plot
posterior_plot_rq1_precrms <- ggplot(
  precrms_long,
  aes(x = .epred,
      y = factor(folder, levels = rev(folder_order)), 
      fill = position,
      colour = position)
) +
  stat_halfeye(
    point_interval = "median_qi",
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
  guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))

png(here("exp1", "img", "rq1_precrms_posterior.png"), width = 2480/2, height = 3508/2.5, res = 200)
posterior_plot_rq1_precrms
dev.off() 

## Posterior Versus Prior Plots ----
# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
png(here("exp1", "img", "rq1_precrms_posteriorprior_chimps.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precrms, pars = c("b_folderchimps", "prior_b_folderchimps"), facet_label = "Chimpanzees")
dev.off()

png(here("exp1", "img", "rq1_precrms_posteriorprior_4m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precrms, pars = c("b_folder4m", "prior_b_folder4m"), facet_label = "4-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_precrms_posteriorprior_6m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precrms, pars = c("b_folder6m", "prior_b_folder6m"), facet_label = "6-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_precrms_posteriorprior_9m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precrms, pars = c("b_folder9m", "prior_b_folder9m"), facet_label = "9-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_precrms_posteriorprior_18m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precrms, pars = c("b_folder18m", "prior_b_folder18m"), facet_label = "18-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_precrms_posteriorprior_adults.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precrms, pars = c("b_folderadults", "prior_b_folderadults"), facet_label = "Adults")
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

## Others ----
# Calculate posterior probability that precision (RMS) in A is higher than in B
mean(post_samples_rq1_precrms$b_folderadults < post_samples_rq1_precrms$b_folderchimps) # 1
mean(post_samples_rq1_precrms$b_folderchimps < post_samples_rq1_precrms$b_folder9m) # 0
mean(post_samples_rq1_precrms$b_folder9m < post_samples_rq1_precrms$b_folder18m) # 0.06075
mean(post_samples_rq1_precrms$b_folder18m < post_samples_rq1_precrms$b_folder6m) # 1
mean(post_samples_rq1_precrms$b_folder6m < post_samples_rq1_precrms$b_folder4m) # 0.96225

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
      levels = c("4m", "6m", "9m", "18m", "adults", "chimps"),
      # labels = c("4M", "6M", "9M", "18M", "Adults", "Chimpanzees")
      labels = c("4 Months", "6 Months", "9 Months", "18 Months", "Adults", "Chimpanzees")
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
    x = "Group",
    y = "Precision (RMS)\nin visual degrees"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp1", "img", "rq1_precrms_paperplot.png"), width = 2480/2, height = 3508/4, res = 200)
p_precrms
dev.off()

# RQ1 (Precision SD) ------------------------------------------------------

## Define Priors of Full Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_rq1_precsd <- c(
  # Group parameters (log-mean scale)
  prior(normal(-1.03, 0.28), class = "b", coef = "folder4m"),
  prior(normal(-1.03, 0.28), class = "b", coef = "folder6m"),
  prior(normal(-1.61, 0.06), class = "b", coef = "folder9m"),
  prior(normal(-1.68, 0.06), class = "b", coef = "folder18m"),
  prior(normal(-1.87, 0.05), class = "b", coef = "folderadults"),
  prior(normal(-0.45, 0.14), class = "b", coef = "folderchimps"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -1.87 - (-1.61) = 0.26, so half of it is 0.13)
  prior(normal(0, 0.13), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.13), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.13), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.13), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.13), class = "b", coef = "positiontop"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_right"),
  
  # Gamma shape (estimate = 6.16 and est. error = 0.15 on a natural scale,
  # which translates to meanlog = 2.004 and sdlog = 0.0256 on log scale,
  # m <- 6.16
  # s <- 0.15
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02434704
  # meanlog <- log(m) - sigma2/2         # 1.81778039
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(1.81778039, 2*0.02434704), class = "shape"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

## Full Model ----
t0 <- proc.time()
full_rq1_precsd <- brm(
  precsd_visd ~ 0 + folder + position + (1 + position | group_id),
  data   = df_tot,
  sample_prior = "yes",
  family = Gamma(link="log"),
  prior  = prior_rq1_precsd,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)
t1 <- proc.time()
proc_time_rq1_precsd <- t1 - t0
rm(t0, t1)

## Define Priors of Reduced Model----
# With Gamma(link="log"), coefficients are on the log-mean scale.
prior_rq1_precsd_red <- c(
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -1.87 - (-1.61) = 0.26, so half of it is 0.13)
  prior(normal(0, 0.13), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.13), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.13), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.13), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.13), class = "b", coef = "positiontop"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_right"),
  
  # Gamma shape (estimate = 6.16 and est. error = 0.15 on a natural scale,
  # which translates to meanlog = 2.004 and sdlog = 0.0256 on log scale,
  # m <- 6.16
  # s <- 0.15
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02434704
  # meanlog <- log(m) - sigma2/2         # 1.81778039
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(1.81778039, 2*0.02434704), class = "shape"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

## Reduced Model ----
red_rq1_precsd <- brm(
  precsd_visd ~ position + (1 + position | group_id),
  data   = df_tot,
  sample_prior = "yes",
  family = Gamma(link="log"),
  prior  = prior_rq1_precsd_red,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

## Model Comparison ----
loo_full_precsd <- loo(full_rq1_precsd)
loo_red_precsd <- loo(red_rq1_precsd)
loo_compare(loo_full_precsd, loo_red_precsd)

## Contrasts ----
precsd_contr_all <- brms_group_effects_response(
  fit   = full_rq1_precsd,
  groups = groups,
  group_prefix = "folder",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

precsd_contr_all
precsd_contr_all |> 
  arrange(desc(ratio_median))

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp1", "img", "rq1_precsd_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq1_precsd, ndraws = 100) 
#pp_check(full_rq1_precsd, type = "hist")
dev.off()

png(here("exp1", "img", "rq1_precsd_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq1_precsd, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----
# Preparation
folder_order  <- c("4m","6m","9m","18m","adults","chimps")
folder_labels <- c(
  "4m"="4 Months","6m"="6 Months","9m"="9 Months","18m"="18 Months",
  "adults"="Adults","chimps"="Chimpanzees"
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
  mutate(position = factor(position, levels = levels(full_rq1_precsd$data$position))) |>
  filter(!is.na(position))

# Create Predictions
ep_precsd   <- posterior_epred(full_rq1_precsd, newdata = nd_pos, re_formula = NA)
precsd_long <- epred_to_long(ep_precsd, nd_pos)

# Create Plot
posterior_plot_rq1_precsd <- ggplot(
  precsd_long,
  aes(x = .epred,
      y = factor(folder, levels = rev(folder_order)),
      fill = position,
      colour = position)
) +
  stat_halfeye(
    point_interval = "median_qi",
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

png(here("exp1", "img", "rq1_precsd_posterior.png"), width = 2480/2, height = 3508/2.5, res = 200)
posterior_plot_rq1_precsd
dev.off() 

## Posterior Versus Prior Plots ----
png(here("exp1", "img", "rq1_precsd_posteriorprior_4m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precsd, pars = c("b_folder4m", "prior_b_folder4m"), facet_label = "4-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_precsd_posteriorprior_6m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precsd, pars = c("b_folder6m", "prior_b_folder6m"), facet_label = "6-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_precsd_posteriorprior_9m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precsd, pars = c("b_folder9m", "prior_b_folder9m"), facet_label = "9-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_precsd_posteriorprior_18m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precsd, pars = c("b_folder18m", "prior_b_folder18m"), facet_label = "18-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_precsd_posteriorprior_adults.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precsd, pars = c("b_folderadults", "prior_b_folderadults"), facet_label = "Adults")
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

## Others ----
# Calculate posterior probability that precision (SD) in A is higher than in B
mean(post_samples_rq1_precsd$b_folderadults < post_samples_rq1_precsd$b_folder9m) # 1
mean(post_samples_rq1_precsd$b_folder9m < post_samples_rq1_precsd$b_folder18m) # 0.4295
mean(post_samples_rq1_precsd$b_folder18m < post_samples_rq1_precsd$b_folder6m) # 0.99975
mean(post_samples_rq1_precsd$b_folder6m < post_samples_rq1_precsd$b_folderchimps) # 0.98575
mean(post_samples_rq1_precsd$b_folderchimps < post_samples_rq1_precsd$b_folder4m) # 0.61275

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
      levels = c("4m", "6m", "9m", "18m", "adults", "chimps"),
      labels = c("4 Months", "6 Months", "9 Months", "18 Months", "Adults", "Chimpanzees")
      # labels = c("4M", "6M", "9M", "18M", "Adults", "Chimpanzees")
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
    x = "Group",
    y = "Precision (SD)\nin visual degrees"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp1", "img", "rq1_precsd_paperplot.png"), width = 2480/2, height = 3508/4, res = 200)
p_precsd
dev.off()

# RQ1 (Robustness) ----------------------------------------------------------
# Preregistered Model:
# Dependent variables: accuracy, precision RMS, precision SD, and robustness (in separate GLMMs).
# Fixed effect variables of interest: group (4-, 6-, 9-, 18-month-old human infants, human adults, chimpanzees)
# Fixed control variables: stimulus position on screen
# Random intercept: subject id
# Random slopes: maximal random effect structure
# 
# Full model: data quality ~ group + position on screen + random effects
# Reduced model: data quality ~ position on screen + random effects

# Robustness definition (proportion):
# robustness = mean length of consecutively valid samples
# Therefore: 0 < 53466

# Model choice:
# We use a Beta likelihood with a logit link.
# With Beta(link="logit") and 0 + folder, folder coefficients are on the logit-mean scale:
#   mu = inv_logit( b_folder* + ... )
# Note: Beta() requires outcomes strictly in (0, 1). If exact 0 or 1 occur,
# clamp slightly or use zero_one_inflated_beta().

## Define Priors of Full Model ----
mu0 <- qlogis(0.05)
mu0 # -2.944439

priors_rq1_rob <- c(
  
  # Robustness (not preregistered; therefore weakly informative priors)
  # We expect robustness to be around 0.05 on average, but with wide uncertainty.
  # Group-level means (logit-mean scale)
  prior(normal(-2.944439, 0.9), class = "b", coef = "folder4m"),
  prior(normal(-2.944439, 0.9), class = "b", coef = "folder6m"),
  prior(normal(-2.944439, 0.9), class = "b", coef = "folder9m"),
  prior(normal(-2.944439, 0.9), class = "b", coef = "folder18m"),
  prior(normal(-2.944439, 0.9), class = "b", coef = "folderadults"),
  prior(normal(-2.944439, 0.9), class = "b", coef = "folderchimps"),
  
  # Dispersion (phi):
  # phi controls how concentrated the Beta distribution is around mu.
  # Weakly informative prior, avoids extreme concentration by default.
  # Beta precision parameter (higher = less variance around mu)
  prior(exponential(1), class = "phi"),
  
  # Random effects regularization
  prior(exponential(1), class = "sd") # enforces positivity but allows inter-individual heterogeneity
)

## Full Model ----
t0 <- proc.time()

full_rq1_rob <- brm(
  robustness_prop_2 ~ 0 + folder + (1 | group_id),
  data   = df_tot |> filter(!is.na(robustness_prop_2)) |> select(folder, group_id,robustness_prop_2) |> distinct(),
  family = Beta(link = "logit"),
  prior  = priors_rq1_rob,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)
t1 <- proc.time()
proc_time_rq1_rob <- t1 - t0
rm(t0, t1)

## Define Priors of Reduced Model ----
mu0 <- qlogis(0.05)
mu0 # -2.944439

priors_rq1_rob_red <- c(
  
  # Dispersion (phi):
  # phi controls how concentrated the Beta distribution is around mu.
  # Weakly informative prior, avoids extreme concentration by default.
  # Beta precision parameter (higher = less variance around mu)
  prior(exponential(1), class = "phi"),
  
  # Random effects regularization
  prior(exponential(1), class = "sd") # enforces positivity but allows inter-individual heterogeneity
)

## Reduced Model ----
red_rq1_rob <- brm(
  robustness_prop_2 ~ (1 | group_id),
  data   = df_tot |> filter(!is.na(robustness_prop_2)) |> select(folder, group_id,robustness_prop_2) |> distinct(),
  family = Beta(link = "logit"),
  prior  = priors_rq1_rob_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

## Model Comparison ----
loo_full_rob <- loo(full_rq1_rob)
loo_red_rob <- loo(red_rq1_rob)
loo_compare(loo_full_rob, loo_red_rob) # red_rq1_rob  -31.3       5.5 

## Contrasts ----
rob_contr_all <- brms_group_effects_response(
  fit   = full_rq1_rob,
  groups = groups,
  group_prefix = "folder",
  type  = "contrasts",
  ref   = NULL,          # => all pairwise
  link  = "log",
  contrast_scale = "ratio"
)

rob_contr_all |> 
  arrange(desc(ratio_median))

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp1", "img", "rq1_rob_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq1_rob, ndraws = 100) 
#pp_check(full_rq1_rob, type = "hist")
dev.off()

png(here("exp1", "img", "rq1_rob_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq1_rob, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----

# Preparation
folder_order  <- c("4m","6m","9m","18m","adults","chimps")
folder_labels <- c("4m"="4 Months","6m"="6 Months","9m"="9 Months","18m"="18 Months",
                   "adults"="Adults","chimps"="Chimpanzees")

# Create Newdata Grid (Without Position)
nd_rob <- tibble(folder = factor(folder_order, levels = folder_order)) |>
  mutate(folder = factor(folder, levels = levels(full_rq1_rob$data$folder))) |>
  filter(!is.na(folder))

# Create Predictions
ep_rob   <- posterior_epred(full_rq1_rob, newdata = nd_rob, re_formula = NA)
rob_long <- epred_to_long(ep_rob, nd_rob)

# Create Plot
posterior_plot_rq1_rob <- ggplot(
  rob_long,
  aes(x = .epred,
      y = factor(folder, levels = rev(folder_order)))) +
  stat_halfeye(point_interval = "median_qi", .width = c(0, 0.95),
               alpha = 0.65, height = 1.05, adjust = 1.0) +
  scale_y_discrete(labels = folder_labels) +
  labs(x = "Predicted Robustness", y = NULL) +
  theme_bw(base_size = 14)

# Save
png(here::here("exp1", "img", "rq1_rob_posterior.png"), width = 2480, height = 3508/3.5, res = 250)
posterior_plot_rq1_rob
dev.off()

## Posterior Versus Prior Plots ----

# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
png(here("exp1", "img", "rq1_rob_posteriorprior_chimps.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_rob, pars = c("b_folderchimps", "prior_b_folderchimps"), facet_label = "Chimpanzees")
dev.off()

png(here("exp1", "img", "rq1_rob_posteriorprior_4m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_rob, pars = c("b_folder4m", "prior_b_folder4m"), facet_label = "4-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_rob_posteriorprior_6m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_rob, pars = c("b_folder6m", "prior_b_folder6m"), facet_label = "6-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_rob_posteriorprior_9m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_rob, pars = c("b_folder9m", "prior_b_folder9m"), facet_label = "9-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_rob_posteriorprior_18m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_rob, pars = c("b_folder18m", "prior_b_folder18m"), facet_label = "18-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_rob_posteriorprior_adults.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_rob, pars = c("b_folderadults", "prior_b_folderadults"), facet_label = "Adults")
dev.off()

## Descriptives ----
df_tot |> 
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

## Others ----
# Calculate posterior probability that Robustness in A is higher than in B
mean(post_samples_rq1_rob$b_folderadults > post_samples_rq1_rob$b_folder18m) # 1 
# "The posterior probability that adults have a better robustness than 18-month-old infants, is 1."
mean(post_samples_rq1_rob$b_folder18m > post_samples_rq1_rob$b_folder9m) # 0.928%
mean(post_samples_rq1_rob$b_folder9m > post_samples_rq1_rob$b_folder6m) # 0.7595%
mean(post_samples_rq1_rob$b_folder6m > post_samples_rq1_rob$b_folderchimps) # 0.5656%
mean(post_samples_rq1_rob$b_folderchimps > post_samples_rq1_rob$b_folder4m) # 0.67825%

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
      levels = c("4m", "6m", "9m", "18m", "adults", "chimps"),
      labels = c("4 Months", "6 Months", "9 Months", "18 Months", "Adults", "Chimpanzees")
      # labels = c("4M", "6M", "9M", "18M", "Adults", "Chimpanzees")
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
    x = "Group",
    y = "Robustness\nin %"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp1", "img", "rq1_rob_paperplot.png"), width = 2480/2, height = 3508/4, res = 200)
p_robust
dev.off()


nd <- data.frame(folder = c("4m","6m","9m","18m","adults","chimps"))
ep <- posterior_epred(full_rq1_rob, newdata = nd, re_formula = NA)
colnames(ep) <- c("4m","6m","9m","18m","adults","chimps")
pars_order <- c("4m","6m","9m","18m","adults","chimps")
y_labels <- c(
  "4m"     = "4 Months",
  "6m"     = "6 Months",
  "9m"     = "9 Months",
  "18m"    = "18 Months",
  "adults" = "Adults",
  "chimps" = "Chimpanzees"
)

posterior_robust <- mcmc_areas(ep, pars = rev(pars_order), prob = 0.95) +
  scale_y_discrete(labels = y_labels) +
  ggplot2::labs(x = "Robustness\n(Posterior Predictive Mean; Response Scale)",
                y = "Group")
png(here("exp1", "img", "rq1_rob_posteriorplot.png"), width = 2480/2, height = 3508/4, res = 250)
posterior_robust
dev.off()


# RQ2 (Accuracy) ----------------------------------------------------------
# RQ2: (How) does eye-tracking data quality change over time, 
# with time being defined as trials (humans, chimpanzees) and/or sessions (chimpanzees)?

# Dependent variables: accuracy, precision RMS, precision SD, and robustness (in separate GLMMs).
# Fixed effect variables of interest: time (first trial/ session, …, last trial/ session), group (4-, 6-, 9-, 18-month-old human infants, human adults, chimpanzees)
# Fixed control variables: position on screen
# Random intercept: subject id
# Random slopes: maximal random effects structure
# 
# Full model: data quality ~ group * time + position on screen + random effects
# Reduced model: data quality ~ group + position on screen + random effects

# Priors:
# Accuracy (fixed effect, group): see RQ1 above.
# Accuracy (interaction): Normally distributed prior with M = 0 and SD = 0.64.

## Define Priors ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
priors_rq2_acc_hum <- c(
  # Group parameters (log-mean scale)
  prior(normal(0.50, 0.48), class = "b", coef = "folder4m"),
  prior(normal(0.50, 0.48), class = "b", coef = "folder6m"),
  prior(normal(0.24, 0.09), class = "b", coef = "folder9m"),
  prior(normal(0.00, 0.09), class = "b", coef = "folder18m"),
  prior(normal(-0.41, 0.08), class = "b", coef = "folderadults"),
  
  # Gamma shape (estimate = 2.39 and est. error = 0.06 on a natural scale,
  # which translates to meanlog = 0.87097834 and sdlog = 0.02510065 on log scale,
  # m <- 2.39
  # s <- 0.06
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02510065
  # meanlog <- log(m) - sigma2/2         # 0.87097834
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(0.87097834, 2*0.02510065), class = "shape"),
  
  # Interaction: folder × time_3
  # Prior for group-specific linear time slopes (per trial) on the log-mean scale.
  # SD = 0.002 implies that, over ~79 trials, most prior mass corresponds to a modest 
  # total change (≈ −26% to +36%) in expected accuracy from the first to the last trial, 
  # reflecting mixed evidence (decline vs no change) without allowing implausibly large
  # session-wide shifts.
  prior(normal(0, 0.002), class = "b", coef = "folder4m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folder6m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folder9m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folder18m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folderadults:time_3"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -0.41 - 0.24 = -0.65, so half of that is 0.325)
  prior(normal(0, 0.325), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.325), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.325), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.325), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.325), class = "b", coef = "positiontop"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

priors_rq2_acc_chi <- c(
  # Gamma shape (estimate = 2.39 and est. error = 0.06 on a natural scale,
  # which translates to meanlog = 0.87097834 and sdlog = 0.02510065 on log scale,
  # m <- 2.39
  # s <- 0.06
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02510065
  # meanlog <- log(m) - sigma2/2         # 0.87097834
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(0.87097834, 2*0.02510065), class = "shape"),
  
  # Main effect time
  # Linear time effect per testing day on the log-mean scale.
  # We re-center time_3 so that 0 = first testing day. The coefficient b_time_3 is therefore
  # the per-day multiplicative change in expected accuracy: mu_day = mu_day1 * exp(b_time_3 * day).
  # SD = 0.026 implies that over ~7 days (Δt = 6) most prior mass corresponds to a modest
  # total change (≈ −26% to +36%) from first to last day:
  # exp(±1.96 * 0.026 * 6) ≈ [0.74, 1.36]. This reflects “no strong prediction” while ruling out
  # implausibly large session-wide shifts.
  prior(normal(0, 0.026), class = "b", coef = "time_3"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -0.41 - 0.24 = -0.65, so half of that is 0.325)
  prior(normal(0, 0.325), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.325), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.325), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.325), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.325), class = "b", coef = "positiontop"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

priors_rq2_acc_chi_2 <- c(
  
  # Gamma shape (estimate = 2.39 and est. error = 0.06 on a natural scale,
  # which translates to meanlog = 0.87097834 and sdlog = 0.02510065 on log scale,
  # m <- 2.39
  # s <- 0.06
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02510065
  # meanlog <- log(m) - sigma2/2         # 0.87097834
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(0.87097834, 2*0.02510065), class = "shape"),
  
  # Main effect time (Trials 1-41, recentered so 0 = first trial)
  # Total change over 40 trials: exp(±1.96 * 0.0039 * 40) ≈ [0.74, 1.36]
  prior(normal(0, 0.0039), class = "b", coef = "time_2"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -0.41 - 0.24 = -0.65, so half of that is 0.325)
  prior(normal(0, 0.325), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.325), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.325), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.325), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.325), class = "b", coef = "positiontop"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

## Full Model ----
full_rq2_acc_hum <- brm(
  acc_visd ~ 0 + folder + folder:time_3 + position +
    (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder != "chimps") |> mutate(time_3 = time_3 - 1), # re-center time_3 to make the intercept more interpretable
  family = hurdle_gamma(link="log"),
  prior  = priors_rq2_acc_hum,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

full_rq2_acc_chi <- brm(
  acc_visd ~ time_3 + position + (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_3 = time_3 - 1),
  family = hurdle_gamma(link="log"),
  prior  = priors_rq2_acc_chi,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

full_rq2_acc_chi_2 <- brm(
  acc_visd ~ time_2 + position + (1 + time_2 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_2 = time_2 - 1),
  family = hurdle_gamma(link="log"),
  prior  = priors_rq2_acc_chi_2,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

## Define Priors of Reduced Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
priors_rq2_acc_hum_red <- c(
  # Group parameters (log-mean scale)
  prior(normal(0.50, 0.48), class = "b", coef = "folder4m"),
  prior(normal(0.50, 0.48), class = "b", coef = "folder6m"),
  prior(normal(0.24, 0.09), class = "b", coef = "folder9m"),
  prior(normal(0.00, 0.09), class = "b", coef = "folder18m"),
  prior(normal(-0.41, 0.08), class = "b", coef = "folderadults"),
  
  # Gamma shape (estimate = 2.39 and est. error = 0.06 on a natural scale,
  # which translates to meanlog = 0.87097834 and sdlog = 0.02510065 on log scale,
  # m <- 2.39
  # s <- 0.06
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02510065
  # meanlog <- log(m) - sigma2/2         # 0.87097834
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(0.87097834, 2*0.02510065), class = "shape"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -0.41 - 0.24 = -0.65, so half of that is 0.325)
  prior(normal(0, 0.325), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.325), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.325), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.325), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.325), class = "b", coef = "positiontop"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

priors_rq2_acc_chi_red <- c(
  # Gamma shape (estimate = 2.39 and est. error = 0.06 on a natural scale,
  # which translates to meanlog = 0.87097834 and sdlog = 0.02510065 on log scale,
  # m <- 2.39
  # s <- 0.06
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02510065
  # meanlog <- log(m) - sigma2/2         # 0.87097834
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(0.87097834, 2*0.02510065), class = "shape"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -0.41 - 0.24 = -0.65, so half of that is 0.325)
  prior(normal(0, 0.325), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.325), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.325), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.325), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.325), class = "b", coef = "positiontop"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

priors_rq2_acc_chi_2_red <- c(
  # Gamma shape (estimate = 2.39 and est. error = 0.06 on a natural scale,
  # which translates to meanlog = 0.87097834 and sdlog = 0.02510065 on log scale,
  # m <- 2.39
  # s <- 0.06
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02510065
  # meanlog <- log(m) - sigma2/2         # 0.87097834
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(0.87097834, 2*0.02510065), class = "shape"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -0.41 - 0.24 = -0.65, so half of that is 0.325)
  prior(normal(0, 0.325), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.325), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.325), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.325), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.325), class = "b", coef = "positiontop"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.325), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

## Reduced Model ----
red_rq2_acc_hum <- brm(
  acc_visd ~ 0 + folder + position +
    (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder != "chimps") |> mutate(time_3 = time_3 - 1), # re-center time_3 to make the intercept more interpretable
  family = hurdle_gamma(link="log"),
  prior  = priors_rq2_acc_hum_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

red_rq2_acc_chi <- brm(
  acc_visd ~ position + (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_3 = time_3 - 1),
  family = hurdle_gamma(link="log"),
  prior  = priors_rq2_acc_chi_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

red_rq2_acc_chi_2 <- brm(
  acc_visd ~ position + (1 + time_2 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_2 = time_2 - 1),
  family = hurdle_gamma(link="log"),
  prior  = priors_rq2_acc_chi_2_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

## Model Comparison ----
# Humans
loo_full_acc_rq2_hum <- loo(full_rq2_acc_hum)
loo_red_acc_rq2_hum <- loo(red_rq2_acc_hum)
loo_compare(loo_full_acc_rq2_hum, loo_red_acc_rq2_hum)  # full_rq2_acc_hum -1.4       1.8

# Chimps I
loo_full_acc_rq2_chi <- loo(full_rq2_acc_chi)
loo_red_acc_rq2_chi <- loo(red_rq2_acc_chi)
loo_compare(loo_full_acc_rq2_chi, loo_red_acc_rq2_chi)  # full_rq2_acc_chi -0.1       0.7 

# Chimps II
loo_full_acc_rq2_chi_2 <- loo(full_rq2_acc_chi_2)
loo_red_acc_rq2_chi_2 <- loo(red_rq2_acc_chi_2)
loo_compare(loo_full_acc_rq2_chi_2, loo_red_acc_rq2_chi_2)  # full_rq2_acc_chi_2 -0.6       0.2

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"

png(here("exp1", "img", "rq2_acc_hum_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_acc_hum, ndraws = 100)
dev.off()

png(here("exp1", "img", "rq2_acc_chi1_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_acc_chi, ndraws = 100)
dev.off()

png(here("exp1", "img", "rq2_acc_chi2_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_acc_chi_2, ndraws = 100)
dev.off()

png(here("exp1", "img", "rq2_acc_hum_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_acc_hum, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----
## Preparation
group_order <- c(
  "4m", "6m", "9m", "18m", "adults",
  "chimp_days", "chimp_trials"
)

group_labels <- c(
  "4m"           = "4 Months",
  "6m"           = "6 Months",
  "9m"           = "9 Months",
  "18m"          = "18 Months",
  "adults"       = "Adults",
  "chimp_days"   = "Chimpanzees (days)",
  "chimp_trials" = "Chimpanzees (trials)"
)

## Humans
post_hum <- as_draws_df(full_rq2_acc_hum)

slope_hum <- post_hum |>
  transmute(
    `4m`     = `b_folder4m:time_3`,
    `6m`     = `b_folder6m:time_3`,
    `9m`     = `b_folder9m:time_3`,
    `18m`    = `b_folder18m:time_3`,
    `adults` = `b_folderadults:time_3`
  ) |>
  mutate(.draw = row_number()) |>
  pivot_longer(
    cols = -.draw,
    names_to = "group",
    values_to = "slope"
  )

## Chimpanzees Days
post_ch_days <- as_draws_df(full_rq2_acc_chi)

slope_ch_days <- post_ch_days |>
  transmute(
    group = "chimp_days",
    slope = b_time_3
  ) |>
  mutate(.draw = row_number())

## Chimpanzees Trials
post_ch_trials <- as_draws_df(full_rq2_acc_chi_2)

slope_ch_trials <- post_ch_trials |>
  transmute(
    group = "chimp_trials",
    slope = b_time_2
  ) |>
  mutate(.draw = row_number())

## Combine
slope_all <- bind_rows(
  slope_hum |> select(.draw, group, slope),
  slope_ch_days |> select(.draw, group, slope),
  slope_ch_trials |> select(.draw, group, slope)
) |>
  mutate(group = factor(group, levels = group_order))

## Plot
posterior_plot_rq2_acc <- ggplot(
  slope_all,
  aes(
    x = slope,
    y = factor(group, levels = rev(group_order))
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  stat_halfeye(
    point_interval = "median_qi",
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05,
    adjust = 1.0,
    fill = "grey70"
  ) +
  scale_y_discrete(labels = group_labels) +
  scale_x_continuous(
    breaks = seq(-0.03, 0.03, by = 0.01),
    limits = c(-0.03, 0.03)
  ) +
  labs(
    x = "Time",
    y = NULL
  ) +
  theme_bw(base_size = 14)

png(here("exp1", "img", "rq2_acc_posterior.png"), width = 2480, height = 3508/4.5, res = 190)
posterior_plot_rq2_acc
dev.off()

## Posterior Versus Prior Plot ----
# Humans 
png(here("exp1", "img", "rq2_acc_posteriorprior_4m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq2_acc_hum, pars = c("b_folder4m:time_3", "prior_b_folder4m:time_3"), facet_label = "4 Months")
dev.off()

png(here("exp1", "img", "rq2_acc_posteriorprior_6m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq2_acc_hum, pars = c("b_folder6m:time_3", "prior_b_folder6m:time_3"), facet_label = "6 Months")
dev.off()

png(here("exp1", "img", "rq2_acc_posteriorprior_9m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq2_acc_hum, pars = c("b_folder9m:time_3", "prior_b_folder9m:time_3"), facet_label = "9 Months")
dev.off()

png(here("exp1", "img", "rq2_acc_posteriorprior_18m.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq2_acc_hum, pars = c("b_folder18m:time_3", "prior_b_folder18m:time_3"), facet_label = "18 Months")
dev.off()

png(here("exp1", "img", "rq2_acc_posteriorprior_adults.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq2_acc_hum, pars = c("b_folderadults:time_3", "prior_b_folderadults:time_3"), facet_label = "Adults")
dev.off()

# Chimps
png(here("exp1", "img", "rq2_acc_posteriorprior_chimps_days.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_acc_chi, pars = c("b_time_3", "prior_b_time_3"), facet_label = "Chimpanzees (Time as Testing Days)")
dev.off()

png(here("exp1", "img", "rq2_acc_posteriorprior_chimps_trials.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_acc_chi_2, pars = c("b_time_2", "prior_b_time_2"), facet_label = "Chimpanzees (Time as Trials Within Testing Days)")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(folder, group_id, time_3) |> # or time_1 or time_2 or time_3
  summarize(acc_visd = mean(acc_visd, na.rm = T)) |> 
  group_by(folder, time_3) |> # or time_1 or time_2 or time_3
  summarize(mean_acc_visd = mean(acc_visd, na.rm = T),
            sd_acc_visd = sd(acc_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(folder, time_3) # or time_1 or time_2 or time_3

## Paper Plot ----
# Plot showing data quality across time in all tested groups
plot_rq2(df = df_tot, png_name = "rq2_acc_session_all.png", out_dir = here::here("exp1", "img"), width = 2480*1.5, height = 3508, res = 300,
                         group_var = "folder", x_var   = "time_1", y_var = "acc_visd",
                         xmax_chimps = 8, filter_chimps_time_gt = TRUE, ymin_humans = 0, ymax_humans = 2, ymin_chimps = 3, ymax_chimps = 5,
                         ytitle = "Accuracy\nin visual degrees", x_label = "Time\n(trials in humans; sessions in apes)")

plot_rq2(df = df_tot, png_name = "rq2_acc_trial_all.png", out_dir = here::here("exp1", "img"), width = 2480*1.5, height = 3508, res = 300,
                         group_var = "folder", x_var   = "time_2", y_var = "acc_visd",
                         xmax_chimps = 11, filter_chimps_time_gt = TRUE, ymin_humans = 0, ymax_humans = 2, ymin_chimps = 3, ymax_chimps = 5,
                         ytitle = "Accuracy\nin visual degrees", x_label = "Time\n(trials in humans; trials within sessions in apes)")

plot_rq2(df = df_tot, png_name = "rq2_acc_day_all.png", out_dir = here::here("exp1", "img"), width = 2480*1.5, height = 3508, res = 300,
                         group_var = "folder", x_var   = "time_3", y_var = "acc_visd",
                         xmax_chimps = 7, filter_chimps_time_gt = TRUE, ymin_humans = 0, ymax_humans = 2, ymin_chimps = 3, ymax_chimps = 5,
                         ytitle = "Accuracy\nin visual degrees", x_label = "Time\n(trials in humans; days in apes)")

## Supp Plots Per Chimp ----
df_plot_rq2_acc <- df_tot |> 
  filter(folder == "chimps") |> 
  group_by(group_id, time_3) |> 
  summarize(acc_visd = mean(acc_visd, na.rm = TRUE), .groups = "drop") |> 
  drop_na(time_3) |> 
  mutate(group_id_pretty = str_to_title(group_id))

y_range <- range(df_plot_rq2_acc$acc_visd, na.rm = TRUE)
ncol_plot <- 3
nrow_plot <- 3
plots_per_page <- ncol_plot * nrow_plot
n_pages <- ceiling(n_distinct(df_plot_rq2_acc$group_id_pretty) / plots_per_page)

for (page in 1:n_pages) {
  
  p_rq2_acc <- ggplot(df_plot_rq2_acc, aes(x = time_3, y = acc_visd, group = group_id_pretty)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 2) +
    facet_wrap_paginate(
      ~ group_id_pretty,
      ncol = ncol_plot,
      nrow = nrow_plot,
      page = page
    ) +
    scale_y_continuous(limits = y_range) +
    scale_x_continuous(breaks = sort(unique(df_plot_rq2_acc$time_3))) +
    labs(
      x = "Time",
      y = "Accuracy"
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    filename = file.path(here("exp1", "img", paste0("individual_acc_visd_plots_page_", page, ".png"))
    ),
    plot = p_rq2_acc,
    width = 11,
    height = 8.5,
    dpi = 300
  )
}

# RQ2 (Precision RMS) -----------------------------------------------------
# RQ2: (How) does eye-tracking data quality change over time,
# with time being defined as trials (humans) and testing days (chimpanzees)?

## Define Priors ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
priors_rq2_precrms_hum <- c(
  # Group parameters (log-mean scale)
  prior(normal(-0.88, 0.32), class = "b", coef = "folder4m"),
  prior(normal(-0.88, 0.32), class = "b", coef = "folder6m"),
  prior(normal(-1.64, 0.08), class = "b", coef = "folder9m"),
  prior(normal(-1.82, 0.07), class = "b", coef = "folder18m"),
  prior(normal(-2.03, 0.07), class = "b", coef = "folderadults"),

  # Gamma shape
  # shape estimate = 7.42 (SE 0.19) -> meanlog ~ 2.004, sdlog ~ 0.0256; doubled sd for width
  prior(lognormal(2.004, 2*0.0256), class = "shape"),

  # Interaction: folder × time_3 (group-specific linear time slopes per trial)
  # Prior for group-specific linear time slopes (per trial) on the log-mean scale.
  # time_3 is unscaled but re-centered (0 = first trial). SD = 0.002 implies that over ~79 trials
  # (Δt ≈ 78) most prior mass corresponds to a modest total change (≈ −26% to +36%) from first
  # to last trial: exp(±1.96 * 0.002 * 78) ≈ [0.74, 1.36].
  prior(normal(0, 0.002), class = "b", coef = "folder4m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folder6m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folder9m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folder18m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folderadults:time_3"),
  
  # Position priors
  # Position should not be bigger than half of the difference between adults and 9ms:
  # -2.03 - (-1.64) = -0.39, half = 0.195
  prior(normal(0, 0.195), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.195), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.195), class = "b", coef = "positionbottom"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

priors_rq2_precrms_chi <- c(
  # Gamma shape
  prior(lognormal(2.004, 2*0.0256), class = "shape"),
  
  # Main effect time (per testing day)
  # Linear time effect per testing day on the log-mean scale.
  # time_3 is re-centered so that 0 = first testing day. SD = 0.026 implies that over ~7 days
  # (Δt = 6) most prior mass corresponds to a modest total change (≈ −26% to +36%) from first
  # to last day: exp(±1.96 * 0.026 * 6) ≈ [0.74, 1.36].
  prior(normal(0, 0.026), class = "b", coef = "time_3"),
  
  # Position priors
  prior(normal(0, 0.195), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.195), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.195), class = "b", coef = "positionbottom"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

priors_rq2_precrms_chi_2 <- c(
  # Gamma shape
  prior(lognormal(2.004, 2*0.0256), class = "shape"),
  
  # Position priors
  prior(normal(0, 0.195), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.195), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.195), class = "b", coef = "positionbottom"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor"),
  
  # Main effect time (Trials 1-41, recentered so 0 = first trial)
  # Total change over 40 trials: exp(±1.96 * 0.0039 * 40) ≈ [0.74, 1.36]
  prior(normal(0, 0.0039), class = "b", coef = "time_2")
)

## Full Models ----
full_rq2_precrms_hum <- brm(
  precrms ~ 0 + folder + folder:time_3 + position +
    (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder != "chimps") |> mutate(time_3 = time_3 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precrms_hum,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

full_rq2_precrms_chi <- brm(
  precrms ~ time_3 + position + (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_3 = time_3 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precrms_chi,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

full_rq2_precrms_chi_2 <- brm(
  precrms ~ time_2 + position + (1 + time_2 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_2 = time_2 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precrms_chi_2,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

## Define Priors of Reduced Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
priors_rq2_precrms_hum_red <- c(
  # Group parameters (log-mean scale)
  prior(normal(-0.88, 0.32), class = "b", coef = "folder4m"),
  prior(normal(-0.88, 0.32), class = "b", coef = "folder6m"),
  prior(normal(-1.64, 0.08), class = "b", coef = "folder9m"),
  prior(normal(-1.82, 0.07), class = "b", coef = "folder18m"),
  prior(normal(-2.03, 0.07), class = "b", coef = "folderadults"),
  
  # Gamma shape
  # shape estimate = 7.42 (SE 0.19) -> meanlog ~ 2.004, sdlog ~ 0.0256; doubled sd for width
  prior(lognormal(2.004, 2*0.0256), class = "shape"),
  
  # Position priors
  # Position should not be bigger than half of the difference between adults and 9ms:
  # -2.03 - (-1.64) = -0.39, half = 0.195
  prior(normal(0, 0.195), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.195), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.195), class = "b", coef = "positionbottom"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

priors_rq2_precrms_chi_red <- c(
  # Gamma shape
  prior(lognormal(2.004, 2*0.0256), class = "shape"),
  
  # Position priors
  prior(normal(0, 0.195), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.195), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.195), class = "b", coef = "positionbottom"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

priors_rq2_precrms_chi_2_red <- c(
  # Gamma shape
  prior(lognormal(2.004, 2*0.0256), class = "shape"),
  
  # Position priors
  prior(normal(0, 0.195), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.195), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.195), class = "b", coef = "positionbottom"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.195), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Reduced Models ----
red_rq2_precrms_hum <- brm(
  precrms ~ 0 + folder + position +
    (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder != "chimps") |> mutate(time_3 = time_3 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precrms_hum_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

red_rq2_precrms_chi <- brm(
  precrms ~ position + (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_3 = time_3 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precrms_chi_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

red_rq2_precrms_chi_2 <- brm(
  precrms ~ position + (1 + time_2 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_2 = time_2 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precrms_chi_2_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

## Model Comparison ----
# Humans
loo_full_precrms_rq2_hum <- loo(full_rq2_precrms_hum)
loo_red_precrms_rq2_hum <- loo(red_rq2_precrms_hum)
loo_compare(loo_full_precrms_rq2_hum, loo_red_precrms_rq2_hum)

# Chimps I
loo_full_precrms_rq2_chi <- loo(full_rq2_precrms_chi)
loo_red_precrms_rq2_chi <- loo(red_rq2_precrms_chi)
loo_compare(loo_full_precrms_rq2_chi, loo_red_precrms_rq2_chi)

# Chimps II
loo_full_precrms_rq2_chi_2 <- loo(full_rq2_precrms_chi_2)
loo_red_precrms_rq2_chi_2 <- loo(red_rq2_precrms_chi_2)
loo_compare(loo_full_precrms_rq2_chi_2, loo_red_precrms_rq2_chi_2)

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp1", "img", "rq2_precrms_hum_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_precrms_hum, ndraws = 100)
dev.off()

png(here("exp1", "img", "rq2_precrms_chi1_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_precrms_chi, ndraws = 100)
dev.off()

png(here("exp1", "img", "rq2_precrms_chi2_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_precrms_chi_2, ndraws = 100)
dev.off()

png(here("exp1", "img", "rq2_precrms_hum_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_precrms_hum, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----
## Preparation
group_order <- c(
  "4m", "6m", "9m", "18m", "adults",
  "chimp_days", "chimp_trials"
)

group_labels <- c(
  "4m"           = "4 Months",
  "6m"           = "6 Months",
  "9m"           = "9 Months",
  "18m"          = "18 Months",
  "adults"       = "Adults",
  "chimp_days"   = "Chimpanzees (days)",
  "chimp_trials" = "Chimpanzees (trials)"
)

## Humans
post_hum <- as_draws_df(full_rq2_precrms_hum)

slope_hum <- post_hum |>
  transmute(
    `4m`     = `b_folder4m:time_3`,
    `6m`     = `b_folder6m:time_3`,
    `9m`     = `b_folder9m:time_3`,
    `18m`    = `b_folder18m:time_3`,
    `adults` = `b_folderadults:time_3`
  ) |>
  mutate(.draw = row_number()) |>
  pivot_longer(
    cols = -.draw,
    names_to = "group",
    values_to = "slope"
  )

## Chimpanzees Days
post_ch_days <- as_draws_df(full_rq2_precrms_chi)

slope_ch_days <- post_ch_days |>
  transmute(
    group = "chimp_days",
    slope = b_time_3
  ) |>
  mutate(.draw = row_number())

## Chimpanzees Trials
post_ch_trials <- as_draws_df(full_rq2_precrms_chi_2)

slope_ch_trials <- post_ch_trials |>
  transmute(
    group = "chimp_trials",
    slope = b_time_2
  ) |>
  mutate(.draw = row_number())

## Combine
slope_all <- bind_rows(
  slope_hum |> select(.draw, group, slope),
  slope_ch_days |> select(.draw, group, slope),
  slope_ch_trials |> select(.draw, group, slope)
) |>
  mutate(group = factor(group, levels = group_order))

## Plot
posterior_plot_rq2_precrms <- ggplot(
  slope_all,
  aes(
    x = slope,
    y = factor(group, levels = rev(group_order))
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  stat_halfeye(
    point_interval = "median_qi",
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05,
    adjust = 1.0,
    fill = "grey70"
  ) +
  scale_y_discrete(labels = group_labels) +
  scale_x_continuous(
    breaks = seq(-0.03, 0.03, by = 0.01),
    limits = c(-0.03, 0.03)
  ) +
  labs(
    x = "Time",
    y = NULL
  ) +
  theme_bw(base_size = 14)

png(here("exp1", "img", "rq2_precrms_posterior.png"), width = 2480, height = 3508/4.5, res = 190)
posterior_plot_rq2_precrms
dev.off()

## Posterior Versus Prior Plot ----
# Humans 
png(here("exp1", "img", "rq2_precrms_posteriorprior_4m.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precrms_hum, pars = c("b_folder4m:time_3", "prior_b_folder4m:time_3"), facet_label = "4 Months")
dev.off()

png(here("exp1", "img", "rq2_precrms_posteriorprior_6m.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precrms_hum, pars = c("b_folder6m:time_3", "prior_b_folder6m:time_3"), facet_label = "6 Months")
dev.off()

png(here("exp1", "img", "rq2_precrms_posteriorprior_9m.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precrms_hum, pars = c("b_folder9m:time_3", "prior_b_folder9m:time_3"), facet_label = "9 Months")
dev.off()

png(here("exp1", "img", "rq2_precrms_posteriorprior_18m.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precrms_hum, pars = c("b_folder18m:time_3", "prior_b_folder18m:time_3"), facet_label = "18 Months")
dev.off()

png(here("exp1", "img", "rq2_precrms_posteriorprior_adults.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precrms_hum, pars = c("b_folderadults:time_3", "prior_b_folderadults:time_3"), facet_label = "Adults")
dev.off()

# Chimps
png(here("exp1", "img", "rq2_precrms_posteriorprior_chimps_days.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precrms_chi, pars = c("b_time_3", "prior_b_time_3"), facet_label = "Chimpanzees (Time as Testing Days)")
dev.off()

png(here("exp1", "img", "rq2_precrms_posteriorprior_chimps_trials.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precrms_chi_2, pars = c("b_time_2", "prior_b_time_2"), facet_label = "Chimpanzees (Time as Trials Within Testing Days)")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(folder, group_id, time_3) |> # or time_1 or time_2 or time_3
  summarize(precrms_visd = mean(precrms_visd, na.rm = T)) |> 
  group_by(folder, time_3) |> # or time_1 or time_2 or time_3
  summarize(mean_precrms_visd = mean(precrms_visd, na.rm = T),
            sd_precrms_visd = sd(precrms_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(folder, time_3) # or time_1 or time_2 or time_3

## Paper Plot ----
plot_rq2(df = df_tot, png_name = "rq2_precrms_session_all.png", out_dir = here::here("exp1", "img"), width = 2480*1.5, height = 3508, res = 300,
                         group_var = "folder", x_var   = "time_1", y_var = "precrms_visd",
                         xmax_chimps = 8, filter_chimps_time_gt = TRUE, ymin_humans = 0, ymax_humans = 2, ymin_chimps = 0, ymax_chimps = 2,
                         ytitle = "Precision (RMS)\nin visual degrees", x_label = "Time\n(trials in humans; sessions in apes)")

plot_rq2(df = df_tot, png_name = "rq2_precrms_trial_all.png", out_dir = here::here("exp1", "img"), width = 2480*1.5, height = 3508, res = 300,
                         group_var = "folder", x_var   = "time_2", y_var = "precrms_visd",
                         xmax_chimps = 11, filter_chimps_time_gt = TRUE, ymin_humans = 0, ymax_humans = 2, ymin_chimps = 0, ymax_chimps = 2,
                         ytitle = "Precision (RMS)\nin visual degrees", x_label = "Time\n(trials in humans; trials within sessions in apes)")

plot_rq2(df = df_tot, png_name = "rq2_precrms_day_all.png", out_dir = here::here("exp1", "img"), width = 2480*1.5, height = 3508, res = 300,
                         group_var = "folder", x_var   = "time_3", y_var = "precrms_visd",
                         xmax_chimps = 7, filter_chimps_time_gt = TRUE, ymin_humans = 0, ymax_humans = 2, ymin_chimps = 0, ymax_chimps = 2,
                         ytitle = "Precision (RMS)\nin visual degrees", x_label = "Time\n(trials in humans; days in apes)")

## Supp Plots Per Chimp ----
df_plot_rq2_precrms <- df_tot |> 
  filter(folder == "chimps") |> 
  group_by(group_id, time_3) |> 
  summarize(precrms_visd = mean(precrms_visd, na.rm = TRUE), .groups = "drop") |> 
  drop_na(time_3) |> 
  mutate(group_id_pretty = str_to_title(group_id))

y_range <- range(df_plot_rq2_precrms$precrms_visd, na.rm = TRUE)
ncol_plot <- 3
nrow_plot <- 3
plots_per_page <- ncol_plot * nrow_plot
n_pages <- ceiling(n_distinct(df_plot_rq2_precrms$group_id_pretty) / plots_per_page)

for (page in 1:n_pages) {
  
  p_rq2_precrms <- ggplot(df_plot_rq2_precrms, aes(x = time_3, y = precrms_visd, group = group_id_pretty)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 2) +
    facet_wrap_paginate(
      ~ group_id_pretty,
      ncol = ncol_plot,
      nrow = nrow_plot,
      page = page
    ) +
    scale_y_continuous(limits = y_range) +
    scale_x_continuous(breaks = sort(unique(df_plot_rq2_precrms$time_3))) +
    labs(
      x = "Time",
      y = "Precision (RMS)"
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    filename = file.path(here("exp1", "img", paste0("individual_precrms_visd_plots_page_", page, ".png"))
    ),
    plot = p_rq2_precrms,
    width = 11,
    height = 8.5,
    dpi = 300
  )
}

# RQ2 (Precision SD) -----------------------------------------------------
# RQ2: (How) does eye-tracking data quality change over time,
# with time being defined as trials (humans) and testing days (chimpanzees)?

## Define Priors ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
priors_rq2_precsd_hum <- c(
  # Group parameters (log-mean scale)
  prior(normal(-1.03, 0.28), class = "b", coef = "folder4m"),
  prior(normal(-1.03, 0.28), class = "b", coef = "folder6m"),
  prior(normal(-1.61, 0.06), class = "b", coef = "folder9m"),
  prior(normal(-1.68, 0.06), class = "b", coef = "folder18m"),
  prior(normal(-1.87, 0.05), class = "b", coef = "folderadults"),

  # Gamma shape (estimate = 6.16 and est. error = 0.15 on a natural scale)
  # m <- 6.16
  # s <- 0.15
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02434704
  # meanlog <- log(m) - sigma2/2         # 1.81778039
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(1.81778039, 2*0.02434704), class = "shape"),
  
  # Interaction: folder × time_3 (group-specific linear time slopes per trial)
  # Prior for group-specific linear time slopes (per trial) on the log-mean scale.
  # time_3 is unscaled but re-centered (0 = first trial). SD = 0.002 implies that over ~79 trials
  # (Δt ≈ 78) most prior mass corresponds to a modest total change (≈ −26% to +36%) from first
  # to last trial: exp(±1.96 * 0.002 * 78) ≈ [0.74, 1.36].
  prior(normal(0, 0.002), class = "b", coef = "folder4m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folder6m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folder9m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folder18m:time_3"),
  prior(normal(0, 0.002), class = "b", coef = "folderadults:time_3"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -1.87 - (-1.61) = 0.26, so half of it is 0.13)
  prior(normal(0, 0.13), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.13), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.13), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.13), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.13), class = "b", coef = "positiontop"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

priors_rq2_precsd_chi <- c(
  # Gamma shape (estimate = 6.16 and est. error = 0.15 on a natural scale)
  # m <- 6.16
  # s <- 0.15
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02434704
  # meanlog <- log(m) - sigma2/2         # 1.81778039
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(1.81778039, 2*0.02434704), class = "shape"),
  
  # Main effect time (per testing day)
  # Linear time effect per testing day on the log-mean scale.
  # time_3 is re-centered so that 0 = first testing day. SD = 0.026 implies that over ~7 days
  # (Δt = 6) most prior mass corresponds to a modest total change (≈ −26% to +36%) from first
  # to last day: exp(±1.96 * 0.026 * 6) ≈ [0.74, 1.36].
  prior(normal(0, 0.026), class = "b", coef = "time_3"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -1.87 - (-1.61) = 0.26, so half of it is 0.13)
  prior(normal(0, 0.13), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.13), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.13), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.13), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.13), class = "b", coef = "positiontop"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

priors_rq2_precsd_chi_2 <- c(
  # Gamma shape (estimate = 6.16 and est. error = 0.15 on a natural scale)
  # m <- 6.16
  # s <- 0.15
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02434704
  # meanlog <- log(m) - sigma2/2         # 1.81778039
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(1.81778039, 2*0.02434704), class = "shape"),
  
  # Main effect time (Trials 1-41, recentered so 0 = first trial)
  # Total change over 40 trials: exp(±1.96 * 0.0039 * 40) ≈ [0.74, 1.36]
  prior(normal(0, 0.0039), class = "b", coef = "time_2"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -1.87 - (-1.61) = 0.26, so half of it is 0.13)
  prior(normal(0, 0.13), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.13), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.13), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.13), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.13), class = "b", coef = "positiontop"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Full Models ----
full_rq2_precsd_hum <- brm(
  precsd ~ 0 + folder + folder:time_3 + position +
    (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder != "chimps") |> mutate(time_3 = time_3 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precsd_hum,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

full_rq2_precsd_chi <- brm(
  precsd ~ time_3 + position + (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_3 = time_3 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precsd_chi,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

full_rq2_precsd_chi_2 <- brm(
  precsd ~ time_2 + position + (1 + time_2 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_2 = time_2 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precsd_chi_2,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

## Define Priors of Reduced Model ----
# With Gamma(link="log"), coefficients are on the log-mean scale.
priors_rq2_precsd_hum_red <- c(
  # Group parameters (log-mean scale)
  prior(normal(-1.03, 0.28), class = "b", coef = "folder4m"),
  prior(normal(-1.03, 0.28), class = "b", coef = "folder6m"),
  prior(normal(-1.61, 0.06), class = "b", coef = "folder9m"),
  prior(normal(-1.68, 0.06), class = "b", coef = "folder18m"),
  prior(normal(-1.87, 0.05), class = "b", coef = "folderadults"),
  
  # Gamma shape (estimate = 6.16 and est. error = 0.15 on a natural scale)
  # m <- 6.16
  # s <- 0.15
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02434704
  # meanlog <- log(m) - sigma2/2         # 1.81778039
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(1.81778039, 2*0.02434704), class = "shape"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -1.87 - (-1.61) = 0.26, so half of it is 0.13)
  prior(normal(0, 0.13), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.13), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.13), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.13), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.13), class = "b", coef = "positiontop"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

priors_rq2_precsd_chi_red <- c(
  # Gamma shape (estimate = 6.16 and est. error = 0.15 on a natural scale)
  # m <- 6.16
  # s <- 0.15
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02434704
  # meanlog <- log(m) - sigma2/2         # 1.81778039
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(1.81778039, 2*0.02434704), class = "shape"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -1.87 - (-1.61) = 0.26, so half of it is 0.13)
  prior(normal(0, 0.13), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.13), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.13), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.13), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.13), class = "b", coef = "positiontop"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

priors_rq2_precsd_chi_2_red <- c(
  # Gamma shape (estimate = 6.16 and est. error = 0.15 on a natural scale)
  # m <- 6.16
  # s <- 0.15
  # sigma2  <- log(1 + (s^2 / m^2))
  # sdlog   <- sqrt(sigma2)              # 0.02434704
  # meanlog <- log(m) - sigma2/2         # 1.81778039
  # c(meanlog = meanlog, sdlog = sdlog)
  # double the sd to make it wider
  prior(lognormal(1.81778039, 2*0.02434704), class = "shape"),
  
  # Position (should not be bigger than half of the difference between adults and 9ms,
  # that is, -1.87 - (-1.61) = 0.26, so half of it is 0.13)
  prior(normal(0, 0.13), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.13), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.13), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.13), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.13), class = "b", coef = "positiontop"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.13), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Reduced Models ----
red_rq2_precsd_hum <- brm(
  precsd ~ 0 + folder + position +
    (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder != "chimps") |> mutate(time_3 = time_3 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precsd_hum_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

red_rq2_precsd_chi <- brm(
  precsd ~ position + (1 + time_3 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_3 = time_3 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precsd_chi_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

red_rq2_precsd_chi_2 <- brm(
  precsd ~ position + (1 + time_2 + position | group_id),
  data   = df_tot |> filter(folder == "chimps") |> mutate(time_2 = time_2 - 1),
  family = Gamma(link = "log"),
  prior  = priors_rq2_precsd_chi_2_red,
  sample_prior = "yes",
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)

## Model Comparison ----
# Humans
loo_full_precsd_rq2_hum <- loo(full_rq2_precsd_hum)
loo_red_precsd_rq2_hum <- loo(red_rq2_precsd_hum)
loo_compare(loo_full_precsd_rq2_hum, loo_red_precsd_rq2_hum)

# Chimps I
loo_full_precsd_rq2_chi <- loo(full_rq2_precsd_chi)
loo_red_precsd_rq2_chi <- loo(red_rq2_precsd_chi)
loo_compare(loo_full_precsd_rq2_chi, loo_red_precsd_rq2_chi)

# Chimps II
loo_full_precsd_rq2_chi_2 <- loo(full_rq2_precsd_chi_2)
loo_red_precsd_rq2_chi_2 <- loo(red_rq2_precsd_chi_2)
loo_compare(loo_full_precsd_rq2_chi_2, loo_red_precsd_rq2_chi_2)

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp1", "img", "rq2_precsd_hum_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_precsd_hum, ndraws = 100)
dev.off()

png(here("exp1", "img", "rq2_precsd_chi1_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_precsd_chi, ndraws = 100)
dev.off()

png(here("exp1", "img", "rq2_precsd_chi2_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_precsd_chi_2, ndraws = 100)
dev.off()

png(here("exp1", "img", "rq2_precsd_hum_ppc_grouped.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq2_precsd_hum, type = "intervals_grouped", group = "folder") # might exceed memory limits
dev.off()

## Posterior Distribution ----
## Preparation
group_order <- c(
  "4m", "6m", "9m", "18m", "adults",
  "chimp_days", "chimp_trials"
)

group_labels <- c(
  "4m"           = "4 Months",
  "6m"           = "6 Months",
  "9m"           = "9 Months",
  "18m"          = "18 Months",
  "adults"       = "Adults",
  "chimp_days"   = "Chimpanzees (days)",
  "chimp_trials" = "Chimpanzees (trials)"
)

## Humans
post_hum <- as_draws_df(full_rq2_precsd_hum)

slope_hum <- post_hum |>
  transmute(
    `4m`     = `b_folder4m:time_3`,
    `6m`     = `b_folder6m:time_3`,
    `9m`     = `b_folder9m:time_3`,
    `18m`    = `b_folder18m:time_3`,
    `adults` = `b_folderadults:time_3`
  ) |>
  mutate(.draw = row_number()) |>
  pivot_longer(
    cols = -.draw,
    names_to = "group",
    values_to = "slope"
  )

## Chimpanzees Days
post_ch_days <- as_draws_df(full_rq2_precsd_chi)

slope_ch_days <- post_ch_days |>
  transmute(
    group = "chimp_days",
    slope = b_time_3
  ) |>
  mutate(.draw = row_number())

## Chimpanzees Trials
post_ch_trials <- as_draws_df(full_rq2_precsd_chi_2)

slope_ch_trials <- post_ch_trials |>
  transmute(
    group = "chimp_trials",
    slope = b_time_2
  ) |>
  mutate(.draw = row_number())

## Combine
slope_all <- bind_rows(
  slope_hum |> select(.draw, group, slope),
  slope_ch_days |> select(.draw, group, slope),
  slope_ch_trials |> select(.draw, group, slope)
) |>
  mutate(group = factor(group, levels = group_order))

## Plot
posterior_plot_rq2_precsd <- ggplot(
  slope_all,
  aes(
    x = slope,
    y = factor(group, levels = rev(group_order))
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  stat_halfeye(
    point_interval = "median_qi",
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05,
    adjust = 1.0,
    fill = "grey70"
  ) +
  scale_y_discrete(labels = group_labels) +
  scale_x_continuous(
    breaks = seq(-0.03, 0.03, by = 0.01),
    limits = c(-0.03, 0.03)
  ) +
  labs(
    x = "Time",
    y = NULL
  ) +
  theme_bw(base_size = 14)

png(here("exp1", "img", "rq2_precsd_posterior.png"), width = 2480, height = 3508/4.5, res = 190)
posterior_plot_rq2_precsd
dev.off()

## Posterior Versus Prior Plot ----
# Humans
png(here("exp1", "img", "rq2_precsd_posteriorprior_4m.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precsd_hum, pars = c("b_folder4m:time_3", "prior_b_folder4m:time_3"), facet_label = "4 Months")
dev.off()

png(here("exp1", "img", "rq2_precsd_posteriorprior_6m.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precsd_hum, pars = c("b_folder6m:time_3", "prior_b_folder6m:time_3"), facet_label = "6 Months")
dev.off()

png(here("exp1", "img", "rq2_precsd_posteriorprior_9m.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precsd_hum, pars = c("b_folder9m:time_3", "prior_b_folder9m:time_3"), facet_label = "9 Months")
dev.off()

png(here("exp1", "img", "rq2_precsd_posteriorprior_18m.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precsd_hum, pars = c("b_folder18m:time_3", "prior_b_folder18m:time_3"), facet_label = "18 Months")
dev.off()

png(here("exp1", "img", "rq2_precsd_posteriorprior_adults.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precsd_hum, pars = c("b_folderadults:time_3", "prior_b_folderadults:time_3"), facet_label = "Adults")
dev.off()

# Chimps
png(here("exp1", "img", "rq2_precsd_posteriorprior_chimps_days.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precsd_chi, pars = c("b_time_3", "prior_b_time_3"), facet_label = "Chimpanzees (Time as Testing Days)")
dev.off()

png(here("exp1", "img", "rq2_precsd_posteriorprior_chimps_trials.png"), width = 2480/2, height = 3508/3, res = 250)
plot_prior_vs_poster(full_rq2_precsd_chi_2, pars = c("b_time_2", "prior_b_time_2"), facet_label = "Chimpanzees (Time as Trials Within Testing Days)")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(folder, group_id, time_3) |> # or time_1 or time_2 or time_3
  summarize(precsd_visd = mean(precsd_visd, na.rm = T)) |> 
  group_by(folder, time_3) |> # or time_1 or time_2 or time_3
  summarize(mean_precsd_visd = mean(precsd_visd, na.rm = T),
            sd_precsd_visd = sd(precsd_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(folder, time_3) # or time_1 or time_2 or time_3

## Paper Plot ----
plot_rq2(df = df_tot, png_name = "rq2_precsd_session_all.png", out_dir = here::here("exp1", "img"), width = 2480*1.5, height = 3508, res = 300,
                         group_var = "folder", x_var   = "time_1", y_var = "precsd_visd",
                         xmax_chimps = 8, filter_chimps_time_gt = TRUE, ymin_humans = 0, ymax_humans = 1.5, ymin_chimps = 0, ymax_chimps = 1.5,
                         ytitle = "Precision (SD)\nin visual degrees", x_label = "Time\n(trials in humans; sessions in apes)")

plot_rq2(df = df_tot, png_name = "rq2_precsd_trial_all.png", out_dir = here::here("exp1", "img"), width = 2480*1.5, height = 3508, res = 300,
                         group_var = "folder", x_var   = "time_2", y_var = "precsd_visd",
                         xmax_chimps = 11, filter_chimps_time_gt = TRUE, ymin_humans = 0, ymax_humans = 1.5, ymin_chimps = 0, ymax_chimps = 1.5,
                         ytitle = "Precision (SD)\nin visual degrees", x_label = "Time\n(trials in humans; trials within sessions in apes)")

plot_rq2(df = df_tot, png_name = "rq2_precsd_day_all.png", out_dir = here::here("exp1", "img"), width = 2480*1.5, height = 3508, res = 300,
                         group_var = "folder", x_var   = "time_3", y_var = "precsd_visd",
                         xmax_chimps = 7, filter_chimps_time_gt = TRUE, ymin_humans = 0, ymax_humans = 1.5, ymin_chimps = 0, ymax_chimps = 1.5,
                         ytitle = "Precision (SD)\nin visual degrees", x_label = "Time\n(trials in humans; days in apes)")

## Supp Plots Per Chimp ----
df_plot_rq2_precsd <- df_tot |> 
  filter(folder == "chimps") |> 
  group_by(group_id, time_3) |> 
  summarize(precsd_visd = mean(precsd_visd, na.rm = TRUE), .groups = "drop") |> 
  drop_na(time_3) |> 
  mutate(group_id_pretty = str_to_title(group_id))

y_range <- range(df_plot_rq2_precsd$precsd_visd, na.rm = TRUE)
ncol_plot <- 3
nrow_plot <- 3
plots_per_page <- ncol_plot * nrow_plot
n_pages <- ceiling(n_distinct(df_plot_rq2_precsd$group_id_pretty) / plots_per_page)

for (page in 1:n_pages) {
  
  p_rq2_precsd <- ggplot(df_plot_rq2_precsd, aes(x = time_3, y = precsd_visd, group = group_id_pretty)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 2) +
    facet_wrap_paginate(
      ~ group_id_pretty,
      ncol = ncol_plot,
      nrow = nrow_plot,
      page = page
    ) +
    scale_y_continuous(limits = y_range) +
    scale_x_continuous(breaks = sort(unique(df_plot_rq2_precsd$time_3))) +
    labs(
      x = "Time",
      y = "Precision (SD)"
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    filename = file.path(here("exp1", "img", paste0("individual_precsd_visd_plots_page_", page, ".png"))
    ),
    plot = p_rq2_precsd,
    width = 11,
    height = 8.5,
    dpi = 300
  )
}

# RQ3 (Fixation Duration) -------------------------------------------------

df_rq3 <- df_tot |> 
  group_by(folder, group_id) |> 
  summarise(
    mean_fixation_duration = mean(mean_fixation_duration, na.rm = TRUE),
    mean_fixation_number   = mean(mean_fixation_number, na.rm = TRUE),
    latencies              = mean(latencies, na.rm = TRUE),
    rel_gaze_in_aoi        = mean(rel_gaze_in_aoi, na.rm = TRUE),
    
    acc_visd               = mean(acc_visd, na.rm = TRUE),
    precrms_visd           = mean(precrms_visd, na.rm = TRUE),
    precsd_visd            = mean(precsd_visd, na.rm = TRUE),
    robustness_prop_2      = mean(robustness_prop_2, na.rm = TRUE),
    
    .groups = "drop"
  ) |> 
  mutate(
    robustness_prop_2 = ifelse(is.nan(robustness_prop_2), NA, robustness_prop_2)
  )

## Define Priors ----
# Group (folder) main effects:
# We include 0 + folder so that each sample gets its own baseline
# expected fixation duration. This avoids forcing all groups to share
# one global intercept, which would be implausibly restrictive.
#
# Fixed-effect priors on group (folder):
# We assumed that fixations around 150 ms are at the lower plausible end 
# and that around 1000 ms is already rather long.
# On the log scale, this corresponds roughly to a broad center around 6.
# We use the same prior for all groups because we do not have
# group-specific prior expectations.
#
# Slope priors:
# Priors are centered at zero because we preregistered to use uninformative priors.
# We use the same prior within each predictor family across groups.
# The scales differ across predictor families because the predictors are on
# different raw scales and were not z-standardized.
#
# Accuracy and precision:
# We encode the prior belief that a +1 increase in accuracy should
# not typically imply more than about +500 ms in fixation duration.
# Using a rough reference duration of ~400 ms (expected "typical fixation duration"), 
# +500 ms corresponds to a multiplicative increase of 900 / 400 = 2.25, i.e. log(2.25) = 0.811.
# Treating this as roughly the outer 95% prior range gives:
# sd = log(2.25) / 1.96 = 0.414.
# Precision RMS was assigned a slightly broader prior because prior-predictive inspection indicated 
# that the original prior was comparatively restrictive relative to the range of plausible effects.
#
# Robustness:
# We encode the prior belief that a +0.02 increase in robustness should
# not typically imply more than about +100 ms in fixation duration.
# Again using ~400 ms as a rough reference, +100 ms corresponds to
# 500 / 400 = 1.25, i.e. log(1.25) = 0.223.
# Because this effect refers to a +0.02 increase in robustness,
# the corresponding slope per +1 unit is 0.223 / 0.02 = 11.15.
# Treating this as roughly the outer 95% prior range gives:
# sd = 11.15 / 1.96 = 5.69.
#
# Random-effect priors:
# see RQ1+2.
#
# Gamma shape prior:
# We keep a very broad gamma(0.01, 0.01) prior on the shape parameter.
# This is intentionally weakly informative and only enforces positivity.

prior_rq3_fixdur <- c(
  # Sample-specific baseline fixation durations (log-mean scale)
  # plausible end and that around 1000 ms are already rather long.
  # This corresponds roughly to log(150) ≈ 5.0 and log(1000) ≈ 6.9, so we center
  # the prior at 6 with SD = 0.7 to allow a broad but still plausible range.
  prior(normal(6, 0.7), class = "b", coef = "folder4m"),
  prior(normal(6, 0.7), class = "b", coef = "folder6m"),
  prior(normal(6, 0.7), class = "b", coef = "folder9m"),
  prior(normal(6, 0.7), class = "b", coef = "folder18m"),
  prior(normal(6, 0.7), class = "b", coef = "folderadults"),
  prior(normal(6, 0.7), class = "b", coef = "folderchimps"),
  
  # Accuracy slopes:
  # centered at zero; same prior across groups;
  # scaled so that a +1 increase should not typically imply > ~100 ms
  prior(normal(0, 0.4), class = "b", coef = "folder4m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folder6m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folder9m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folder18m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folderadults:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folderchimps:acc_visd"),
  
  # Precision RMS slopes:
  # centered at zero; same prior across groups;
  # same substantive prior constraint as for accuracy
  prior(normal(0, 1.5), class = "b", coef = "folder4m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder6m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder9m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder18m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folderadults:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folderchimps:precrms_visd"),
  
  # Robustness slopes:
  # centered at zero; same prior across groups;
  # scaled so that a +0.1 increase should not typically imply > ~50 ms
  prior(normal(0, 5.69), class = "b", coef = "folder4m:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folder6m:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folder9m:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folder18m:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folderadults:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folderchimps:robustness_prop_2"),

  # Random-effect SDs & random-effect correlations:
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Full Model ----
full_rq3_fixdur <- brm(
  mean_fixation_duration ~ 0 + folder +
    folder:acc_visd +
    folder:precrms_visd +
    folder:robustness_prop_2 +
    (1 + acc_visd + precrms_visd + robustness_prop_2 | group_id),
  data   = df_rq3,
  family = Gamma(link = "log"),
  prior  = prior_rq3_fixdur,
  chains = 4, cores = n_cores - 1, iter = 16000, warmup = 6000, 
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model ----
prior_rq3_fixdur_red <- c(
  # Sample-specific baseline fixation durations (log-mean scale)
  # plausible end and that around 1000 ms are already rather long.
  # This corresponds roughly to log(150) ≈ 5.0 and log(1000) ≈ 6.9, so we center
  # the prior at 6 with SD = 0.7 to allow a broad but still plausible range.
  prior(normal(6, 0.7), class = "b", coef = "folder4m"),
  prior(normal(6, 0.7), class = "b", coef = "folder6m"),
  prior(normal(6, 0.7), class = "b", coef = "folder9m"),
  prior(normal(6, 0.7), class = "b", coef = "folder18m"),
  prior(normal(6, 0.7), class = "b", coef = "folderadults"),
  prior(normal(6, 0.7), class = "b", coef = "folderchimps"),
  
  # Random-effect SDs & random-effect correlations:
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor"),
  
  # Gamma shape:
  prior(gamma(0.01, 0.01), class = "shape")
)

## Reduced Model ----
red_rq3_fixdur <- brm(
  mean_fixation_duration ~ 0 + folder +
    (1 + acc_visd + precrms_visd + robustness_prop_2 | group_id),
  data   = df_rq3,
  family = Gamma(link = "log"),
  prior  = prior_rq3_fixdur_red,
  chains = 4, cores = n_cores - 1, iter = 10000, warmup = 4000,
  sample_prior = "yes",
  seed = 123
)

## Model Comparison ----
loo_full_fixdur <- loo(full_rq3_fixdur)
loo_red_fixdur <- loo(red_rq3_fixdur)
loo_compare(loo_full_fixdur, loo_red_fixdur) # red_rq3_fixdur  -47.7      7.0   

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp1", "img", "rq3_fixdur_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq3_fixdur, ndraws = 100)
#pp_check(full_rq3_fixdur, type = "hist")
dev.off()

## Posterior Distribution ----
post_rq3 <- as_draws_df(full_rq3_fixdur)

slope_rq3 <- post_rq3 |>
  transmute(
    `4m_acc_visd`              = `b_folder4m:acc_visd`,
    `6m_acc_visd`              = `b_folder6m:acc_visd`,
    `9m_acc_visd`              = `b_folder9m:acc_visd`,
    `18m_acc_visd`             = `b_folder18m:acc_visd`,
    `adults_acc_visd`          = `b_folderadults:acc_visd`,
    `chimps_acc_visd`          = `b_folderchimps:acc_visd`,
    
    `4m_precrms_visd`          = `b_folder4m:precrms_visd`,
    `6m_precrms_visd`          = `b_folder6m:precrms_visd`,
    `9m_precrms_visd`          = `b_folder9m:precrms_visd`,
    `18m_precrms_visd`         = `b_folder18m:precrms_visd`,
    `adults_precrms_visd`      = `b_folderadults:precrms_visd`,
    `chimps_precrms_visd`      = `b_folderchimps:precrms_visd`,
    
    `4m_robustness_prop_2`     = `b_folder4m:robustness_prop_2`,
    `6m_robustness_prop_2`     = `b_folder6m:robustness_prop_2`,
    `9m_robustness_prop_2`     = `b_folder9m:robustness_prop_2`,
    `18m_robustness_prop_2`    = `b_folder18m:robustness_prop_2`,
    `adults_robustness_prop_2` = `b_folderadults:robustness_prop_2`,
    `chimps_robustness_prop_2` = `b_folderchimps:robustness_prop_2`
  ) |>
  mutate(.draw = row_number()) |>
  pivot_longer(
    cols = -.draw,
    names_to = c("group", "predictor"),
    names_pattern = "^(4m|6m|9m|18m|adults|chimps)_(.*)$",
    values_to = "slope"
  ) |>
  mutate(
    group = factor(group, levels = group_order),
    predictor = factor(predictor, levels = predictor_order)
  )

posterior_plot_rq3_fixdur <- ggplot(
  slope_rq3,
  aes(
    x = slope,
    y = factor(group, levels = rev(group_order))
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  stat_halfeye(
    point_interval = "median_qi",
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05,
    adjust = 1.0,
    fill = "grey70"
  ) +
  facet_wrap(
    ~ predictor,
    ncol = 3,
    scales = "free_x",
    labeller = labeller(predictor = predictor_labels)
  ) +
  scale_y_discrete(labels = group_labels) +
  labs(
    x = "Slope Estimate",
    y = NULL
  ) +
  theme_bw(base_size = 14)

png(here("exp1", "img", "rq3_fixdur_posterior_3c.png"), width = 2480, height = 3508 / 4, res = 250)
posterior_plot_rq3_fixdur
dev.off()

## Posterior Versus Prior Plots ----

# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
## Accuracy
png(here("exp1", "img", "rq3_fixdur_posteriorprior_4m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder4m:acc_visd", "prior_b_folder4m:acc_visd"), facet_label = "4-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_6m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder6m:acc_visd", "prior_b_folder6m:acc_visd"), facet_label = "6-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_9m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder9m:acc_visd", "prior_b_folder9m:acc_visd"), facet_label = "9-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_18m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder18m:acc_visd", "prior_b_folder18m:acc_visd"), facet_label = "18-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_adults_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folderadults:acc_visd", "prior_b_folderadults:acc_visd"), facet_label = "Adults, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_chimps_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folderchimps:acc_visd", "prior_b_folderchimps:acc_visd"), facet_label = "Chimpanzees, Accuracy")
dev.off()

## Precision (RMS)
png(here("exp1", "img", "rq3_fixdur_posteriorprior_4m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder4m:precrms_visd", "prior_b_folder4m:precrms_visd"), facet_label = "4-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_6m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder6m:precrms_visd", "prior_b_folder6m:precrms_visd"), facet_label = "6-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_9m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder9m:precrms_visd", "prior_b_folder9m:precrms_visd"), facet_label = "9-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_18m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder18m:precrms_visd", "prior_b_folder18m:precrms_visd"), facet_label = "18-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_adults_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folderadults:precrms_visd", "prior_b_folderadults:precrms_visd"), facet_label = "Adults, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_chimps_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folderchimps:precrms_visd", "prior_b_folderchimps:precrms_visd"), facet_label = "Chimpanzees, Precision (RMS)")
dev.off()

## Robustness
png(here("exp1", "img", "rq3_fixdur_posteriorprior_4m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder4m:robustness_prop_2", "prior_b_folder4m:robustness_prop_2"), facet_label = "4-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_6m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder6m:robustness_prop_2", "prior_b_folder6m:robustness_prop_2"), facet_label = "6-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_9m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder9m:robustness_prop_2", "prior_b_folder9m:robustness_prop_2"), facet_label = "9-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorprior_18m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folder18m:robustness_prop_2", "prior_b_folder18m:robustness_prop_2"), facet_label = "18-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorpriora_adults_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folderadults:robustness_prop_2", "prior_b_folderadults:robustness_prop_2"), facet_label = "Adults, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixdur_posteriorpriora_chimps_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixdur, pars = c("b_folderchimps:robustness_prop_2", "prior_b_folderchimps:robustness_prop_2"), facet_label = "Chimpanzees, Robustness")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(mean_fixation_duration = mean(mean_fixation_duration, na.rm = T)) |> 
  as.data.frame() |> 
  group_by(folder) |> 
  summarize(mean_mean_fixation_duration = round(mean(mean_fixation_duration, na.rm = T),2),
            sd_mean_fixation_duration = round(sd(mean_fixation_duration, na.rm = T),2)) |> 
  as.data.frame() |> 
  ungroup() |> 
  arrange(mean_mean_fixation_duration)

## Paper Plot ----
plot_rq3(df = df_tot, x_var = "acc_visd", y_var = "mean_fixation_duration", width = 2480, height = 3508/2, res = 250,
  png_name = "rq3_acc_fixdur.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")

plot_rq3(df = df_tot, x_var = "precsd_visd", y_var = "mean_fixation_duration", width = 2480, height = 3508/2, res = 250,
  png_name = "rq3_precsd_fixdur.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")

plot_rq3(df = df_tot, x_var = "precrms_visd", y_var = "mean_fixation_duration", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_precrms_fixdur.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")

plot_rq3(df = df_tot, x_var = "robustness_prop_2", y_var = "mean_fixation_duration", width = 2480, height = 3508/2, res = 250,
  png_name = "rq3_rob_fixdur.png", x_lab = "Robustness\n(in %)", y_lab = "Mean Fixation Duration\n(in ms)")

# Optionally not separated by group
# plot_rq3_all(df = df_tot, x_var = "acc_visd", y_var = "mean_fixation_duration",
#   png_name = "rq3_all_acc_fixdur.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")
# 
# plot_rq3_all(df = df_tot, x_var = "precsd_visd", y_var = "mean_fixation_duration",
#              png_name = "rq3_all_precsd_fixdur.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")
# 
# plot_rq3_all(df = df_tot, x_var = "precrms_visd", y_var = "mean_fixation_duration",
#              png_name = "rq3_all_precrms_fixdur.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")
# 
# plot_rq3_all(df = df_tot, x_var = "robustness_prop_2", y_var = "mean_fixation_duration",
#              png_name = "rq3_all_rob_fixdur.png", x_lab = "Robustness\n(in %)", y_lab = "Mean Fixation Duration\n(in ms)")

# RQ3 (Fixation Number) ---------------------------------------------------

## Define Priors ----
# Group (folder) main effects:
# We include 0 + folder so that each sample gets its own baseline
# expected fixation number. This avoids forcing all groups to share
# one global intercept, which would be implausibly restrictive.
#
# We assumed a priori that around 10 fixations per trial would be a plausible
# central value across groups. On the log scale, this corresponds to log(10) ≈ 2.30.
# We use the same prior for all groups because we do not have group-specific prior expectations.
# (Chimpanzees are generally less interested, however the stimuli stayed for longer on screen.)
# We set SD = 1.0 to allow a broad range of plausible baseline counts
# while keeping the prior weakly informative.
#
# Slope priors:
# Priors are centered at zero because we preregistered to use uninformative priors.
# We use the same prior within each predictor family across groups.
# The scales differ across predictor families because the predictors are on
# different raw scales and were not z-standardized.
#
# Random-effect priors:
# see RQ1+2.

prior_rq3_fixnum <- c(
  # Sample-specific baseline fixation numbers (log-mean scale)
  prior(normal(2.3, 1.0), class = "b", coef = "folder4m"),
  prior(normal(2.3, 1.0), class = "b", coef = "folder6m"),
  prior(normal(2.3, 1.0), class = "b", coef = "folder9m"),
  prior(normal(2.3, 1.0), class = "b", coef = "folder18m"),
  prior(normal(2.3, 1.0), class = "b", coef = "folderadults"),
  prior(normal(2.3, 1.0), class = "b", coef = "folderchimps"),
  
  # Accuracy slopes:
  # centered at zero; same prior across groups
  prior(normal(0, 0.4), class = "b", coef = "folder4m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folder6m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folder9m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folder18m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folderadults:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folderchimps:acc_visd"),
  
  # Precision RMS slopes:
  # centered at zero; same prior across groups
  prior(normal(0, 1.5), class = "b", coef = "folder4m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder6m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder9m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder18m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folderadults:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folderchimps:precrms_visd"),
  
  # Robustness slopes:
  # centered at zero; same prior across groups
  prior(normal(0, 5.69), class = "b", coef = "folder4m:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folder6m:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folder9m:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folder18m:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folderadults:robustness_prop_2"),
  prior(normal(0, 5.69), class = "b", coef = "folderchimps:robustness_prop_2"),
  
  # Random-effect SDs & random-effect correlations:
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Full Model ----
full_rq3_fixnum <- brm(
  mean_fixation_number ~ 0 + folder +
    folder:acc_visd +
    folder:precrms_visd +
    folder:robustness_prop_2 +
    (1 + acc_visd + precrms_visd + robustness_prop_2 | group_id),
  data   = df_rq3,
  family = Gamma(link = "log"),
  prior  = prior_rq3_fixnum,
  chains = 4, cores = n_cores - 1, iter = 16000, warmup = 6000, 
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model ----
prior_rq3_fixnum_red <- c(
  # Sample-specific baseline fixation numbers (log-mean scale)
  prior(normal(2.3, 1.0), class = "b", coef = "folder4m"),
  prior(normal(2.3, 1.0), class = "b", coef = "folder6m"),
  prior(normal(2.3, 1.0), class = "b", coef = "folder9m"),
  prior(normal(2.3, 1.0), class = "b", coef = "folder18m"),
  prior(normal(2.3, 1.0), class = "b", coef = "folderadults"),
  prior(normal(2.3, 1.0), class = "b", coef = "folderchimps"),
  
  # Random-effect SDs & random-effect correlations:
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Reduced Model ----
full_rq3_fixnum_red <- brm(
  mean_fixation_number ~ 0 + folder +
    (1 + acc_visd + precrms_visd + robustness_prop_2 | group_id),
  data   = df_rq3,
  family = Gamma(link = "log"),
  prior  = prior_rq3_fixnum_red,
  chains = 4, cores = n_cores - 1, iter = 16000, warmup = 6000, 
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  sample_prior = "yes",
  seed = 123
)

## Model Comparison ----
loo_full_fixnum <- loo(full_rq3_fixnum)
loo_red_fixnum <- loo(red_rq3_fixnum)
loo_compare(loo_full_fixnum, loo_red_fixnum)

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp1", "img", "rq3_fixnum_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq3_fixnum, ndraws = 100)
dev.off()

## Posterior Distribution ----
post_rq3 <- as_draws_df(full_rq3_fixnum)

slope_rq3 <- post_rq3 |>
  transmute(
    `4m_acc_visd`              = `b_folder4m:acc_visd`,
    `6m_acc_visd`              = `b_folder6m:acc_visd`,
    `9m_acc_visd`              = `b_folder9m:acc_visd`,
    `18m_acc_visd`             = `b_folder18m:acc_visd`,
    `adults_acc_visd`          = `b_folderadults:acc_visd`,
    `chimps_acc_visd`          = `b_folderchimps:acc_visd`,
    
    `4m_precrms_visd`          = `b_folder4m:precrms_visd`,
    `6m_precrms_visd`          = `b_folder6m:precrms_visd`,
    `9m_precrms_visd`          = `b_folder9m:precrms_visd`,
    `18m_precrms_visd`         = `b_folder18m:precrms_visd`,
    `adults_precrms_visd`      = `b_folderadults:precrms_visd`,
    `chimps_precrms_visd`      = `b_folderchimps:precrms_visd`,
    
    `4m_robustness_prop_2`     = `b_folder4m:robustness_prop_2`,
    `6m_robustness_prop_2`     = `b_folder6m:robustness_prop_2`,
    `9m_robustness_prop_2`     = `b_folder9m:robustness_prop_2`,
    `18m_robustness_prop_2`    = `b_folder18m:robustness_prop_2`,
    `adults_robustness_prop_2` = `b_folderadults:robustness_prop_2`,
    `chimps_robustness_prop_2` = `b_folderchimps:robustness_prop_2`
  ) |>
  mutate(.draw = row_number()) |>
  pivot_longer(
    cols = -.draw,
    names_to = c("group", "predictor"),
    names_pattern = "^(4m|6m|9m|18m|adults|chimps)_(.*)$",
    values_to = "slope"
  ) |>
  mutate(
    group = factor(group, levels = group_order),
    predictor = factor(predictor, levels = predictor_order)
  )

posterior_plot_rq3_fixnum <- ggplot(
  slope_rq3,
  aes(
    x = slope,
    y = factor(group, levels = rev(group_order))
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  stat_halfeye(
    point_interval = "median_qi",
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05,
    adjust = 1.0,
    fill = "grey70"
  ) +
  facet_wrap(
    ~ predictor,
    ncol = 3,
    scales = "free_x",
    labeller = labeller(predictor = predictor_labels)
  ) +
  scale_y_discrete(labels = group_labels) +
  labs(
    x = "Slope Estimate",
    y = NULL
  ) +
  theme_bw(base_size = 14)

png(here("exp1", "img", "rq3_fixnum_posterior_3c.png"), width = 2480, height = 3508 / 4, res = 250)
posterior_plot_rq3_fixnum
dev.off()

## Posterior Versus Prior Plots ----

# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
## Accuracy
png(here("exp1", "img", "rq3_fixnum_posteriorprior_4m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder4m:acc_visd", "prior_b_folder4m:acc_visd"), facet_label = "4-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_6m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder6m:acc_visd", "prior_b_folder6m:acc_visd"), facet_label = "6-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_9m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder9m:acc_visd", "prior_b_folder9m:acc_visd"), facet_label = "9-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_18m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder18m:acc_visd", "prior_b_folder18m:acc_visd"), facet_label = "18-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_adults_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folderadults:acc_visd", "prior_b_folderadults:acc_visd"), facet_label = "Adults, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_chimps_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folderchimps:acc_visd", "prior_b_folderchimps:acc_visd"), facet_label = "Chimpanzees, Accuracy")
dev.off()

## Precision (RMS)
png(here("exp1", "img", "rq3_fixnum_posteriorprior_4m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder4m:precrms_visd", "prior_b_folder4m:precrms_visd"), facet_label = "4-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_6m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder6m:precrms_visd", "prior_b_folder6m:precrms_visd"), facet_label = "6-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_9m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder9m:precrms_visd", "prior_b_folder9m:precrms_visd"), facet_label = "9-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_18m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder18m:precrms_visd", "prior_b_folder18m:precrms_visd"), facet_label = "18-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_adults_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folderadults:precrms_visd", "prior_b_folderadults:precrms_visd"), facet_label = "Adults, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_chimps_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folderchimps:precrms_visd", "prior_b_folderchimps:precrms_visd"), facet_label = "Chimpanzees, Precision (RMS)")
dev.off()

## Robustness
png(here("exp1", "img", "rq3_fixnum_posteriorprior_4m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder4m:robustness_prop_2", "prior_b_folder4m:robustness_prop_2"), facet_label = "4-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_6m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder6m:robustness_prop_2", "prior_b_folder6m:robustness_prop_2"), facet_label = "6-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_9m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder9m:robustness_prop_2", "prior_b_folder9m:robustness_prop_2"), facet_label = "9-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_18m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folder18m:robustness_prop_2", "prior_b_folder18m:robustness_prop_2"), facet_label = "18-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_adults_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folderadults:robustness_prop_2", "prior_b_folderadults:robustness_prop_2"), facet_label = "Adults, Robustness")
dev.off()

png(here("exp1", "img", "rq3_fixnum_posteriorprior_chimps_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_fixnum, pars = c("b_folderchimps:robustness_prop_2", "prior_b_folderchimps:robustness_prop_2"), facet_label = "Chimpanzees, Robustness")
dev.off()


## Descriptives ----
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(mean_fixation_number = mean(mean_fixation_number, na.rm = T)) |> 
  as.data.frame() |> 
  group_by(folder) |> 
  summarize(mean_mean_fixation_number = round(mean(mean_fixation_number, na.rm = T),2),
            sd_mean_fixation_number = round(sd(mean_fixation_number, na.rm = T),2)) |> 
  as.data.frame() |> 
  ungroup() |>
  slice(3,4,5,2,6,1)

## Paper Plot ----
plot_rq3(df = df_tot, x_var = "acc_visd", y_var = "mean_fixation_number", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_acc_fixnum.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Mean Fixation Number")

plot_rq3(df = df_tot, x_var = "precsd_visd", y_var = "mean_fixation_number", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_precsd_fixnum.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Mean Fixation Number")

plot_rq3(df = df_tot, x_var = "precrms_visd", y_var = "mean_fixation_number", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_precrms_fixnum.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Mean Fixation Number")

plot_rq3(df = df_tot, x_var = "robustness_prop_2", y_var = "mean_fixation_number", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_rob_fixnum.png", x_lab = "Robustness\n(in %)", y_lab = "Mean Fixation Number")

# Optionally not separated by group
# plot_rq3_all(df = df_tot, x_var = "acc_visd", y_var = "mean_fixation_number",
#              png_name = "rq3_all_acc_fixnum.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Mean Fixation Number")
# 
# plot_rq3_all(df = df_tot, x_var = "precsd_visd", y_var = "mean_fixation_number",
#              png_name = "rq3_all_precsd_fixnum.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Mean Fixation Number")
# 
# plot_rq3_all(df = df_tot, x_var = "precrms_visd", y_var = "mean_fixation_number",
#              png_name = "rq3_all_precrms_fixnum.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Mean Fixation Number")
# 
# plot_rq3_all(df = df_tot, x_var = "robustness_prop_2", y_var = "mean_fixation_number",
#              png_name = "rq3_all_rob_fixnum.png", x_lab = "Robustness\n(in %)", y_lab = "Mean Fixation Number")

# RQ3 (Latencies) ---------------------------------------------------------

## Define Priors ----
# Group (folder) main effects:
# We include 0 + folder so that each sample gets its own baseline
# expected fixation number. This avoids forcing all groups to share
# one global intercept, which would be implausibly restrictive.
#
# We assumed a priori that latencies around 1000ms (4M), 500 (6M, 9M, 18M), 250 (Adults) 
# per trial would be a plausible central value. 
# On the log scale, this corresponds to log(1000) = 6.91, log(500) ≈ 6.22, log(250) = 5.52.
# We set SD = 3 to allow a broad range of plausible latencies while keeping the prior weakly informative.
#
# Slope priors:
# Priors are centered at zero because we preregistered to use uninformative priors.
# We use the same prior within each predictor family across groups.
# The scales differ across predictor families because the predictors are on
# different raw scales and were not z-standardized.
#
# Random-effect priors:
# see RQ1+2.

prior_rq3_latencies <- c(
  # Sample-specific baseline fixation numbers (log-mean scale)
  prior(normal(6.91, 3), class = "b", coef = "folder4m"),
  prior(normal(6.22, 3), class = "b", coef = "folder6m"),
  prior(normal(6.22, 3), class = "b", coef = "folder9m"),
  prior(normal(6.22, 3), class = "b", coef = "folder18m"),
  prior(normal(5.52, 3), class = "b", coef = "folderadults"),
  
  # Accuracy slopes:
  # centered at zero; same prior across groups
  prior(normal(0, 1), class = "b", coef = "folder4m:acc_visd"),
  prior(normal(0, 1), class = "b", coef = "folder6m:acc_visd"),
  prior(normal(0, 1), class = "b", coef = "folder9m:acc_visd"),
  prior(normal(0, 1), class = "b", coef = "folder18m:acc_visd"),
  prior(normal(0, 1), class = "b", coef = "folderadults:acc_visd"),
  
  # Precision RMS slopes:
  # centered at zero; same prior across groups
  prior(normal(0, 1.5), class = "b", coef = "folder4m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder6m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder9m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder18m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folderadults:precrms_visd"),
  
  # Robustness slopes:
  # centered at zero; same prior across groups
  prior(normal(0, 6), class = "b", coef = "folder4m:robustness_prop_2"),
  prior(normal(0, 6), class = "b", coef = "folder6m:robustness_prop_2"),
  prior(normal(0, 6), class = "b", coef = "folder9m:robustness_prop_2"),
  prior(normal(0, 6), class = "b", coef = "folder18m:robustness_prop_2"),
  prior(normal(0, 6), class = "b", coef = "folderadults:robustness_prop_2"),
  
  # Random-effect SDs & random-effect correlations:
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Full Model ----
full_rq3_latencies <- brm(
  latencies ~ 0 + folder +
    folder:acc_visd +
    folder:precrms_visd +
    folder:robustness_prop_2 +
    (1 + acc_visd + precrms_visd + robustness_prop_2 | group_id),
  data   = df_rq3,
  family = Gamma(link = "log"),
  prior  = prior_rq3_latencies,
  chains = 4, cores = n_cores - 1, iter = 16000, warmup = 6000, 
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Reduced Model ----
# Group (folder) main effects:
# We include 0 + folder so that each sample gets its own baseline
# expected fixation number. This avoids forcing all groups to share
# one global intercept, which would be implausibly restrictive.
#
# We assumed a priori that latencies around 1000ms (4M), 500 (6M, 9M, 18M), 250 (Adults) 
# per trial would be a plausible central value. 
# On the log scale, this corresponds to log(1000) = 6.91, log(500) ≈ 6.22, log(250) = 5.52.
# We set SD = 3 to allow a broad range of plausible latencies while keeping the prior weakly informative.
#
# Slope priors:
# Priors are centered at zero because we preregistered to use uninformative priors.
# We use the same prior within each predictor family across groups.
# The scales differ across predictor families because the predictors are on
# different raw scales and were not z-standardized.
#
# Random-effect priors:
# see RQ1+2.

prior_rq3_latencies_red <- c(
  # Sample-specific baseline fixation numbers (log-mean scale)
  prior(normal(6.91, 3), class = "b", coef = "folder4m"),
  prior(normal(6.22, 3), class = "b", coef = "folder6m"),
  prior(normal(6.22, 3), class = "b", coef = "folder9m"),
  prior(normal(6.22, 3), class = "b", coef = "folder18m"),
  prior(normal(5.52, 3), class = "b", coef = "folderadults"),
  
  # Random-effect SDs & random-effect correlations:
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Reduced Model ----
red_rq3_latencies <- brm(
  latencies ~ 0 + folder +
    (1 + acc_visd + precrms_visd + robustness_prop_2 | group_id),
  data   = df_rq3,
  family = Gamma(link = "log"),
  prior  = prior_rq3_latencies_red,
  chains = 4, cores = n_cores - 1, iter = 16000, warmup = 6000, 
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  sample_prior = "yes",
  seed = 123
)

## Model Comparison ----
loo_full_latencies <- loo(full_rq3_latencies)
loo_red_latencies <- loo(red_rq3_latencies)
loo_compare(loo_full_latencies, loo_red_latencies)

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp1", "img", "rq3_latencies_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq3_latencies, ndraws = 100)
dev.off()

## Posterior Distribution ----
post_rq3 <- as_draws_df(full_rq3_latencies)

slope_rq3 <- post_rq3 |>
  transmute(
    `4m_acc_visd`              = `b_folder4m:acc_visd`,
    `6m_acc_visd`              = `b_folder6m:acc_visd`,
    `9m_acc_visd`              = `b_folder9m:acc_visd`,
    `18m_acc_visd`             = `b_folder18m:acc_visd`,
    `adults_acc_visd`          = `b_folderadults:acc_visd`,
    
    `4m_precrms_visd`          = `b_folder4m:precrms_visd`,
    `6m_precrms_visd`          = `b_folder6m:precrms_visd`,
    `9m_precrms_visd`          = `b_folder9m:precrms_visd`,
    `18m_precrms_visd`         = `b_folder18m:precrms_visd`,
    `adults_precrms_visd`      = `b_folderadults:precrms_visd`,
    
    `4m_robustness_prop_2`     = `b_folder4m:robustness_prop_2`,
    `6m_robustness_prop_2`     = `b_folder6m:robustness_prop_2`,
    `9m_robustness_prop_2`     = `b_folder9m:robustness_prop_2`,
    `18m_robustness_prop_2`    = `b_folder18m:robustness_prop_2`,
    `adults_robustness_prop_2` = `b_folderadults:robustness_prop_2`
  ) |>
  mutate(.draw = row_number()) |>
  pivot_longer(
    cols = -.draw,
    names_to = c("group", "predictor"),
    names_pattern = "^(4m|6m|9m|18m|adults)_(.*)$",
    values_to = "slope"
  ) |>
  mutate(
    group = factor(group, levels = group_order),
    predictor = factor(predictor, levels = predictor_order)
  )

posterior_plot_rq3_latencies <- ggplot(
  slope_rq3,
  aes(
    x = slope,
    y = factor(group, levels = rev(group_order))
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  stat_halfeye(
    point_interval = "median_qi",
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05,
    adjust = 1.0,
    fill = "grey70"
  ) +
  facet_wrap(
    ~ predictor,
    ncol = 2,
    scales = "free_x",
    labeller = labeller(predictor = predictor_labels)
  ) +
  scale_y_discrete(labels = group_labels) +
  labs(
    x = "Slope Estimate",
    y = NULL
  ) +
  theme_bw(base_size = 14)

png(here("exp1", "img", "rq3_latencies_posterior.png"), width = 2480, height = 3508 / 4, res = 250)
posterior_plot_rq3_latencies
dev.off()

## Posterior Versus Prior Plots ----

# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
## Accuracy
png(here("exp1", "img", "rq3_latencies_posteriorprior_4m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder4m:acc_visd", "prior_b_folder4m:acc_visd"), facet_label = "4-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_6m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder6m:acc_visd", "prior_b_folder6m:acc_visd"), facet_label = "6-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_9m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder9m:acc_visd", "prior_b_folder9m:acc_visd"), facet_label = "9-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_18m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder18m:acc_visd", "prior_b_folder18m:acc_visd"), facet_label = "18-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_adults_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folderadults:acc_visd", "prior_b_folderadults:acc_visd"), facet_label = "Adults, Accuracy")
dev.off()

## Precision (RMS)
png(here("exp1", "img", "rq3_latencies_posteriorprior_4m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder4m:precrms_visd", "prior_b_folder4m:precrms_visd"), facet_label = "4-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_6m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder6m:precrms_visd", "prior_b_folder6m:precrms_visd"), facet_label = "6-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_9m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder9m:precrms_visd", "prior_b_folder9m:precrms_visd"), facet_label = "9-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_18m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder18m:precrms_visd", "prior_b_folder18m:precrms_visd"), facet_label = "18-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_adults_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folderadults:precrms_visd", "prior_b_folderadults:precrms_visd"), facet_label = "Adults, Precision (RMS)")
dev.off()

## Robustness
png(here("exp1", "img", "rq3_latencies_posteriorprior_4m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder4m:robustness_prop_2", "prior_b_folder4m:robustness_prop_2"), facet_label = "4-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_6m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder6m:robustness_prop_2", "prior_b_folder6m:robustness_prop_2"), facet_label = "6-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_9m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder9m:robustness_prop_2", "prior_b_folder9m:robustness_prop_2"), facet_label = "9-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_18m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folder18m:robustness_prop_2", "prior_b_folder18m:robustness_prop_2"), facet_label = "18-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_latencies_posteriorprior_adults_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_latencies, pars = c("b_folderadults:robustness_prop_2", "prior_b_folderadults:robustness_prop_2"), facet_label = "Adults, Robustness")
dev.off()

## Descriptives ----
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(latencies = mean(latencies, na.rm = T)) |> 
  as.data.frame() |> 
  group_by(folder) |> 
  summarize(mean_latencies = round(mean(latencies, na.rm = T),2),
            sd_latencies = round(sd(latencies, na.rm = T),2)) |> 
  as.data.frame() |> 
  ungroup() |> 
  slice(3,4,5,2,6,1)

## Paper Plot ----
plot_rq3(df = df_tot, x_var = "acc_visd", y_var = "latencies", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_acc_lat.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Latencies\n(in ms)")

plot_rq3(df = df_tot, x_var = "precsd_visd", y_var = "latencies", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_precsd_lat.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Latencies\n(in ms)")

plot_rq3(df = df_tot, x_var = "precrms_visd", y_var = "latencies", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_precrms_lat.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Latencies\n(in ms)")

plot_rq3(df = df_tot, x_var = "robustness_prop_2", y_var = "latencies", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_rob_lat.png", x_lab = "Robustness\n(in %)", y_lab = "Latencies\n(in ms)")

# Optionally not separated by group
# plot_rq3_all(df = df_tot, x_var = "acc_visd", y_var = "latencies",
#              png_name = "rq3_all_acc_lat.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Latencies\n(in ms)")
# 
# plot_rq3_all(df = df_tot, x_var = "precsd_visd", y_var = "latencies",
#              png_name = "rq3_all_precsd_lat.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Latencies\n(in ms)")
# 
# plot_rq3_all(df = df_tot, x_var = "precrms_visd", y_var = "latencies",
#              png_name = "rq3_all_precrms_lat.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Latencies\n(in ms)")
# 
# plot_rq3_all(df = df_tot, x_var = "robustness_prop_2", y_var = "latencies",
#              png_name = "rq3_all_rob_lat.png", x_lab = "Robustness\n(in %)", y_lab = "Latencies\n(in ms)")

# RQ3 (Relative Looking Time) ---------------------------------------------

## Define Priors ----
# Group (folder) main effects:
# We include 0 + folder so that each sample gets its own baseline
# expected relative looking time. This avoids forcing all groups to share
# one global intercept, which would be implausibly restrictive.
#
# We assumed a priori that around relative looking time of
# 0.1 (chimpanzees), 0.9 (adults), 0.75 (infants) would be plausible.
# On the logit scale, this corresponds to qlogis(0.1) ≈ -2.20, qlogis(0.9) ≈ 2.20, qlogis(0.75) ≈ 1.10.
# We set SD = 1.0 to allow a broad range of plausible baseline counts
# while keeping the prior weakly informative.
#
# Slope priors:
# Priors are centered at zero because we preregistered to use uninformative priors.
# We use the same prior within each predictor family across groups.
# The scales differ across predictor families because the predictors are on
# different raw scales and were not z-standardized.
#
# Random-effect priors:
# see RQ1+2.

prior_rq3_rel_gaze_in_aoi <- c(
  # Sample-specific baseline fixation numbers (log-mean scale)
  prior(normal(1.1, 1.0), class = "b", coef = "folder4m"),
  prior(normal(1.1, 1.0), class = "b", coef = "folder6m"),
  prior(normal(1.1, 1.0), class = "b", coef = "folder9m"),
  prior(normal(1.1, 1.0), class = "b", coef = "folder18m"),
  prior(normal(2.2, 1.0), class = "b", coef = "folderadults"),
  prior(normal(-2.2, 1.0), class = "b", coef = "folderchimps"),
  
  # Accuracy slopes:
  # centered at zero; same prior across groups
  prior(normal(0, 0.4), class = "b", coef = "folder4m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folder6m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folder9m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folder18m:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folderadults:acc_visd"),
  prior(normal(0, 0.4), class = "b", coef = "folderchimps:acc_visd"),
  
  # Precision RMS slopes:
  # centered at zero; same prior across groups
  prior(normal(0, 1.5), class = "b", coef = "folder4m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder6m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder9m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folder18m:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folderadults:precrms_visd"),
  prior(normal(0, 1.5), class = "b", coef = "folderchimps:precrms_visd"),
  
  # Robustness slopes:
  # centered at zero; same prior across groups
  prior(normal(0, 5), class = "b", coef = "folder4m:robustness_prop_2"),
  prior(normal(0, 5), class = "b", coef = "folder6m:robustness_prop_2"),
  prior(normal(0, 5), class = "b", coef = "folder9m:robustness_prop_2"),
  prior(normal(0, 5), class = "b", coef = "folder18m:robustness_prop_2"),
  prior(normal(0, 5), class = "b", coef = "folderadults:robustness_prop_2"),
  prior(normal(0, 5), class = "b", coef = "folderchimps:robustness_prop_2"),
  
  # Random-effect SDs & random-effect correlations:
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Full Model ----
full_rq3_rel_gaze_in_aoi <- brm(
  rel_gaze_in_aoi ~ 0 + folder +
    folder:acc_visd +
    folder:precrms_visd +
    folder:robustness_prop_2 +
    (1 + acc_visd + precrms_visd + robustness_prop_2 | group_id),
  data   = df_rq3,
  family = Beta(link = "logit"),
  prior  = prior_rq3_rel_gaze_in_aoi,
  chains = 4, cores = n_cores - 1, iter = 10000, warmup = 5000, 
  sample_prior = "yes",
  seed = 123
)

## Define Priors of Redunced Model ----
prior_rq3_rel_gaze_in_aoi_red <- c(
  # Sample-specific baseline fixation numbers (log-mean scale)
  prior(normal(1.1, 1.0), class = "b", coef = "folder4m"),
  prior(normal(1.1, 1.0), class = "b", coef = "folder6m"),
  prior(normal(1.1, 1.0), class = "b", coef = "folder9m"),
  prior(normal(1.1, 1.0), class = "b", coef = "folder18m"),
  prior(normal(2.2, 1.0), class = "b", coef = "folderadults"),
  prior(normal(-2.2, 1.0), class = "b", coef = "folderchimps"),
  
  # Random-effect SDs & random-effect correlations:
  prior(exponential(2), class = "sd"),
  prior(lkj(2), class = "cor")
)

## Reduced Model ----
red_rq3_rel_gaze_in_aoi <- brm(
  rel_gaze_in_aoi ~ 0 + folder +
    (1 + acc_visd + precrms_visd + robustness_prop_2 | group_id),
  data   = df_rq3,
  family = Beta(link = "logit"),
  prior  = prior_rq3_rel_gaze_in_aoi_red,
  chains = 4, cores = n_cores - 1, iter = 10000, warmup = 5000, 
  sample_prior = "yes",
  seed = 123
)

## Model Comparison ----
loo_full_rel_gaze_in_aoi <- loo(full_rq3_rel_gaze_in_aoi)
loo_red_rel_gaze_in_aoi <- loo(red_rq3_rel_gaze_in_aoi)
loo_compare(loo_full_rel_gaze_in_aoi, loo_red_rel_gaze_in_aoi)

## Model Fit: Posterior Predictive Check ----
# Check whether model is "match to the data"
png(here("exp1", "img", "rq3_rel_gaze_in_aoi_ppc.png"), width = 2480/2, height = 3508/2, res = 200)
pp_check(full_rq3_rel_gaze_in_aoi, ndraws = 100)
dev.off()

## Posterior Distribution ----
post_rq3 <- as_draws_df(full_rq3_rel_gaze_in_aoi)

slope_rq3 <- post_rq3 |>
  transmute(
    `4m_acc_visd`              = `b_folder4m:acc_visd`,
    `6m_acc_visd`              = `b_folder6m:acc_visd`,
    `9m_acc_visd`              = `b_folder9m:acc_visd`,
    `18m_acc_visd`             = `b_folder18m:acc_visd`,
    `adults_acc_visd`          = `b_folderadults:acc_visd`,
    `chimps_acc_visd`          = `b_folderchimps:acc_visd`,
    
    `4m_precrms_visd`          = `b_folder4m:precrms_visd`,
    `6m_precrms_visd`          = `b_folder6m:precrms_visd`,
    `9m_precrms_visd`          = `b_folder9m:precrms_visd`,
    `18m_precrms_visd`         = `b_folder18m:precrms_visd`,
    `adults_precrms_visd`      = `b_folderadults:precrms_visd`,
    `chimps_precrms_visd`      = `b_folderchimps:precrms_visd`,
    
    `4m_robustness_prop_2`     = `b_folder4m:robustness_prop_2`,
    `6m_robustness_prop_2`     = `b_folder6m:robustness_prop_2`,
    `9m_robustness_prop_2`     = `b_folder9m:robustness_prop_2`,
    `18m_robustness_prop_2`    = `b_folder18m:robustness_prop_2`,
    `adults_robustness_prop_2` = `b_folderadults:robustness_prop_2`,
    `chimps_robustness_prop_2` = `b_folderchimps:robustness_prop_2`
  ) |>
  mutate(.draw = row_number()) |>
  pivot_longer(
    cols = -.draw,
    names_to = c("group", "predictor"),
    names_pattern = "^(4m|6m|9m|18m|adults|chimps)_(.*)$",
    values_to = "slope"
  ) |>
  mutate(
    group = factor(group, levels = group_order),
    predictor = factor(predictor, levels = predictor_order)
  )

posterior_plot_rq3_rel_gaze_in_aoi <- ggplot(
  slope_rq3,
  aes(
    x = slope,
    y = factor(group, levels = rev(group_order))
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black") +
  stat_halfeye(
    point_interval = "median_qi",
    .width = c(0, 0.95),
    alpha = 0.65,
    height = 1.05,
    adjust = 1.0,
    fill = "grey70"
  ) +
  facet_wrap(
    ~ predictor,
    ncol = 3,
    scales = "free_x",
    labeller = labeller(predictor = predictor_labels)
  ) +
  scale_y_discrete(labels = group_labels) +
  labs(
    x = "Slope Estimate",
    y = NULL
  ) +
  theme_bw(base_size = 14)

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posterior_3c.png"), width = 2480, height = 3508 / 4, res = 250)
posterior_plot_rq3_rel_gaze_in_aoi
dev.off()

## Posterior Versus Prior Plots ----

# Plot prior and posterior distribution to see how sensitive the results are to the choice of priors
## Accuracy
png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_4m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder4m:acc_visd", "prior_b_folder4m:acc_visd"), facet_label = "4-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_6m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder6m:acc_visd", "prior_b_folder6m:acc_visd"), facet_label = "6-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_9m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder9m:acc_visd", "prior_b_folder9m:acc_visd"), facet_label = "9-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_18m_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder18m:acc_visd", "prior_b_folder18m:acc_visd"), facet_label = "18-Month-Olds, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_adults_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folderadults:acc_visd", "prior_b_folderadults:acc_visd"), facet_label = "Adults, Accuracy")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_chimps_acc.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folderchimps:acc_visd", "prior_b_folderchimps:acc_visd"), facet_label = "Chimpanzees, Accuracy")
dev.off()

## Precision (RMS)
png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_4m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder4m:precrms_visd", "prior_b_folder4m:precrms_visd"), facet_label = "4-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_6m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder6m:precrms_visd", "prior_b_folder6m:precrms_visd"), facet_label = "6-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_9m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder9m:precrms_visd", "prior_b_folder9m:precrms_visd"), facet_label = "9-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_18m_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder18m:precrms_visd", "prior_b_folder18m:precrms_visd"), facet_label = "18-Month-Olds, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_adults_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folderadults:precrms_visd", "prior_b_folderadults:precrms_visd"), facet_label = "Adults, Precision (RMS)")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_chimps_precrms.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folderchimps:precrms_visd", "prior_b_folderchimps:precrms_visd"), facet_label = "Chimpanzees, Precision (RMS)")
dev.off()

## Robustness
png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_4m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder4m:robustness_prop_2", "prior_b_folder4m:robustness_prop_2"), facet_label = "4-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_6m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder6m:robustness_prop_2", "prior_b_folder6m:robustness_prop_2"), facet_label = "6-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_9m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder9m:robustness_prop_2", "prior_b_folder9m:robustness_prop_2"), facet_label = "9-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_18m_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folder18m:robustness_prop_2", "prior_b_folder18m:robustness_prop_2"), facet_label = "18-Month-Olds, Robustness")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_adults_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folderadults:robustness_prop_2", "prior_b_folderadults:robustness_prop_2"), facet_label = "Adults, Robustness")
dev.off()

png(here("exp1", "img", "rq3_rel_gaze_in_aoi_posteriorprior_chimps_robustness.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq3_rel_gaze_in_aoi, pars = c("b_folderchimps:robustness_prop_2", "prior_b_folderchimps:robustness_prop_2"), facet_label = "Chimpanzees, Robustness")
dev.off()


## Descriptives ----
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(rel_gaze_in_aoi = mean(rel_gaze_in_aoi, na.rm = T)) |> 
  as.data.frame() |> 
  group_by(folder) |> 
  summarize(mean_rel_gaze_in_aoi = round(mean(rel_gaze_in_aoi, na.rm = T),2),
            sd_rel_gaze_in_aoi = round(sd(rel_gaze_in_aoi, na.rm = T),2)) |> 
  as.data.frame() |> 
  ungroup() |> 
  slice(3,4,5,2,6,1)

## Paper Plot ----
plot_rq3(df = df_tot, x_var = "acc_visd", y_var = "rel_gaze_in_aoi", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_acc_rellook.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")

plot_rq3(df = df_tot, x_var = "precsd_visd", y_var = "rel_gaze_in_aoi", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_precsd_rellookt.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")

plot_rq3(df = df_tot, x_var = "precrms_visd", y_var = "rel_gaze_in_aoi", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_precrms_rellook.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")

plot_rq3(df = df_tot, x_var = "robustness_prop_2", y_var = "rel_gaze_in_aoi", width = 2480, height = 3508/2, res = 250,
         png_name = "rq3_rob_rellook.png", x_lab = "Robustness\n(in %)", y_lab = "Relative Looking Time\n(in ms)")

# Optionally not separated by group
# plot_rq3_all(df = df_tot, x_var = "acc_visd", y_var = "rel_gaze_in_aoi",
#              png_name = "rq3_all_acc_rellook.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")
# 
# plot_rq3_all(df = df_tot, x_var = "precsd_visd", y_var = "rel_gaze_in_aoi",
#              png_name = "rq3_all_precsd_rellook.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")
# 
# plot_rq3_all(df = df_tot, x_var = "precrms_visd", y_var = "rel_gaze_in_aoi",
#              png_name = "rq3_all_precrms_rellook.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")
# 
# plot_rq3_all(df = df_tot, x_var = "robustness_prop_2", y_var = "rel_gaze_in_aoi",
#              png_name = "rq3_all_rob_rellook.png", x_lab = "Robustness\n(in %)", y_lab = "Relative Looking Time\n(in ms)")


# Explorative Analyses ----------------------------------------------------
# Exploratory Analyses ----------------------------------------------------
library(readxl)

tobii_4m <- read_excel(here("exp1", "doc", "protocol.xlsx"), sheet = 1) |> 
  mutate(
    folder = "4m",
    file = as.character(file),
    tobii_average_calibration_accuracy_degree = as.numeric(tobii_average_calibration_accuracy_degree),
    tobii_average_validation_accuracy_degree = as.numeric(tobii_average_validation_accuracy_degree)
  )

tobii_6m <- read_excel(here("exp1", "doc", "protocol.xlsx"), sheet = 2) |> 
  mutate(
    folder = "6m",
    file = as.character(file),
    tobii_average_calibration_accuracy_degree = as.numeric(tobii_average_calibration_accuracy_degree),
    tobii_average_validation_accuracy_degree = as.numeric(tobii_average_validation_accuracy_degree)
  )

tobii_9m <- read_excel(here("exp1", "doc", "protocol.xlsx"), sheet = 3) |> 
  mutate(
    folder = "9m",
    file = as.character(file),
    tobii_average_calibration_accuracy_degree = as.numeric(tobii_average_calibration_accuracy_degree),
    tobii_average_validation_accuracy_degree = as.numeric(tobii_average_validation_accuracy_degree)
  )

tobii_18m <- read_excel(here("exp1", "doc", "protocol.xlsx"), sheet = 4) |> 
  mutate(
    folder = "18m",
    file = as.character(file),
    tobii_average_calibration_accuracy_degree = as.numeric(tobii_average_calibration_accuracy_degree),
    tobii_average_validation_accuracy_degree = as.numeric(tobii_average_validation_accuracy_degree)
  )

tobii_adults <- read_excel(here("exp1", "doc", "protocol.xlsx"), sheet = 5) |> 
  mutate(
    folder = "adults",
    file = as.character(file),
    tobii_average_calibration_accuracy_degree = as.numeric(tobii_average_calibration_accuracy_degree),
    tobii_average_validation_accuracy_degree = as.numeric(tobii_average_validation_accuracy_degree)
  )

tobii_chimps <- read_excel(here("exp1", "doc", "protocol.xlsx"), sheet = 6) |> 
  mutate(
    folder = "chimps",
    file = as.character(file),
    tobii_average_calibration_accuracy_degree = as.numeric(tobii_average_calibration_accuracy_degree),
    tobii_accuracy_validation = NA
  )

tobii_accuracy <- bind_rows(
  tobii_4m |> select(folder, file, tobii_average_calibration_accuracy_degree, tobii_average_validation_accuracy_degree),
  tobii_6m |> select(folder, file, tobii_average_calibration_accuracy_degree, tobii_average_validation_accuracy_degree),
  tobii_9m |> select(folder, file, tobii_average_calibration_accuracy_degree, tobii_average_validation_accuracy_degree),
  tobii_18m |> select(folder, file, tobii_average_calibration_accuracy_degree, tobii_average_validation_accuracy_degree),
  tobii_adults |> select(folder, file, tobii_average_calibration_accuracy_degree, tobii_average_validation_accuracy_degree),
  tobii_chimps |> select(folder, file, tobii_average_calibration_accuracy_degree, tobii_accuracy_validation)
) |> 
  select(
    folder,
    file,
    tobii_average_calibration_accuracy_degree,
    tobii_average_validation_accuracy_degree
  ) |> 
  rename(
    id = file,
    tobii_accuracy = tobii_average_calibration_accuracy_degree,
    tobii_accuracy_validation = tobii_average_validation_accuracy_degree
  ) |> 
  mutate(group_id = paste(folder, id, sep = "_")) |> 
  select(group_id, folder, id, tobii_accuracy, tobii_accuracy_validation) |>
  mutate(group_id = sub("^chimps_", "", group_id))

calculated_accuracy <- df_tot |> 
  group_by(group_id) |> 
  summarize(calculated_accuracy = mean(acc_visd, na.rm = T)) |> 
  as.data.frame()

plot_dat <- tobii_accuracy |>
  left_join(calculated_accuracy, by = "group_id") |>
  mutate(
    folder_label = recode(
      folder,
      "4m" = "4-Month-Olds",
      "6m" = "6-Month-Olds",
      "9m" = "9-Month-Olds",
      "18m" = "18-Month-Olds",
      "adults" = "Adults",
      "chimps" = "Chimpanzees"
    ),
    folder_label = factor(
      folder_label,
      levels = c(
        "4-Month-Olds",
        "6-Month-Olds",
        "9-Month-Olds",
        "18-Month-Olds",
        "Adults",
        "Chimpanzees"
      )
    )
  )

panel_limits <- plot_dat |>
  group_by(folder_label) |>
  summarize(panel_max = max(c(tobii_accuracy_validation, calculated_accuracy), na.rm = TRUE)) |>
  filter(folder_label != "Chimpanzees") |> 
  # summarize(panel_max = max(c(tobii_accuracy, calculated_accuracy), na.rm = TRUE)) |>
  ungroup()

plot_dat <- plot_dat |>
  left_join(panel_limits, by = "folder_label")

lm_coefs <- plot_dat |>
  filter(folder != "chimps") |>
  group_by(folder_label) |>
  summarize(
    intercept = coef(lm(calculated_accuracy ~ tobii_accuracy_validation))[1],
    slope = coef(lm(calculated_accuracy ~ tobii_accuracy_validation))[2]
  )
# summarize(
#   intercept = coef(lm(calculated_accuracy ~ tobii_accuracy))[1],
#   slope = coef(lm(calculated_accuracy ~ tobii_accuracy))[2]
# )

#png(here("exp1", "img", "explorative_tobii_versus_own.png"), width = 2480, height = 3508/2.5, res = 280)
png(here("exp1", "img", "explorative_tobiivalidation_versus_own.png"), width = 2480, height = 3508/2.5, res = 280)
# ggplot(plot_dat, aes(x = tobii_accuracy, y = calculated_accuracy)) +
ggplot(plot_dat |> filter(folder != "chimps"), aes(x = tobii_accuracy, y = calculated_accuracy)) +
  geom_blank(aes(x = 0, y = 0)) +
  geom_blank(aes(x = panel_max, y = panel_max)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linewidth = 0.7) +
  geom_abline(
    data = lm_coefs,
    aes(intercept = intercept, slope = slope),
    color = "darkred",
    linewidth = 0.9
  ) +
  geom_point(size = 2, alpha = 0.9) +
  facet_wrap(~ folder_label, ncol = 3, scales = "free") +
  labs(
    x = "Tobii Reported Accuracy (Validation)\n(in visual degrees)",
    y = "Calculated Accuracy\n(in visual degrees)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(lineheight = 0.9),
    axis.title.y = element_text(lineheight = 0.9)
  )
dev.off()

cor.test(plot_dat$tobii_accuracy, plot_dat$calculated_accuracy)
cor.test(plot_dat |> filter(folder == "4m") |> pull(tobii_accuracy), 
         plot_dat |> filter(folder == "4m") |> pull(calculated_accuracy))
cor.test(plot_dat |> filter(folder == "6m") |> pull(tobii_accuracy), 
         plot_dat |> filter(folder == "6m") |> pull(calculated_accuracy))
cor.test(plot_dat |> filter(folder == "9m") |> pull(tobii_accuracy), 
         plot_dat |> filter(folder == "9m") |> pull(calculated_accuracy))
cor.test(plot_dat |> filter(folder == "18m") |> pull(tobii_accuracy), 
         plot_dat |> filter(folder == "18m") |> pull(calculated_accuracy))
cor.test(plot_dat |> filter(folder == "adults") |> pull(tobii_accuracy), 
         plot_dat |> filter(folder == "adults") |> pull(calculated_accuracy))
cor.test(plot_dat |> filter(folder == "chimps") |> pull(tobii_accuracy), 
         plot_dat |> filter(folder == "chimps") |> pull(calculated_accuracy))

cor.test(plot_dat$tobii_accuracy_validation, plot_dat$calculated_accuracy)
cor.test(plot_dat |> filter(folder == "4m") |> pull(tobii_accuracy_validation), 
         plot_dat |> filter(folder == "4m") |> pull(calculated_accuracy))
cor.test(plot_dat |> filter(folder == "6m") |> pull(tobii_accuracy_validation), 
         plot_dat |> filter(folder == "6m") |> pull(calculated_accuracy))
cor.test(plot_dat |> filter(folder == "9m") |> pull(tobii_accuracy_validation), 
         plot_dat |> filter(folder == "9m") |> pull(calculated_accuracy))
cor.test(plot_dat |> filter(folder == "18m") |> pull(tobii_accuracy_validation), 
         plot_dat |> filter(folder == "18m") |> pull(calculated_accuracy))
cor.test(plot_dat |> filter(folder == "adults") |> pull(tobii_accuracy_validation), 
         plot_dat |> filter(folder == "adults") |> pull(calculated_accuracy))


