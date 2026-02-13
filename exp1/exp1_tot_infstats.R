# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(brms)
library(posterior)
library(marginaleffects)

# Load Functions ----------------------------------------------------------
source(here("exp1", "R", "descriptives.R"))
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
  acc_visd ~ 0 + folder + 0 + position + (1 + position | group_id),
  data   = df_tot,
  family = hurdle_gamma(link = "log"),
  prior  = prior_rq1_acc,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  # control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 123,
  # save_pars = save_pars(all = TRUE)
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
  acc_visd ~ 0 + position + (1 + position | group_id),
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

## Marginal Effects ----
# avg_predictions(full_rq1_acc, by="folder")

## Visualization ----

# Extract all posterior samples
post_samples_rq1_acc <- as_draws_df(full_rq1_acc)

# Transform all condition parameters to probabilities
posterior_probs_rq1_acc <- post_samples_rq1_acc |> 
  select(b_folder4m, b_folder6m, b_folder9m, b_folder18m, b_folderadults, b_folderchimps) |> 
  mutate(b_folder4m = plogis(b_folder4m), b_folder6m = plogis(b_folder6m), b_folder9m = plogis(b_folder9m),
         b_folder18m = plogis(b_folder18m), b_folderadults = plogis(b_folderadults), b_folderchimps = plogis(b_folderchimps))

# Plot posterior distributions of each condition
png(here("exp1", "img", "rq1_acc_posterior.png"), width = 2480, height = 3508/6, res = 300)
posterior_probs_rq1_acc |>
  pivot_longer(cols = c(b_folder4m, b_folder6m, b_folder9m, b_folder18m, b_folderadults, b_folderchimps),
               names_to = "param", values_to = "value") |>
  mutate(param = recode(
    param,
    b_folder4m     = " 4-Month-Olds",
    b_folder6m     = " 6-Month-Olds",
    b_folder9m     = " 9-Month-Olds",
    b_folder18m    = "18-Month-Olds",
    b_folderadults = "Adults",
    b_folderchimps = "Chimpanzees")) |> 
  mutate(Group = param) |> 
  ggplot(aes(x = value, fill = Group)) +
  geom_histogram(bins = 80, position = "identity", alpha = 0.35) +
  theme_minimal()
dev.off()

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

png(here("exp1", "img", "rq1_acc_posteriorprior_18m.png"), width = 2480/2, height = 3508/6, res = 300)
plot_prior_vs_poster(full_rq1_acc, pars = c("b_folder18m", "prior_b_folder18m"), facet_label = "18-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_acc_posteriorprior_adults.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_acc, pars = c("b_folderadults", "prior_b_folderadults"), facet_label = "Adults")
dev.off()

## Inference ----

# Descriptives
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(acc_visd = mean(acc_visd, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_acc_visd = mean(acc_visd, na.rm = T),
            sd_acc_visd = sd(acc_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_acc_visd)

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
      labels = c("4M", "6M", "9M", "18M", "Adults", "Chimpanzees")
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
    alpha = 0.75,
    size = 2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high),
    width = 0.12,
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean),
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Group",
    y = "Precision (RMS)\nin visual degrees"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp1", "img", "rq1_acc_paperplot.png"), width = 2480/2, height = 3508/4, res = 200)
p_acc
dev.off()


# RQ1 (Precision RMS) -----------------------------------------------------

## Define Priors ----
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
  precrms_visd ~ 0 + folder + 0 + position + (1 + position | group_id),
  data   = df_tot,
  family = Gamma(link="log")
  prior  = prior_rq1_precrms,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  sample_prior="yes",
  # control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 123,
  # save_pars = save_pars(all = TRUE)
)
t1 <- proc.time()
proc_time_rq1_precrms <- t1 - t0
rm(t0, t1)

## Visualization ----

# Extract all posterior samples
post_samples_rq1_precrms <- as_draws_df(full_rq1_precrms)

# Transform all condition parameters to probabilities
posterior_probs_rq1_precrms <- post_samples_rq1_precrms |> 
  select(b_folder4m, b_folder6m, b_folder9m, b_folder18m, b_folderadults, b_folderchimps) |> 
  mutate(b_folder4m = plogis(b_folder4m), b_folder6m = plogis(b_folder6m), b_folder9m = plogis(b_folder9m),
         b_folder18m = plogis(b_folder18m), b_folderadults = plogis(b_folderadults), b_folderchimps = plogis(b_folderchimps))

# Plot posterior distributions of each condition
png(here("exp1", "img", "rq1_precrms_posterior.png"), width = 2480, height = 3508/6, res = 300)
posterior_probs_rq1_precrms |>
  pivot_longer(cols = c(b_folder4m, b_folder6m, b_folder9m, b_folder18m, b_folderadults, b_folderchimps),
               names_to = "param", values_to = "value") |>
  mutate(param = recode(
      param,
      b_folder4m     = " 4-Month-Olds",
      b_folder6m     = " 6-Month-Olds",
      b_folder9m     = " 9-Month-Olds",
      b_folder18m    = "18-Month-Olds",
      b_folderadults = "Adults",
      b_folderchimps = "Chimpanzees")) |> 
  mutate(Group = param) |> 
  ggplot(aes(x = value, fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.35) +
  theme_minimal()
dev.off()

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

png(here("exp1", "img", "rq1_precrms_posteriorprior_18m.png"), width = 2480/2, height = 3508/6, res = 300)
plot_prior_vs_poster(full_rq1_precrms, pars = c("b_folder18m", "prior_b_folder18m"), facet_label = "18-Month-Olds")
dev.off()

png(here("exp1", "img", "rq1_precrms_posteriorprior_adults.png"), width = 2480/2, height = 3508/3, res = 300)
plot_prior_vs_poster(full_rq1_precrms, pars = c("b_folderadults", "prior_b_folderadults"), facet_label = "Adults")
dev.off()

## Inference ----

# Descriptives
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(precrms_visd = mean(precrms_visd, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_precrms_visd = mean(precrms_visd, na.rm = T),
            sd_precrms_visd = sd(precrms_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_precrms_visd)

# Calculate posterior probability that precision (RMS) in A is higher than in B
# todo: adjust order + add %
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
      labels = c("4M", "6M", "9M", "18M", "Adults", "Chimpanzees")
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
    alpha = 0.75,
    size = 2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high),
    width = 0.12,
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean),
    size = 3,
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

## Define Priors ----
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
  precsd_visd ~ 0 + folder + 0 + position + (1 + position | group_id),
  data   = df_tot,
  family = Gamma(link="log"),
  prior  = prior_rq1_precsd,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)
t1 <- proc.time()
proc_time_rq1_precsd <- t1 - t0
rm(t0, t1)

## Inference ---- 
# Descriptives
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(precsd_visd = mean(precsd_visd, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_precsd_visd = mean(precsd_visd, na.rm = T),
            sd_precsd_visd = sd(precsd_visd, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_precsd_visd)

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
# robustness_prop = recorded_ms / trial_length_ms
# Therefore: 0 < robustness_prop < 1

# Model choice:
# We use a Beta likelihood with a logit link.
# With Beta(link="logit") and 0 + folder, folder coefficients are on the logit-mean scale:
#   mu = inv_logit( b_folder* + ... )
# Note: Beta() requires outcomes strictly in (0, 1). If exact 0 or 1 occur,
# clamp slightly or use zero_one_inflated_beta().

## Define Priors ----
priors_rq1_rob <- c(
  
  # Robustness (not preregistered; therefore weakly informative priors)
  # Prior choice:
  # We expect robustness to be around 0.5 on average, but with wide uncertainty.
  # Centering at 0.5 (logit scale), with a large SD to avoid strong assumptions.
  # Even with a wide SD on the logit scale, mu remains constrained to (0, 1).
  
  # Group-level means (logit-mean scale)
  prior(normal(qlogis(0.5), 1.5), class = "b", coef = "folder4m"),
  prior(normal(qlogis(0.5), 1.5), class = "b", coef = "folder6m"),
  prior(normal(qlogis(0.5), 1.5), class = "b", coef = "folder9m"),
  prior(normal(qlogis(0.5), 1.5), class = "b", coef = "folder18m"),
  prior(normal(qlogis(0.5), 1.5), class = "b", coef = "folderadults"),
  prior(normal(qlogis(0.5), 1.5), class = "b", coef = "folderchimps"),
  
  # Dispersion (phi):
  # phi controls how concentrated the Beta distribution is around mu.
  # Weakly informative prior, avoids extreme concentration by default.
  # Beta precision parameter (higher = less variance around mu)
  prior(exponential(1), class = "phi"),
  
  # Position effects (weakly informative; small-to-moderate differences expected)
  prior(normal(0, 0.56), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.56), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.56), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.56), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.56), class = "b", coef = "positiontop"),
  prior(normal(0, 0.56), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.56), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(1), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

## Full Model ----
t0 <- proc.time()

full_rq1_rob <- brm(
  robustness_prop ~ 0 + folder + 0 + position + (1 + position | group_id),
  data   = df_tot,
  family = Beta(link = "logit"),
  prior  = priors_rq1_rob,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)
t1 <- proc.time()
proc_time_rq1_rob <- t1 - t0
rm(t0, t1)

## Visualization ----

# Extract all posterior samples
post_samples_rq1_rob <- as_draws_df(full_rq1_rob)

# Transform all condition parameters to probabilities
posterior_probs_rq1_rob <- post_samples_rq1_rob |> 
  select(b_folder4m, b_folder6m, b_folder9m, b_folder18m, b_folderadults, b_folderchimps) |> 
  mutate(b_folder4m = plogis(b_folder4m), b_folder6m = plogis(b_folder6m), b_folder9m = plogis(b_folder9m),
         b_folder18m = plogis(b_folder18m), b_folderadults = plogis(b_folderadults), b_folderchimps = plogis(b_folderchimps))

# Plot posterior distributions of each condition
posterior_probs_rq1_rob |>
  pivot_longer(cols = c(b_folder4m, b_folder6m, b_folder9m, b_folder18m, b_folderadults, b_folderchimps),
               names_to = "param", values_to = "value") |>
  ggplot(aes(x = value, fill = param)) +
  geom_histogram(bins = 80, position = "identity", alpha = 0.35) +
  theme_minimal()

## Inference ----

# Descriptives
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(robustness_ms = mean(robustness_ms, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_robustness_ms = mean(robustness_ms, na.rm = T),
            sd_robustness_ms = sd(robustness_ms, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_robustness_ms)

# Calculate posterior probability that roburacy in A is higher than in B
mean(post_samples_rq1_rob$b_folderadults > post_samples_rq1_rob$b_folder18m) # xx% # 
# "The posterior probability that chimpanzees have a better robustness than 4-month-old infants, is xx%."
mean(post_samples_rq1_rob$b_folder18m < post_samples_rq1_rob$b_folder9m) # xx%
mean(post_samples_rq1_rob$b_folder9m > post_samples_rq1_rob$b_folderchimps) # xx%
mean(post_samples_rq1_rob$b_folderchimps > post_samples_rq1_rob$b_folder6m) # xx%
mean(post_samples_rq1_rob$b_folder6m > post_samples_rq1_rob$b_folder4m) # xx%

## Paper Plot ----
# Aggregate to subject level (mean over time/trials)
df_subj <- df_tot |>
  # filter(!(group_id %in% c("alex", "daza"))) |> 
  filter(!is.na(folder), !is.na(group_id), !is.na(time), !is.na(robustness_ms)) |>
  group_by(folder, group_id) |>
  summarise(
    robustness_ms = mean(robustness_ms, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Group = factor(
      folder,
      levels = c("4m", "6m", "9m", "18m", "adults", "chimps"),
      labels = c("4M", "6M", "9M", "18M", "Adults", "Chimpanzees")
    )
  )

# Mean + 95% CI across subjects (per group)
sum_df <- df_subj |>  
  group_by(Group) |>
  summarise(
    n = n(),
    mean = mean(robustness_ms),
    sd = sd(robustness_ms),
    se = sd / sqrt(n),
    tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    ci_low = mean - tcrit * se,
    ci_high = mean + tcrit * se,
    .groups = "drop"
  )

p_robust <- ggplot(df_subj, aes(x = Group, y = robustness_ms)) +
  geom_violin(trim = FALSE, alpha = 0.25) +
  geom_point(
    aes(color = Group),
    position = position_jitter(width = 0.12, height = 0),
    alpha = 0.75,
    size = 2,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = sum_df,
    aes(x = Group, ymin = ci_low, ymax = ci_high),
    width = 0.12,
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = sum_df,
    aes(x = Group, y = mean),
    size = 3,
    inherit.aes = FALSE
  ) +
  labs(
    x = "Group",
    y = "Robustness\nin ms"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

p_robust


# RQ2 (Accuracy) ----------------------------------------------------------
# RQ2:  (How) does eye-tracking data quality change over time, 
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
priors_rq2 <- c(
  # Group parameters (log-mean scale)
  prior(normal(0.50, 0.48), class = "b", coef = "folder4m"),
  prior(normal(0.50, 0.48), class = "b", coef = "folder6m"),
  prior(normal(0.24, 0.09), class = "b", coef = "folder9m"),
  prior(normal(0.00, 0.09), class = "b", coef = "folder18m"),
  prior(normal(-0.41, 0.08), class = "b", coef = "folderadults"),
  prior(normal(0.75, 0.24), class = "b", coef = "folderchimps"),

  # Gamma shape (very wide; centered on preregistered median of 2.39 value)
  prior(lognormal(log(2.39), 1), class = "shape"),
  
  # Interaction: folder × time_1
  prior(normal(0, 0.64), class = "b", coef = "folder4m:time_1"),
  prior(normal(0, 0.64), class = "b", coef = "folder6m:time_1"),
  prior(normal(0, 0.64), class = "b", coef = "folder9m:time_1"),
  prior(normal(0, 0.64), class = "b", coef = "folder18m:time_1"),
  prior(normal(0, 0.64), class = "b", coef = "folderadults:time_1"),
  prior(normal(0, 0.64), class = "b", coef = "folderchimps:time_1"),
  
  # Position (weakly informative)
  prior(normal(0, 0.56), class = "b", coef = "positionbot_left"),
  prior(normal(0, 0.56), class = "b", coef = "positionbot_right"),
  prior(normal(0, 0.56), class = "b", coef = "positionbottom"),
  # prior(normal(0, 0.56), class = "b", coef = "positioncenter"), # center is the reference level
  prior(normal(0, 0.56), class = "b", coef = "positiontop"),
  prior(normal(0, 0.56), class = "b", coef = "positiontop_left"),
  prior(normal(0, 0.56), class = "b", coef = "positiontop_right"),
  
  # Random effects regularization
  prior(exponential(0.25), class = "sd"), # enforces positivity but allows inter-individual heterogeneity
  prior(lkj(2), class = "cor") # mildly favors correlations near zero and reduces the probability of extreme ±1 correlations unless strongly 
  # supported, improving computational stability in random-slope models
)

## Full model ----
full_rq2 <- brm(
  acc_visd ~ 0 + folder + 0 + folder:time_1 + 0 + position +
    (1 + position + time_1 | group_id),
  data   = df_tot,
  family = hurdle_gamma(link = "log"),
  prior  = priors_rq2,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 123
  # save_pars = save_pars(all = TRUE)
)

# Reduced model
red_rq2 <- brm(
  acc_visd ~ 0 + folder + 0 + position +
    (1 + position + time_1 | group_id),
  data   = df_tot,
  family = hurdle_gamma(link = "log"),
  prior  = priors_rq2,  # or define a priors_red_rq2 without the folder:time priors
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  seed = 123
)

## Paper Plot ----
png(here("exp1", "img", "rq2_all.png"), width = 2480, height = 3508, res = 300)

lvl <- c("4m", "6m", "9m", "18m", "adults", "chimps")

facet_labs <- c(
  "4m"     = "4-Month-Olds",
  "6m"     = "6-Month-Olds",
  "9m"     = "9-Month-Olds",
  "18m"    = "18-Month-Olds",
  "adults" = "Adults",
  "chimps" = "Chimpanzees"
)

# Build a clean plotting data frame
df_plot <- df_tot |>
  mutate(folder = trimws(as.character(folder))) |>
  filter(!(folder == "chimps" & time_1 > 8)) |>
  filter(!is.na(time_1), !is.na(acc_visd)) |>
  mutate(folder = factor(folder, levels = lvl))

# Summarise means + CI per time point
gg_all_rq2 <- df_plot |>
  group_by(folder, time_1) |>
  summarise(
    n = sum(!is.na(acc_visd)),
    mean_acc = mean(acc_visd, na.rm = TRUE),
    sd_acc   = sd(acc_visd, na.rm = TRUE),
    se_acc   = sd_acc / sqrt(n),
    tcrit    = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    ci_low   = mean_acc - tcrit * se_acc,
    ci_high  = mean_acc + tcrit * se_acc,
    .groups = "drop"
  )

# Dummy limits for facet-specific x and y ranges
dummy_limits <- bind_rows(
  tibble(folder = c("4m","6m","9m","18m","adults"), x_min = 0, x_max = 80, y_min = 0, y_max = 2),
  tibble(folder = "chimps",                   x_min = 0, x_max = 8,  y_min = 3, y_max = 5)
) |>
  mutate(folder = factor(folder, levels = lvl)) |>
  pivot_longer(
    cols = c(x_min, x_max, y_min, y_max),
    names_to = c(".value", "which"),
    names_pattern = "([xy])_(min|max)"
  ) |>
  transmute(folder, time_1 = x, mean_acc = y)

# Plot
p_all_rq2 <- ggplot(gg_all_rq2, aes(x = time_1, y = mean_acc, group = 1)) +
  geom_blank(data = dummy_limits, aes(x = time_1, y = mean_acc)) +
  geom_errorbar(
    data = gg_all_rq2 |> filter(!is.na(ci_low), !is.na(ci_high)),
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.2,
    linewidth = 0.4
  ) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  facet_wrap(
    ~ folder, ncol = 2, nrow = 3, scales = "free",
    labeller = as_labeller(facet_labs)
  ) +
  labs(
    y = "Accuracy\nin visual degrees",
    x = "Time\n(trials in humans; sessions in apes)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

p_all_rq2
dev.off()






