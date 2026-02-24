# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(brms)
library(posterior)
library(marginaleffects)
library(readxl)

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
  acc_visd ~ 0 + folder + 0 + position + (1 + position | group_id),
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
# RQ1: Marginal means (response scale) + pairwise contrasts ----------------
rq1_acc_means_contr    <- get_rq1_marginals(full_rq1_acc, "Accuracy")



avg_acc <- marginaleffects::avg_predictions(
  full_rq1_acc,
  by = "folder",
  type = "response",
  re_formula = NA
)

cmp_acc <- marginaleffects::comparisons(
  full_rq1_acc,
  variables = "folder",
  type = "response",
  re_formula = NA
)

## Visualization ----

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
    y = "Accuracy\nin visual degrees"
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

## Marginal Effects ----
# avg_predictions(full_rq1_precrms, by="folder")

## Visualization ----
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
  sample_prior = "yes",
  family = Gamma(link="log"),
  prior  = prior_rq1_precsd,
  chains = 4, cores = n_cores - 1, iter = 4000, warmup = 2000,
  seed = 123
)
t1 <- proc.time()
proc_time_rq1_precsd <- t1 - t0
rm(t0, t1)

## Marginal Effects ----

## Visualization ----
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
      labels = c("4M", "6M", "9M", "18M", "Adults", "Chimpanzees")
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

## Define Priors ----
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

## Visualization ----

## Marginal Effects ----
# avg_predictions(full_rq1_precsd, by="folder")

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

## Inference ----

# Descriptives
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
      labels = c("4M", "6M", "9M", "18M", "Adults", "Chimpanzees")
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
    y = "Robustness\nin %"
  ) +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank())

png(here("exp1", "img", "rq1_rob_paperplot.png"), width = 2480/2, height = 3508/4, res = 200)
p_robust
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
  
  # Gamma shape (very wide; centered on preregistered median of 2.39 value)
  prior(lognormal(log(2.39), 1), class = "shape"),
  
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
  
  # Gamma shape (very wide; centered on preregistered median of 2.39 value)
  prior(lognormal(log(2.39), 1), class = "shape"),
  
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

## Full model ----
full_rq2_acc_hum <- brm(
  acc_visd ~ 0 + folder + 0 + folder:time_3 + 0 + position +
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

## Inference ----

# Descriptives
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
  prior(normal(-0.11, 0.16), class = "b", coef = "folderchimps"),
  
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

## Full models ----

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

## Inference ----

# Descriptives
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
  prior(normal(-0.45, 0.14), class = "b", coef = "folderchimps"),
  
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

priors_rq2_precsd_chi <- c(
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

## Full models ----

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

## Inference ----

# Descriptives
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

# RQ3 (Fixation Duration) -------------------------------------------------

## Inference ----

# Descriptives
df_tot |> 
  group_by(folder, group_id) |> 
  summarize(mean_fixation_duration = mean(mean_fixation_duration, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_mean_fixation_duration = mean(mean_fixation_duration, na.rm = T),
            sd_mean_fixation_duration = sd(mean_fixation_duration, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_mean_fixation_duration)

## Paper Plot ----
plot_rq3(df = df_tot, x_var = "acc_visd", y_var = "mean_fixation_duration",
  png_name = "rq3_acc_fixdur.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")

plot_rq3(df = df_tot, x_var = "precsd_visd", y_var = "mean_fixation_duration",
  png_name = "rq3_precsd_fixdur.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")

plot_rq3(df = df_tot, x_var = "precrms_visd", y_var = "mean_fixation_duration",
         png_name = "rq3_precrms_fixdur.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")

plot_rq3(df = df_tot, x_var = "robustness_prop_2", y_var = "mean_fixation_duration",
  png_name = "rq3_rob_fixdur.png", x_lab = "Robustness\n(in %)", y_lab = "Mean Fixation Duration\n(in ms)")

# Optionally not separated by group
plot_rq3_all(df = df_tot, x_var = "acc_visd", y_var = "mean_fixation_duration",
  png_name = "rq3_all_acc_fixdur.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")

plot_rq3_all(df = df_tot, x_var = "precsd_visd", y_var = "mean_fixation_duration",
             png_name = "rq3_all_precsd_fixdur.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")

plot_rq3_all(df = df_tot, x_var = "precrms_visd", y_var = "mean_fixation_duration",
             png_name = "rq3_all_precrms_fixdur.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Mean Fixation Duration\n(in ms)")

plot_rq3_all(df = df_tot, x_var = "robustness_prop_2", y_var = "mean_fixation_duration",
             png_name = "rq3_all_rob_fixdur.png", x_lab = "Robustness\n(in %)", y_lab = "Mean Fixation Duration\n(in ms)")

# RQ3 (Fixation Number) ---------------------------------------------------

## Inference ----

# Descriptives
df_tot |>
  group_by(folder, group_id) |> 
  summarize(mean_fixation_number = mean(mean_fixation_number, na.rm = T)) |> 
  group_by(folder) |> 
  summarize(mean_mean_fixation_number = mean(mean_fixation_number, na.rm = T),
            sd_mean_fixation_number = sd(mean_fixation_number, na.rm = T)) |> 
  ungroup() |> 
  arrange(mean_mean_fixation_number)

## Paper Plot ----
plot_rq3(df = df_tot, x_var = "acc_visd", y_var = "mean_fixation_number",
         png_name = "rq3_acc_fixnum.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Mean Fixation Number")

plot_rq3(df = df_tot, x_var = "precsd_visd", y_var = "mean_fixation_number",
         png_name = "rq3_precsd_fixnum.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Mean Fixation Number")

plot_rq3(df = df_tot, x_var = "precrms_visd", y_var = "mean_fixation_number",
         png_name = "rq3_precrms_fixnum.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Mean Fixation Number")

plot_rq3(df = df_tot, x_var = "robustness_prop_2", y_var = "mean_fixation_number",
         png_name = "rq3_rob_fixnum.png", x_lab = "Robustness\n(in %)", y_lab = "Mean Fixation Number")

# Optionally not separated by group
plot_rq3_all(df = df_tot, x_var = "acc_visd", y_var = "mean_fixation_number",
             png_name = "rq3_all_acc_fixnum.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Mean Fixation Number")

plot_rq3_all(df = df_tot, x_var = "precsd_visd", y_var = "mean_fixation_number",
             png_name = "rq3_all_precsd_fixnum.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Mean Fixation Number")

plot_rq3_all(df = df_tot, x_var = "precrms_visd", y_var = "mean_fixation_number",
             png_name = "rq3_all_precrms_fixnum.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Mean Fixation Number")

plot_rq3_all(df = df_tot, x_var = "robustness_prop_2", y_var = "mean_fixation_number",
             png_name = "rq3_all_rob_fixnum.png", x_lab = "Robustness\n(in %)", y_lab = "Mean Fixation Number")

# RQ3 (Latencies) ---------------------------------------------------------

## Inference ----

# Descriptives
df_tot |>
  group_by(folder, group_id) |>
  summarize(latencies = mean(latencies, na.rm = T)) |>
  group_by(folder) |>
  summarize(mean_latencies = mean(latencies, na.rm = T),
            sd_latencies = sd(latencies, na.rm = T)) |>
  ungroup() |>
  arrange(mean_latencies)

## Paper Plot ----
plot_rq3(df = df_tot, x_var = "acc_visd", y_var = "latencies",
         png_name = "rq3_acc_lat.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Latencies\n(in ms)")

plot_rq3(df = df_tot, x_var = "precsd_visd", y_var = "latencies",
         png_name = "rq3_precsd_lat.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Latencies\n(in ms)")

plot_rq3(df = df_tot, x_var = "precrms_visd", y_var = "latencies",
         png_name = "rq3_precrms_lat.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Latencies\n(in ms)")

plot_rq3(df = df_tot, x_var = "robustness_prop_2", y_var = "latencies",
         png_name = "rq3_rob_lat.png", x_lab = "Robustness\n(in %)", y_lab = "Latencies\n(in ms)")

# Optionally not separated by group
plot_rq3_all(df = df_tot, x_var = "acc_visd", y_var = "latencies",
             png_name = "rq3_all_acc_lat.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Latencies\n(in ms)")

plot_rq3_all(df = df_tot, x_var = "precsd_visd", y_var = "latencies",
             png_name = "rq3_all_precsd_lat.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Latencies\n(in ms)")

plot_rq3_all(df = df_tot, x_var = "precrms_visd", y_var = "latencies",
             png_name = "rq3_all_precrms_lat.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Latencies\n(in ms)")

plot_rq3_all(df = df_tot, x_var = "robustness_prop_2", y_var = "latencies",
             png_name = "rq3_all_rob_lat.png", x_lab = "Robustness\n(in %)", y_lab = "Latencies\n(in ms)")

# RQ3 (Relative Looking Time) ---------------------------------------------

## Inference ----
# Descriptives
df_tot |>
  group_by(folder, group_id) |>
  summarize(rel_gaze_in_aoi = mean(rel_gaze_in_aoi, na.rm = T)) |>
  group_by(folder) |>
  summarize(mean_rel_gaze_in_aoi = mean(rel_gaze_in_aoi, na.rm = T),
            sd_rel_gaze_in_aoi = sd(rel_gaze_in_aoi, na.rm = T)) |>
  ungroup() |>
  arrange(mean_rel_gaze_in_aoi)

## Paper Plot ----
plot_rq3(df = df_tot, x_var = "acc_visd", y_var = "rel_gaze_in_aoi",
         png_name = "rq3_acc_rellook.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")

plot_rq3(df = df_tot, x_var = "precsd_visd", y_var = "rel_gaze_in_aoi",
         png_name = "rq3_precsd_rellookt.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")

plot_rq3(df = df_tot, x_var = "precrms_visd", y_var = "rel_gaze_in_aoi",
         png_name = "rq3_precrms_rellook.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")

plot_rq3(df = df_tot, x_var = "robustness_prop_2", y_var = "rel_gaze_in_aoi",
         png_name = "rq3_rob_rellook.png", x_lab = "Robustness\n(in %)", y_lab = "Relative Looking Time\n(in ms)")

# Optionally not separated by group
plot_rq3_all(df = df_tot, x_var = "acc_visd", y_var = "rel_gaze_in_aoi",
             png_name = "rq3_all_acc_rellook.png", x_lab = "Accuracy\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")

plot_rq3_all(df = df_tot, x_var = "precsd_visd", y_var = "rel_gaze_in_aoi",
             png_name = "rq3_all_precsd_rellook.png", x_lab = "Precision (SD)\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")

plot_rq3_all(df = df_tot, x_var = "precrms_visd", y_var = "rel_gaze_in_aoi",
             png_name = "rq3_all_precrms_rellook.png", x_lab = "Precision (RMS)\n(in visual degrees)", y_lab = "Relative Looking Time\n(in ms)")

plot_rq3_all(df = df_tot, x_var = "robustness_prop_2", y_var = "rel_gaze_in_aoi",
             png_name = "rq3_all_rob_rellook.png", x_lab = "Robustness\n(in %)", y_lab = "Relative Looking Time\n(in ms)")