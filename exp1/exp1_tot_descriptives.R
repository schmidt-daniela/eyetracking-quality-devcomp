# open todos
# descriptives + plots across time
# remove trials with connection losts
# robustness

# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(Hmisc)
library(ggtext)

# Functions ---------------------------------------------------------------
source(here("exp1", "R", "descriptives.R"))

# Read Data ---------------------------------------------------------------
## 2p ----
filenames <- list.files(path = here("data", "preproc_included_2", "chimps_2p"))
df_tot_chimps_2p <- data.frame()
for(i in 1:length(filenames)){
  filename <- filenames[i]
  
  df_temp <- read.table(here("data", "preproc_included_2", "chimps_2p", filename), header = T, sep = "\t") |> 
    mutate(age_group = "apes (2-point ape calibration)")
  
  df_temp_etoutcome <- read.table(here("data", "preproc_included_2", "chimps_2p", filename), header = T, sep = "\t") |> 
    mutate(age_group = "apes (2-point ape calibration)")
  
  df_tot_chimps_2p <- df_tot_chimps_2p |> 
    bind_rows(df_temp)
  
  rm(df_temp)
}
df_tot_chimps_2p <- df_tot_chimps_2p |> distinct()
df_tot_human <- data.frame()
for(i in c("4","6","9","18","adult")){
  for(j in c(1:32)){
    
    age_group <- i
    filenames <- list.files(path = here("data", "preproc_included_2", age_group))
    n <- j
    filename <- filenames[n]
    
    df_temp <- read.table(here("data", "preproc_included_2", age_group, filename), header = T, sep = "\t") |> 
      mutate(age_group = age_group)
    
    df_tot_human <- df_tot_human |> 
      bind_rows(df_temp)
    
    rm(df_temp)
  }
}

df_tot <- df_tot_human |> bind_rows(df_tot_chimps)

acc_tot <- df_tot_human  |> 
  mutate(session = NA, date = NA) |>  
  select(filename, accuracy, acc_visd, stimulus, position, trial, age_group) |> 
  bind_rows(df_tot_chimps) |> 
  mutate(position = str_replace_all(position, "center_center", "center")) |> 
  mutate(position = str_replace_all(position, "centercenter", "center")) |> 
  mutate(position = str_replace_all(position, "top_left", "topleft")) |> 
  mutate(position = str_replace_all(position, "top_right", "topright")) |> 
  mutate(position = str_replace_all(position, "bot_left", "botleft")) |> 
  mutate(position = str_replace_all(position, "bot_right", "botright")) |> 
  mutate(position = str_replace_all(position, "up", "top")) |> 
  mutate(position = str_replace_all(position, "down", "bottom"))

precrms_tot <- df_tot_human |> 
  mutate(session = NA, date = NA) |>  
  select(filename, precrms, precrms_visd, stimulus, position, trial, age_group) |> 
  bind_rows(df_tot_chimps) |> 
  mutate(position = str_replace_all(position, "center_center", "center")) |> 
  mutate(position = str_replace_all(position, "centercenter", "center")) |> 
  mutate(position = str_replace_all(position, "top_left", "topleft")) |> 
  mutate(position = str_replace_all(position, "top_right", "topright")) |> 
  mutate(position = str_replace_all(position, "bot_left", "botleft")) |> 
  mutate(position = str_replace_all(position, "bot_right", "botright")) |> 
  mutate(position = str_replace_all(position, "up", "top")) |> 
  mutate(position = str_replace_all(position, "down", "bottom"))

## dplyr::summarize Accuracy ----
acc_mean_sd <- mean_sd(acc_tot, c("age_group", "filename"), "acc_visd") 
acc_mean_sd_2 <- mean_sd(acc_tot, c("age_group", "filename", "position"), "acc_visd") 
acc_mean_sd_2 <- acc_mean_sd_2 |> 
  group_by(age_group, position) |> 
  dplyr::summarize(sd_acc_visd = sd(mean_acc_visd, na.rm = T), mean_acc_visd = mean(mean_acc_visd, na.rm = T)) |> 
  drop_na(position)
acc_mean_sd_3 <- acc_tot |> 
  ungroup() |> 
  filter(age_group == "apes (2-point ape calibration)") |> 
  group_by(filename, date) |> 
  dplyr::summarize(sd_acc_visd = sd(acc_visd, na.rm = T), mean_acc_visd = mean(acc_visd, na.rm = T))

## dplyr::summarize Precision (RMS) ----
precrms_mean_sd <- mean_sd(precrms_tot, c("age_group", "filename"), "precrms_visd") 
precrms_mean_sd_2 <- mean_sd(precrms_tot, c("age_group", "filename", "position"), "precrms_visd") 
precrms_mean_sd_2 <- precrms_mean_sd_2 |> 
  group_by(age_group, position) |> 
  dplyr::summarize(sd_precrms_visd = sd(mean_precrms_visd, na.rm = T), mean_precrms_visd = mean(mean_precrms_visd, na.rm = T)) |> 
  drop_na(position)
precrms_mean_sd_3 <- precrms_tot |> 
  ungroup() |> 
  filter(age_group == "apes (2-point ape calibration)") |> 
  group_by(filename, date) |> 
  dplyr::summarize(sd_precrms_visd = sd(precrms_visd, na.rm = T), mean_precrms_visd = mean(precrms_visd, na.rm = T))

##ä Trial Contributions ----
for(i in acc_tot$age_group |> unique()){
  trial_desc <- acc_tot |> 
    filter(age_group == i) |> 
    drop_na(accuracy) |> 
    select(filename, trial) |> 
    distinct() |> 
    group_by(filename) |> 
    count(filename, name = "trials") |> 
    ungroup() |> 
    summarise(M = mean(trials), SD = sd(trials))

  print(i)
  print(trial_desc)
}

for(i in precrms_tot$age_group |> unique()){
  trial_desc <- precrms_tot |> 
    filter(age_group == i) |> 
    drop_na(precrms_visd) |> 
    select(filename, trial) |> 
    distinct() |> 
    group_by(filename) |> 
    count(filename, name = "trials") |> 
    ungroup() |> 
    summarise(M = mean(trials), SD = sd(trials))
  
  print(i)
  print(trial_desc)
}

## Inference Accuracy ----
t.test(acc_mean_sd |> filter(age_group == "4") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "6") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "4") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "9") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "4") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "18") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "4") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "adult") |> pull(mean_acc_visd), paired = F)

t.test(acc_mean_sd |> filter(age_group == "6") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "9") |> pull(mean_acc_visd), paired = F) # ns
t.test(acc_mean_sd |> filter(age_group == "6") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "18") |> pull(mean_acc_visd), paired = F) # ns
t.test(acc_mean_sd |> filter(age_group == "6") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "adult") |> pull(mean_acc_visd), paired = F)

t.test(acc_mean_sd |> filter(age_group == "9") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "18") |> pull(mean_acc_visd), paired = F) # ns
t.test(acc_mean_sd |> filter(age_group == "9") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "adult") |> pull(mean_acc_visd), paired = F)

t.test(acc_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "4") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "6") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "9") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "18") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "adult") |> pull(mean_acc_visd), paired = F)

t.test(acc_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "4") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "6") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "9") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "18") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "adult") |> pull(mean_acc_visd), paired = F)

t.test(acc_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "4") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "6") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "9") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "18") |> pull(mean_acc_visd), paired = F)
t.test(acc_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "adult") |> pull(mean_acc_visd), paired = F)

t.test(acc_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_acc_visd), paired = F) # s
t.test(acc_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_acc_visd), paired = F) # s
t.test(acc_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_acc_visd), acc_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_acc_visd), paired = F) # s

## Inference RMS ----
t.test(precrms_mean_sd |> filter(age_group == "4") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "6") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "4") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "9") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "4") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "18") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "4") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "adult") |> pull(mean_precrms_visd), paired = F) # s

t.test(precrms_mean_sd |> filter(age_group == "6") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "9") |> pull(mean_precrms_visd), paired = F) # ns
t.test(precrms_mean_sd |> filter(age_group == "6") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "18") |> pull(mean_precrms_visd), paired = F) # ns
t.test(precrms_mean_sd |> filter(age_group == "9") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "18") |> pull(mean_precrms_visd), paired = F) # ns

t.test(precrms_mean_sd |> filter(age_group == "6") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "adult") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "9") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "adult") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "18") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "adult") |> pull(mean_precrms_visd), paired = F) # s

t.test(precrms_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "4") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "6") |> pull(mean_precrms_visd), paired = F) # ns
t.test(precrms_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "9") |> pull(mean_precrms_visd), paired = F) # ns
t.test(precrms_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "18") |> pull(mean_precrms_visd), paired = F) # ns
t.test(precrms_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "adult") |> pull(mean_precrms_visd), paired = F) # ns

t.test(precrms_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_precrms_visd), paired = F) # ns
t.test(precrms_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "4") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "6") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "9") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "18") |> pull(mean_precrms_visd), paired = F) # s 
t.test(precrms_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "adult") |> pull(mean_precrms_visd), paired = F) # ns

t.test(precrms_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "4") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "6") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "9") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "18") |> pull(mean_precrms_visd), paired = F) # s
t.test(precrms_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "adult") |> pull(mean_precrms_visd), paired = F) # s

t.test(precrms_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "apes (2-point ape calibration)") |> pull(mean_precrms_visd), paired = F) # ns
t.test(precrms_mean_sd |> filter(age_group == "apes (5-point alex calibration)") |> pull(mean_precrms_visd), precrms_mean_sd |> filter(age_group == "apes (9-point human calibration)") |> pull(mean_precrms_visd), paired = F) # ns

## Drop Out per Position ----
all_trials <- df_tot_chimps_2p |> 
  filter(trial <= 79) |> # exclude repetition sessions
  left_join(data.frame(trial = c(1:79))) |> 
  select(trial, stimulus) |> 
  distinct() |> 
  drop_na(stimulus) |> 
  arrange(trial)

all_trials <- all_trials |> 
  bind_rows(replicate(df_tot_chimps_2p$filename |> unique() |> length() - 1, all_trials, simplify = FALSE)) |> 
  mutate(filename = rep(df_tot_chimps_2p$filename |> unique(), each = 79))

df_tot_chimps_2p |> 
  right_join(all_trials) |>
  select(trial, stimulus, filename, session, acc_visd) |> 
  distinct() |> 
  group_by(stimulus) |> 
  summarise(n_na = sum(is.na(acc_visd)),
            n_val = sum(!is.na(acc_visd)),
            n_drop = round(n_na / (n_na+n_val),2))

df_tot_chimps_2p |> 
  right_join(all_trials) |>
  select(trial, stimulus, filename, session, precrms_visd) |> 
  distinct() |>
  group_by(stimulus) |> 
  summarise(n_na = sum(is.na(precrms_visd)),
            n_val = sum(!is.na(precrms_visd)),
            n_drop = round(n_na / (n_na+n_val),2))

df_tot_chimps_2p |> 
#  filter(position == "up" & session %in% c("session01","session02","session03","session04","session05","session06","session07","session08")) |> 
#  filter(position == "down" & session %in% c("session01","session02","session03","session04","session05","session06","session07","session08")) |> 
#  filter(position == "center" & session %in% c("session01","session02","session03","session04","session05","session06","session07","session08")) |> 
#  filter(position == "centercenter" & session %in% c("session01","session02","session03","session04","session05","session06","session07","session08")) |> 
#  filter(position == "topleft" & session %in% c("session01","session02","session03","session04","session05","session06","session07","session08")) |> 
#  filter(position == "topright" & session %in% c("session01","session02","session03","session04","session05","session06","session07","session08")) |> 
#  filter(position == "botleft" & session %in% c("session01","session02","session03","session04","session05","session06","session07","session08")) |> 
#  filter(position == "botright" & session %in% c("session01","session02","session03","session04","session05","session06","session07","session08")) |> 
  drop_na(acc_visd) |> 
  group_by(filename) |> 
  count() |> 
#  mutate(N = 16, drop = round(n/N,2)) |> 
#  mutate(N = 32, drop = round(n/N,2)) |> 
mutate(N = 3, drop = round(n/N,2)) |> 
  ungroup() |> 
  dplyr::summarize(M = mean(drop)) # up: 60%, down: 66%, center: 62%, centercenter: 73%, topleft: 65%, topright: 67%, botleft: 65%, botright: 71%

# Reliability ----
intertrial_individual <- df_tot |> 
  select(age_group, filename, trial, acc_visd, precrms_visd) |>
  group_by(filename) |> 
  dplyr::summarize(
    intertrial_sd = sd(precrms_visd, na.rm = TRUE),  # intertrial variability
    #intertrial_sd = sd(acc_visd, na.rm = TRUE),  # intertrial variability
    n_trials = n(),
    age_group = first(age_group))

intertrial_agegroup <- intertrial_individual |> 
  group_by(age_group) |> 
  dplyr::summarize(
    mean_intertrial_sd = mean(intertrial_sd, na.rm = TRUE),
    n_individuals = n())

# ## dplyr::summarize Precision SD ----
# precsd_mean_sd <- mean_sd(df_tot, c("age_group", "filename"), "precsd_visd")
# 
# ## Fixation Duration ----
# df_tot <- df_tot |> rename(fixation_duration = mean_fixation_duration)
# fixdur_mean_sd <- mean_sd(df_tot, c("age_group", "filename"), "fixation_duration")
# 
# ## Fixation Number ----
# df_tot <- df_tot |> rename(fixation_number = mean_fixation_number)
# fixnum_mean_sd <- mean_sd(df_tot, c("age_group", "filename"), "fixation_number")
# 
# ## Latency ----
# lat_mean_sd <- mean_sd(df_tot |> filter(stimulus == "object"), c("age_group", "filename"), "latencies") # todo: ask Maleen about filter object
# 
# ## Relative Looking Time in AOI ----
# rellt_mean_sd <- mean_sd(df_tot, c("age_group", "filename"), "rel_gaze_in_aoi")
# relfix_mean_sd <- mean_sd(df_tot, c("age_group", "filename"), "rel_fix_in_aoi")
# 
# ## Merge Data ----
# merged_data <- acc_mean_sd |>
#   left_join(precrms_mean_sd) |>
#   left_join(precsd_mean_sd) |>
#   left_join(fixdur_mean_sd) |>
#   left_join(fixnum_mean_sd) |>
#   left_join(lat_mean_sd) |>
#   left_join(rellt_mean_sd) |> 
#   left_join(relfix_mean_sd) |> 
#   mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult")))

# Plot --------------------------------------------------------------------

## RQ1 ----
# pdf(here("img", "rq1_distributions.pdf"), width = 12, height = 7)

### Accuracy (Group Differences) ----
plot_distribution_rq1(data = acc_mean_sd |>
                        mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult", "apes (2-point ape calibration)", "apes (9-point human calibration)"))) |> 
                        group_by(age_group) |> dplyr::summarize(sd_acc_visd = sd(mean_acc_visd, na.rm = T), mean_acc_visd = mean(mean_acc_visd, na.rm = T)),
                      mean_col = "mean_acc_visd", sd_col = "sd_acc_visd",
                      plot_title = "Eye Tracking Data Quality (Accuracy) (Preliminary Results) (The Greater, The Worse)", x_label = "Accuracy in Visual Degree", x_lim = c(0, 8))

#png("acc_violin_5.png", width = 2048, height = 1152, res = 300)
# png("acc_violin_6.png", width = 2048, height = 1152, res = 300)
#png("acc_violin_7.png", width = 2048, height = 1152, res = 300)
#png("acc_violin_8.png", width = 2048, height = 1152, res = 300)
ggplot(acc_mean_sd |> 
         mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult", "apes (2-point ape calibration)", "apes (9-point human calibration)", "apes (5-point alex calibration)"),
                                   labels = c("4", "6", "9", "18", "adult", "apes\n(2 points\napes)", "apes\n(9 points\nhumans)", "apes\n(5 points\nalex)"))),
      # filter(age_group != "apes\n(2 points\napes)") |>
      # filter(age_group != "apes\n(5 points\nalex)") |>
      # filter(age_group != "apes\n(9 points\nhumans)"),
       aes(x = age_group, y = mean_acc_visd, fill = age_group)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.7, size = 2, color = "black") +
  labs(
    title = "Accuracy",
    x = "Group",
    y = "Accuracy\n(distance from center in visual degrees)"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "4" = "red",            # Age group 4 in red
      "6" = "mediumblue",     # Age group 6 in mediumdark blue
      "9" = "lightgreen",     # Age group 9 in intense green
      "18" = "yellow",        # Age group 18 in yellow
      "adult" = "purple",      # Age group adult in purple
      "apes\n(2 points\napes)" = "salmon3",
      "apes\n(9 points\nhumans)" = "grey",
      "apes\n(5 points\nalex)" = "orange")
  ) +
  theme(
    legend.position = "none",  # Remove legend for simplicity
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

# Juveniles versus adults
juvenile_names <- c("azibo.tsv", "badu.tsv", "carola.tsv", "makeni.tsv", "ohini.tsv", "youma.tsv")
adult_names <- c("alex.tsv", "daza.tsv", "fraukje.tsv", "frederike.tsv", "frodo.tsv",
                 "hope.tsv", "jeudi.tsv", "maja.tsv", "natascha.tsv", "riet.tsv",
                 "sandra.tsv", "tai.tsv", "zira.tsv")

# Update age_group based on filename
juvenil_adult_data <- acc_mean_sd |> 
  mutate(age_group = case_when(
    age_group == "apes (2-point ape calibration)" & str_detect(filename, str_c(juvenile_names, collapse = "|")) ~ "apes (juveniles)",
    age_group == "apes (2-point ape calibration)" & str_detect(filename, str_c(adult_names, collapse = "|")) ~ "apes (adults)",
    TRUE ~ age_group)) |> 
  filter(age_group != "apes (9-point human calibration)")
  
#png("acc_violin_juveniles.png", width = 2048, height = 1152, res = 300)
ggplot(juvenil_adult_data |> 
         mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult", "apes (juveniles)", "apes (adults)"),
                                   labels = c("4", "6", "9", "18", "adult", "apes\n(juveniles)", "apes\n(adults)"))) |> 
         drop_na(age_group),
       aes(x = age_group, y = mean_acc_visd, fill = age_group)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.7, size = 2, color = "black") +
  labs(
    title = "Accuracy",
    x = "Group",
    y = "Accuracy\n(distance from center in visual degrees)"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "4" = "red",            # Age group 4 in red
      "6" = "mediumblue",     # Age group 6 in mediumdark blue
      "9" = "lightgreen",     # Age group 9 in intense green
      "18" = "yellow",        # Age group 18 in yellow
      "adult" = "purple",      # Age group adult in purple
      "apes\n(juveniles)" = "salmon2",
      "apes\n(adults)" = "whitesmoke"
    )
  ) +
  theme(
    legend.position = "none",  # Remove legend for simplicity
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
#dev.off()

# Plot per position
acc_mean_sd_2$position <- factor(acc_mean_sd_2$position, levels = c("center", "topleft", "top", "topright", "botleft", "bottom", "botright"))
acc_mean_sd_2$age_group <- factor(acc_mean_sd_2$age_group, levels = c("4", "6", "9", "18", "adult", "apes (2-point ape calibration)", "apes (9-point human calibration)"))
custom_colors <- c("4" = "#FF6347", 
                   "6" = "#4682B4",
                   "9" = "#32CD32",
                   "18" = "#FFD700",
                   "adult" = "#8A2BE2",
                   "apes (2-point ape calibration)" = "#000000",
                   "apes (9-point human calibration)" = "#891101")

ggplot(acc_mean_sd_2, aes(x = position, y = mean_acc_visd, color = age_group)) +
  geom_errorbar(aes(ymin = mean_acc_visd - sd_acc_visd, ymax = mean_acc_visd + sd_acc_visd),
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(x = "Position", y = "Mean Accuracy (visd)", title = "Mean Accuracy by Position and Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = custom_colors, name = "Group")

### Precision RMS (Group Differences) ----
plot_distribution_rq1(data = precrms_mean_sd |> mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult"))) |> group_by(age_group) |> 
                        dplyr::summarize(sd_precrms_visd = sd(mean_precrms_visd, na.rm = T) , mean_precrms_visd = mean(mean_precrms_visd, na.rm = T)),
                      mean_col = "mean_precrms_visd", sd_col = "sd_precrms_visd",
                      plot_title = "Normal Distributions of Precision (RMS) by Age Group", x_label = "Precision (RMS) in Visual Degree", x_lim = c(0, 2))

#png("precrms_violin_5.png", width = 2048, height = 1152, res = 300)
#png("precrms_violin_6.png", width = 2048, height = 1152, res = 300)
#png("precrms_violin_7.png", width = 2048, height = 1152, res = 300)
png("precrms_violin_8.png", width = 2048, height = 1152, res = 300)
ggplot(precrms_mean_sd |> 
         mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult", "apes (2-point ape calibration)", "apes (9-point human calibration)", "apes (5-point alex calibration)"),
                                   labels = c("4", "6", "9", "18", "adult", "apes\n(2 points\napes)", "apes\n(9 points\nhumans)", "apes\n(5 points\nalex)"))), #|>
         #filter(age_group != "apes\n(2 points\napes)") |>
         #filter(age_group != "apes\n(5 points\nalex)"),# |>
         #filter(age_group != "apes\n(9 points\nhumans)"),      
       aes(x = age_group, y = mean_precrms_visd, fill = age_group)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.7, size = 2, color = "black") +
  labs(
    title = "Precision",
    x = "Group",
    y = "Precision\n(distance between gaze samples per fixation\nin visual degrees)"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "4" = "red",            # Age group 4 in red
      "6" = "mediumblue",     # Age group 6 in mediumdark blue
      "9" = "lightgreen",     # Age group 9 in intense green
      "18" = "yellow",        # Age group 18 in yellow
      "adult" = "purple",      # Age group adult in purple
      "apes\n(2 points\napes)" = "salmon3",
      "apes\n(9 points\nhumans)" = "grey",
      "apes\n(5 points\nalex)" = "orange"
    )
  ) +
  theme(
    legend.position = "none",  # Remove legend for simplicity
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

### Accuracy (Sessions) ----
library(patchwork)
acc_mean_sd_3_session <- acc_mean_sd_3 |> 
  mutate(filename_clean = str_remove(filename, "\\.tsv")) |> 
  mutate(date = mdy(date)) |> 
  arrange(filename_clean, date)

acc_mean_sd_3_session <- acc_mean_sd_3_session |> 
  group_by(filename_clean) |> 
  mutate(session = row_number()) |> 
  mutate(session = as.integer(session)) |> 
  ungroup() |> 
  filter(filename_clean != "badu")

plots <- acc_mean_sd_3_session |> 
  group_split(filename_clean) |> 
  lapply(function(df_part) {
    fname <- unique(df_part$filename_clean)
    
    ggplot(df_part, aes(x = session, y = mean_acc_visd)) +
      geom_line(color = "darkgrey") +
      geom_point(color = "steelblue", size = 2) +
      labs(
        title = fname,
        y = "Mean Accuracy"
      ) +
      theme_minimal()
  })

combined_plot <- wrap_plots(plots) +
  plot_layout(ncol = 3)  # Adjust number of columns as needed
png(here("img", "acc_session_1.png"), width = 2048, height = 2048+1152, res = 300)
print(combined_plot)
dev.off()

### Precision (RMS) (Sessions) ----
library(patchwork)
precrms_mean_sd_3_session <- precrms_mean_sd_3 |> 
  mutate(filename_clean = str_remove(filename, "\\.tsv")) |> 
  mutate(date = mdy(date)) |> 
  arrange(filename_clean, date)

precrms_mean_sd_3_session <- precrms_mean_sd_3_session |> 
  group_by(filename_clean) |> 
  mutate(session = row_number()) |> 
  mutate(session = as.integer(session)) |> 
  ungroup() |> 
  filter(filename_clean != "badu")

plots <- precrms_mean_sd_3_session |> 
  group_split(filename_clean) |> 
  lapply(function(df_part) {
    fname <- unique(df_part$filename_clean)
    
    ggplot(df_part, aes(x = session, y = mean_precrms_visd)) +
      geom_line(color = "darkgrey") +
      geom_point(color = "steelblue", size = 2) +
      labs(
        title = fname,
        y = "Mean Precision (RMS)"
      ) +
      theme_minimal()
  })

combined_plot <- wrap_plots(plots) +
  plot_layout(ncol = 3)  # Adjust number of columns as needed
png(here("img", "precrms_session_1.png"), width = 2048, height = 2048+1152, res = 300)
print(combined_plot)
dev.off()

# Plot the session-wise average
n_apes_session <- precrms_mean_sd_3_session |> pull(filename) |> unique() |> length()

png(here("img", "precrms_session_2.png"), width = 2048, height = 1152, res = 300)
ggplot(precrms_mean_sd_3_session |> group_by(session) |> dplyr::summarize(sd_precrms_visd = sd(mean_precrms_visd, na.rm = T),
                                                                   mean_precrms_visd = mean(mean_precrms_visd, na.rm = T),
                                                                   n = n_apes_session,
                                                                   se = sd_precrms_visd / sqrt(n),
                                                                   ci = qt(0.975, df = n - 1) * se,
                                                                   ci_lower = mean_precrms_visd - ci,
                                                                   ci_upper = mean_precrms_visd + ci), 
       aes(x = session, y = mean_precrms_visd)) +
  geom_line(color = "grey60") +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkgrey") +
  labs(
    x = "Session",
    y = "Mean Precision (RMS)",
    title = "Average Precision (RMS) Across Individuals per Session"
  ) +
  theme_minimal()
dev.off()

# Plot the session-wise average
n_apes_session <- acc_mean_sd_3_session |> pull(filename) |> unique() |> length()

png(here("img", "acc_session_2.png"), width = 2048, height = 1152, res = 300)
ggplot(acc_mean_sd_3_session |> group_by(session) |> dplyr::summarize(sd_acc_visd = sd(mean_acc_visd, na.rm = T),
                                                               mean_acc_visd = mean(mean_acc_visd, na.rm = T),
                                                               n = n_apes_session,
                                                               se = sd_acc_visd / sqrt(n),
                                                               ci = qt(0.975, df = n - 1) * se,
                                                               ci_lower = mean_acc_visd - ci,
                                                               ci_upper = mean_acc_visd + ci), 
       aes(x = session, y = mean_acc_visd)) +
  geom_line(color = "grey60") +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkgrey") +
  labs(
    x = "Session",
    y = "Mean Accuracy",
    title = "Average Accuracy Across Individuals per Session"
  ) +
  theme_minimal()
dev.off()

## RQ 2 ----
png(here("img", "rq2_acc_rellt.png"), width = 2048, height = 2048, res = 300)
ggplot(df_tot |> 
         filter(!(age_group %in% c("apes (9-point human calibration)"))) |> 
         mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult", "apes (2-point ape calibration)"))) |> 
         group_by(age_group, filename) |> 
         dplyr::summarize(acc_visd = mean(acc_visd, na.rm = T), rel_gaze_in_aoi = mean(rel_gaze_in_aoi, na.rm = T)) |>
         ungroup(), aes_string(x = "acc_visd", y = "rel_gaze_in_aoi")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(as.formula(paste("~", "age_group")), scales = "free") +
  theme_minimal() +
  labs(title = "Accuracy & Mean Relative Gaze Sample Duration in AOI", x = "Accuracy", y = "Mean Relative Gaze Samples Duration in AOI")
dev.off()

png(here("img", "rq2_precrms_rellt.png"), width = 2048, height = 2048, res = 300)
ggplot(df_tot |> 
         filter(!(age_group %in% c("apes (9-point human calibration)"))) |> 
         mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult", "apes (2-point ape calibration)"))) |> 
         group_by(age_group, filename) |> 
         dplyr::summarize(precrms_visd = mean(precrms_visd, na.rm = T), rel_gaze_in_aoi = mean(rel_gaze_in_aoi, na.rm = T)) |>
         ungroup(), aes_string(x = "precrms_visd", y = "rel_gaze_in_aoi")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(as.formula(paste("~", "age_group")), scales = "free") +
  theme_minimal() +
  labs(title = "Precision & Mean Relative Gaze Sample Duration in AOI", x = "Precision", y = "Mean Relative Gaze Samples Duration in AOI")
dev.off()

png(here("img", "rq2_precrms_fixdur.png"), width = 2048, height = 2048, res = 300)
ggplot(df_tot |> 
         filter(!(age_group %in% c("apes (2-point ape calibration)", "apes (9-point human calibration)"))) |> 
         mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult"))) |> 
         group_by(age_group, filename) |> 
         dplyr::summarize(precrms_visd = mean(precrms_visd, na.rm = T), mean_fixation_duration = mean(mean_fixation_duration, na.rm = T)) |>
         ungroup(), aes_string(x = "precrms_visd", y = "mean_fixation_duration")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(as.formula(paste("~", "age_group")), scales = "free") +
  theme_minimal() +
  labs(title = "Precision & Fixation Duration", x = "Precision", y = "Fixation Duration")
dev.off()

png(here("img", "rq2_acc_fixdur.png"), width = 2048, height = 2048, res = 300)
ggplot(df_tot |> 
         filter(!(age_group %in% c("apes (2-point ape calibration)", "apes (9-point human calibration)"))) |>
         mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult"))) |> 
         group_by(age_group, filename) |> 
         dplyr::summarize(acc_visd = mean(acc_visd, na.rm = T), mean_fixation_duration = mean(mean_fixation_duration, na.rm = T)) |>
         ungroup(), aes_string(x = "acc_visd", y = "mean_fixation_duration")) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(as.formula(paste("~", "age_group")), scales = "free") +
  theme_minimal() +
  labs(title = "Accuracy & Fixation Duration", x = "Accuracy", y = "Fixation Duration")
dev.off()

## Further Analyses ----
### Accuracy Per Position ----
# Create the plot
ggplot(acc_mean_sd_2 |>  mutate(position = factor(position, levels = c("center", "top", "topleft", "topright", "bottom", "botleft", "botright"))), 
       aes(x = position, y = mean_acc_visd)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ age_group, scales = "free_y") +
  labs(
    x = "Position",
    y = "Mean Accuracy",
    title = "Mean Accuracy per Position by Age Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create the plot with points and error bars
N <- acc_mean_sd |> drop_na(filename) |>  select(age_group, filename) |> distinct() |> group_by(age_group) |> count() |> ungroup()

png(here("img", "acc_position_1.png"), width = 2048+1024, height = 1152, res = 300)
ggplot(acc_mean_sd_2 |>
         mutate(position = factor(position, levels = c("center", "top", "topleft", "topright", "bottom", "botleft", "botright"))) |> 
         left_join(N, by = "age_group") |> 
         mutate(se = sd_acc_visd / sqrt(n),
                ci = qt(0.975, df = n - 1) * se,
                ci_lower = mean_acc_visd - ci,
                ci_upper = mean_acc_visd + ci) |> 
         filter(age_group %in% c("4", "6", "9", "18", "adult")) |> 
         mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult"))), 
       aes(x = position, y = mean_acc_visd)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkgrey") +
  geom_point(size = 2, color = "steelblue") +
  facet_wrap(~ age_group, scales = "free_y", nrow= 1) +
  labs(
    x = "Position",
    y = "Mean Accuracy",
    title = "Mean Accuracy per Position by Age Group (with 95% CI)"
  ) +
  ylim(0.2,1.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

png(here("img", "acc_position_2.png"), width = 2048, height = 1152, res = 300)
ggplot(acc_mean_sd_2 |>
         mutate(position = factor(position, levels = c("center", "top", "topleft", "topright", "bottom", "botleft", "botright"))) |> 
         left_join(N, by = "age_group") |> 
         mutate(se = sd_acc_visd / sqrt(n),
                ci = qt(0.975, df = n - 1) * se,
                ci_lower = mean_acc_visd - ci,
                ci_upper = mean_acc_visd + ci) |> 
         filter(age_group %in% c("apes (2-point ape calibration)", "apes (9-point human calibration)")) |> 
         mutate(age_group = factor(age_group, levels = c("apes (2-point ape calibration)", "apes (9-point human calibration)"))), 
       aes(x = position, y = mean_acc_visd)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "grey") +
  geom_point(size = 2, color = "steelblue") +
  facet_wrap(~ age_group, scales = "free_y", nrow= 1) +
  labs(
    x = "Position",
    y = "Mean Accuracy",
    title = "Mean Accuracy per Position by Age Group (with 95% CI)"
  ) +
  ylim(1,6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

### Precision Per Position ----
# Create the plot
ggplot(precrms_mean_sd_2 |>  mutate(position = factor(position, levels = c("center", "top", "topleft", "topright", "bottom", "botleft", "botright"))), 
       aes(x = position, y = mean_precrms_visd)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ age_group, scales = "free_y") +
  labs(
    x = "Position",
    y = "Mean Precision",
    title = "Mean Precision per Position by Age Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create the plot with points and error bars
N <- precrms_mean_sd |> drop_na(filename) |>  select(age_group, filename) |> distinct() |> group_by(age_group) |> count() |> ungroup()

png(here("img", "precrms_position_1.png"), width = 2048+1024, height = 1152, res = 300)
ggplot(precrms_mean_sd_2 |>
         mutate(position = factor(position, levels = c("center", "top", "topleft", "topright", "bottom", "botleft", "botright"))) |> 
         left_join(N, by = "age_group") |> 
         mutate(se = sd_precrms_visd / sqrt(n),
                ci = qt(0.975, df = n - 1) * se,
                ci_lower = mean_precrms_visd - ci,
                ci_upper = mean_precrms_visd + ci) |> 
         filter(age_group %in% c("4", "6", "9", "18", "adult")) |> 
         mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult"))), 
       aes(x = position, y = mean_precrms_visd)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "darkgrey") +
  geom_point(size = 2, color = "steelblue") +
  facet_wrap(~ age_group, scales = "free_y", nrow= 1) +
  labs(
    x = "Position",
    y = "Mean Precision",
    title = "Mean Precision per Position by Age Group (with 95% CI)"
  ) +
  ylim(0,2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

png(here("img", "precrms_position_2.png"), width = 2048, height = 1152, res = 300)
ggplot(precrms_mean_sd_2 |>
         mutate(position = factor(position, levels = c("center", "top", "topleft", "topright", "bottom", "botleft", "botright"))) |> 
         left_join(N, by = "age_group") |> 
         mutate(se = sd_precrms_visd / sqrt(n),
                ci = qt(0.975, df = n - 1) * se,
                ci_lower = mean_precrms_visd - ci,
                ci_upper = mean_precrms_visd + ci) |> 
         filter(age_group %in% c("apes (2-point ape calibration)", "apes (9-point human calibration)")) |> 
         mutate(age_group = factor(age_group, levels = c("apes (2-point ape calibration)", "apes (9-point human calibration)"))), 
       aes(x = position, y = mean_precrms_visd)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, color = "grey") +
  geom_point(size = 2, color = "steelblue") +
  facet_wrap(~ age_group, scales = "free_y", nrow= 1) +
  labs(
    x = "Position",
    y = "Mean Precision",
    title = "Mean Precision per Position by Age Group (with 95% CI)"
  ) +
  ylim(-0.2,2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

## Accuracy Per Human Calibration ----
ape_daniela_alina_acc <- df_tot_chimps_9p |> 
  # filter((position %in% c("center", "centercenter", "center_center"))) |> 
  select(filename, acc_visd, calibration) |> 
  bind_rows(df_tot_chimps_2p |> select(acc_visd, filename) |> mutate(calibration = "Ape")) |> 
  group_by(filename, calibration) |> 
  dplyr::summarize(acc_visd = mean(acc_visd, na.rm=T)) |> 
  ungroup() |> 
  pivot_wider(names_from = "calibration", values_from = "acc_visd") |> 
  drop_na() |> 
  mutate(diff_alex = round(Ape - Alex,2)) |> 
  mutate(diff_alina = round(Ape - Alina,2)) |> 
  mutate(diff_hanna = round(Ape - Hanna,2)) |> 
  mutate(diff_matthias = round(Ape - Matthias,2)) |> 
  mutate(diff_daniela = round(Ape - Daniela,2)) |> 
  mutate(diff_josefine = round(Ape - Josefine,2)) |> 
  mutate(diff_marie = round(Ape - Marie_,2)) |> 
  rename(alina = Alina, daniela = Daniela, ape = Ape, hanna = Hanna, josefine = Josefine, marie = Marie_, matthias = Matthias, alex = Alex)# |> 
  # select(-c(filename, alex, alina, ape, daniela, hanna, josefine, marie, matthias)) |> 
  # summarise(across(everything(), mean, na.rm = TRUE)) |> 
  # select(diff_alina, diff_alex, diff_josefine, diff_matthias, diff_hanna, diff_marie, diff_daniela)

ape_alina <- ape_daniela_alina_acc |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, alina) |> 
  pivot_longer(cols = c("ape", "alina"), names_to = "calibration", values_to = "value") |>
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x))) |> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x))) |> 
  mutate(calibration = factor(calibration, levels = c("Ape", "Alina")))

png(here("img", "acc_alina_1.png"), width = 2048/1.5, height = 1152, res = 300)
ggplot(ape_alina, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs Alina", x = NULL, y = "Mean Accuracy", color = "Individual") +
  theme_minimal() +
  ylim(0,7)
dev.off()

ape_daniela <- ape_daniela_alina_acc |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, daniela) |> 
  pivot_longer(cols = c("ape", "daniela"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "acc_daniela_1.png"), width = 2048/1.5, height = 1152, res = 300)
ggplot(ape_daniela, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs Daniela", x = NULL, y = "Mean Accuracy", color = "Individual") +
  theme_minimal() +
  ylim(0,7)
dev.off()

ape_josefine <- ape_daniela_alina_acc |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, josefine) |> 
  pivot_longer(cols = c("ape", "josefine"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "acc_josefine_1.png"), width = 2048/1.5, height = 1152, res = 300)
ggplot(ape_josefine, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs Josefine", x = NULL, y = "Mean Accuracy", color = "Individual") +
  theme_minimal() +
  ylim(0,7)
dev.off()

ape_marie <- ape_daniela_alina_acc |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, marie) |> 
  pivot_longer(cols = c("ape", "marie"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "acc_marie_1.png"), width = 2048/1.5, height = 1152, res = 300)
ggplot(ape_marie, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs Marie", x = NULL, y = "Mean Accuracy", color = "Individual") +
  theme_minimal() +
  ylim(0,7)
dev.off()

ape_hanna <- ape_daniela_alina_acc |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, hanna) |> 
  pivot_longer(cols = c("ape", "hanna"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "acc_hanna_1.png"), width = 2048/1.5, height = 1152, res = 300)
ggplot(ape_hanna, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs Hanna", x = NULL, y = "Mean Accuracy", color = "Individual") +
  theme_minimal() +
  ylim(0,7)
dev.off()

ape_matthias <- ape_daniela_alina_acc |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, matthias) |> 
  pivot_longer(cols = c("ape", "matthias"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "acc_matthias_1.png"), width = 2048/1.5, height = 1152, res = 300)
ggplot(ape_matthias, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs Matthias", x = NULL, y = "Mean Accuracy", color = "Individual") +
  theme_minimal() +
  ylim(0,7)
dev.off()

ape_averaged_acc <- ape_daniela_alina_acc |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(-c(ape, alex, diff_alex, diff_alina, diff_hanna, diff_matthias, diff_daniela, diff_josefine, diff_marie)) |> 
  pivot_longer(cols = c("alina", "daniela", "matthias", "hanna", "marie", "josefine"), names_to = "calibration", values_to = "accuracy") |>
  group_by(filename) |> 
  dplyr::summarize(human_acc = mean(accuracy, na.rm = T)) |> # average across human calibration
  ungroup()

ape_averaged_acc2 <- ape_daniela_alina_acc |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(c(filename, alex)) |> 
  pivot_longer(cols = "alex", names_to = "calibration", values_to = "accuracy") |>
  rename(alex_acc = accuracy) |> 
  select(-calibration)

ape_averaged_acc3 <- ape_daniela_alina_acc |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(c(filename, ape)) |> 
  pivot_longer(cols = "ape", names_to = "calibration", values_to = "accuracy") |>
  rename(ape_acc = accuracy) |> 
  select(-calibration)

ape_averaged_acc4 <- ape_averaged_acc |> 
  left_join(ape_averaged_acc2) |> 
  left_join(ape_averaged_acc3) |> 
  pivot_longer(cols = c("human_acc", "alex_acc", "ape_acc"), values_to = "accuracy") |> 
  mutate(name = recode(name, human_acc = "Human Calibration (9p)")) |> 
  mutate(name = recode(name, alex_acc = "Alex Calibration (5p)")) |> 
  mutate(name = recode(name, ape_acc = "Ape Calibration (2p)"))
  
ape_averaged_acc5 <- ape_averaged_acc4 |> 
  group_by(name) |> 
  dplyr::summarize(accuracy = mean(accuracy, na.rm = T)) |> 
  pull(accuracy) |> round(2) # 1. Alex, 2. Apes, 3. Human 9 point

png(here("img", "acc_ape2p_vs_human9p.png"), width = 2048/1.5, height = 1152, res = 300)
ggplot(ape_averaged_acc4 |> filter(name %in% c("Human Calibration (9p)", "Ape Calibration (2p)")), aes(x = name, y = accuracy, group = filename, color = filename)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = paste0("Ape: ", ape_averaged_acc5[2], ". Human: ", ape_averaged_acc5[3], "."), x = NULL, y = "Mean Accuracy", color = "Individual") +
  theme_minimal() +
  ylim(1.5,6.5)
dev.off()

png(here("img", "acc_ape2p_vs_alex5p.png"), width = 2048/1.5, height = 1152, res = 300)
ggplot(ape_averaged_acc4 |> filter(name %in% c("Alex Calibration (5p)", "Ape Calibration (2p)")) |> 
         mutate(name = factor(name, levels = c("Ape Calibration (2p)", "Alex Calibration (5p)"))), 
       aes(x = name, y = accuracy, group = filename, color = filename)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  labs(title = paste0("Alex: ", ape_averaged_acc5[1], ". Apes: ", ape_averaged_acc5[2], "."), x = NULL, y = "Mean Accuracy", color = "Individual") +
  theme_minimal() +
  ylim(1.5,6.5)
dev.off()

## Precision Per Human Calibration ----
# I think this has to be adjusted, use code of accuracy and make the same with precision (todo)
ape_daniela_alina_precrms <- df_tot_chimps_9p |> 
  select(filename, precrms_visd, calibration) |> 
  bind_rows(df_tot_chimps_2p |> select(precrms_visd, filename) |> mutate(calibration = "Ape")) |> 
  group_by(filename, calibration) |> 
  dplyr::summarize(precrms_visd = mean(precrms_visd, na.rm=T)) |> 
  ungroup() |> 
  pivot_wider(names_from = "calibration", values_from = "precrms_visd") |> 
  drop_na() |> 
  mutate(diff_alex = round(Ape - Alex,2)) |> 
  mutate(diff_alina = round(Ape - Alina,2)) |> 
  mutate(diff_hanna = round(Ape - Hanna,2)) |> 
  mutate(diff_matthias = round(Ape - Matthias,2)) |> 
  mutate(diff_daniela = round(Ape - Daniela,2)) |> 
  mutate(diff_josefine = round(Ape - Josefine,2)) |> 
  mutate(diff_marie = round(Ape - Marie_,2)) |> 
  rename(alina = Alina, daniela = Daniela, ape = Ape, hanna = Hanna, josefine = Josefine, marie = Marie_, matthias = Matthias, alex = Alex) #|> 
  # select(-c(filename, alex, alina, ape, daniela, hanna, josefine, marie, matthias)) |>
  # summarise(across(everything(), mean, na.rm = TRUE)) |>
  # select(diff_alina, diff_alex, diff_josefine, diff_matthias, diff_hanna, diff_marie, diff_daniela)

ape_alina <- ape_daniela_alina_precrms  |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, alina) |> 
  pivot_longer(cols = c("ape", "alina"), names_to = "calibration", values_to = "value") |>
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x))) |> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x))) |> 
  mutate(calibration = factor(calibration, levels = c("Ape", "Alina")))

png(here("img", "precrms_alina_1.png"), width = 2048, height = 1152, res = 300)
ggplot(ape_alina, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs Alina", x = NULL, y = "Mean Precision", color = "Individual") +
  theme_minimal() +
  ylim(0,1.5)
dev.off()

ape_daniela <- ape_daniela_alina_precrms |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, daniela) |> 
  pivot_longer(cols = c("ape", "daniela"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "precrms_daniela_1.png"), width = 2048, height = 1152, res = 300)
ggplot(ape_daniela, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs Daniela", x = NULL, y = "Mean Precision", color = "Individual") +
  theme_minimal() +
  ylim(0,1.5)
dev.off()

ape_marie <- ape_daniela_alina_precrms |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, marie) |> 
  pivot_longer(cols = c("ape", "marie"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "precrms_marie_1.png"), width = 2048, height = 1152, res = 300)
ggplot(ape_marie, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs Marie", x = NULL, y = "Mean Precision", color = "Individual") +
  theme_minimal() +
  ylim(0,1.5)
dev.off()

ape_josefine <- ape_daniela_alina_precrms |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, josefine) |> 
  pivot_longer(cols = c("ape", "josefine"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "precrms_josefine_1.png"), width = 2048, height = 1152, res = 300)
ggplot(ape_josefine, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs josefine", x = NULL, y = "Mean Precision", color = "Individual") +
  theme_minimal() +
  ylim(0,1.5)
dev.off()

ape_hanna <- ape_daniela_alina_precrms |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, hanna) |> 
  pivot_longer(cols = c("ape", "hanna"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "precrms_hanna_1.png"), width = 2048, height = 1152, res = 300)
ggplot(ape_hanna, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs hanna", x = NULL, y = "Mean Precision", color = "Individual") +
  theme_minimal() +
  ylim(0,1.5)
dev.off()

ape_matthias <- ape_daniela_alina_precrms |> 
  mutate(filename = str_remove(filename, ".tsv")) |>
  select(filename, ape, matthias) |> 
  pivot_longer(cols = c("ape", "matthias"), names_to = "calibration", values_to = "value") |> 
  mutate(filename = str_replace(filename, "^(\\w)", ~ toupper(.x)))|> 
  mutate(calibration = str_replace(calibration, "^(\\w)", ~ toupper(.x)))

png(here("img", "precrms_matthias_1.png"), width = 2048, height = 1152, res = 300)
ggplot(ape_matthias, aes(x = calibration, y = value, group = filename, color = filename)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Ape vs matthias", x = NULL, y = "Mean Precision", color = "Individual") +
  theme_minimal() +
  ylim(0,1.5)
dev.off()


# Plot for poster ----
## Data preparation
df_plot <- acc_mean_sd |> 
  mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult", "apes (2-point ape calibration)", "apes (9-point human calibration)", "apes (5-point alex calibration)"),
                            labels = c("4", "6", "9", "18", "adult", "chimpanzees\n(2 points\napes)", "chimpanzees\n(9 points\nhumans)", "chimpanzees\n(5 points\nalex)")))

pal_1 <- c("4" = "red", "6" = "mediumblue", "9" = "lightgreen","18" = "yellow", "adult" = "purple",
           "apes\n(2 points\napes)" = "salmon3", "apes\n(9 points\nhumans)" = "grey", "apes\n(5 points\nalex)" = "orange")

g_acc_1 <- ggplot(df_plot |> filter(age_group %in% c("4", "6", "9", "18", "adult", "chimpanzees\n(2 points\napes)")), aes(x = age_group, y = mean_acc_visd)) +
  geom_violin(aes(fill = age_group),
              trim = FALSE, width = 0.85,
              alpha = 0.25, color = NA) +
  geom_jitter(aes(color = age_group),
              width = 0.08, height = 0,
              size = 1.7, alpha = 0.85) +
  stat_summary(fun = mean, geom = "point",
               size = 3.2, color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
               width = 0.12, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = pal_1, name = "Age Group") +
  scale_color_manual(values = pal_1, name = "Age Group") +
  scale_y_continuous(limits = c(0, 6), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Age Group",
    y = "<span style='font-size:12pt; font-weight:600'>Accuracy (in Visual Degrees)</span><br>
         <span style='font-size:9pt; font-weight:400'>Distance From Stimulus Center</span>"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.y = ggtext::element_markdown(size = 12,
                                            lineheight = 1.05,
                                            angle = 90)
  )

pal_2 <- c("4" = "red", "6" = "mediumblue", "9" = "lightgreen","18" = "yellow", "Adult" = "purple", "Chimpanzees" = "salmon3")
g_acc_2 <- ggplot(df_plot |> 
                    filter(age_group %in% c("4", "6", "9", "18", "adult", "chimpanzees\n(2 points\napes)"))|> 
                    mutate(age_group = factor(age_group, levels = c("4", "6", "9", "18", "adult", "chimpanzees\n(2 points\napes)"),
                                              labels = c("4", "6", "9", "18", "Adult", "Chimpanzees"))),
                  aes(x = age_group, y = mean_acc_visd)) +
  geom_violin(aes(fill = age_group),
              trim = FALSE, width = 0.85,
              alpha = 0.25, color = NA) +
  geom_jitter(aes(color = age_group),
              width = 0.08, height = 0,
              size = 1, alpha = 0.85) +
  stat_summary(fun = mean, geom = "point",
               size = 1, color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
               width = 0.12, color = "black", linewidth = 0.5) +
  scale_fill_manual(values = pal_2, name = "Group") +
  scale_color_manual(values = pal_2, name = "Group") +
  scale_y_continuous(limits = c(0, 7), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Group",
    y = "<span style='font-size:12pt; font-weight:600'>Accuracy (in Visual Degrees)</span><br>
         <span style='font-size:9pt; font-weight:400'>Distance From Stimulus Center</span>"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    axis.title.y = ggtext::element_markdown(size = 12,
                                            lineheight = 1.05,
                                            angle = 90)
  ) + 
  theme(legend.position = "none")

pal_3 <- c("Own (2p)" = "salmon3", "Human\nAdult (9p)" = "gray15", "Other\nChimpanzee (5p)" = "orange")
g_acc_3 <- ggplot(df_plot |> 
                    filter(age_group %in% c("chimpanzees\n(2 points\napes)", "chimpanzees\n(5 points\nalex)", "chimpanzees\n(9 points\nhumans)")) |> 
                    mutate(age_group = factor(age_group, 
                                              levels = c("chimpanzees\n(2 points\napes)", "chimpanzees\n(9 points\nhumans)", "chimpanzees\n(5 points\nalex)"),
                                              labels = c("Own (2p)", "Human\nAdult (9p)", "Other\nChimpanzee (5p)"))),
                    aes(x = age_group, y = mean_acc_visd)) +
  geom_violin(aes(fill = age_group),
              trim = FALSE, width = 0.85,
              alpha = 0.25, color = NA) +
  geom_jitter(aes(color = age_group),
              width = 0.08, height = 0,
              size = 1, alpha = 0.85) +
  stat_summary(fun = mean, geom = "point",
               size = 3.2, color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
               width = 0.12, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = pal_3, name = "Calibration") +
  scale_color_manual(values = pal_3, name = "Calibration") +
  scale_y_continuous(limits = c(0, 7), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Calibration",
    y = "<span style='font-size:12pt; font-weight:600'>Accuracy (in Visual Degrees)</span><br>
         <span style='font-size:9pt; font-weight:400'>Distance From Stimulus Center</span>"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "right",
    axis.title.y = ggtext::element_markdown(size = 12,
                                            lineheight = 1.05,
                                            angle = 90)
  ) +
  theme(legend.position = "none")

# png(here("img", "posterplot_rq1_accuracy.png"), width = 700, height = 800, res = 210)
# g_acc_3
# dev.off()

png(here("img", "paperplot_rq3_accuracy_350.png"), width = 2048, height = 1152, res = 350)
g_acc_3
dev.off()


# png(here("img", "posterplot_rq1_accuracy.png"), width = 1300, height = 750, res = 200)
# g_acc_2
# dev.off()
# 
# png(here("img", "paperplot_rq1_accuracy.png"), width = 2048, height = 1152, res = 250)
# g_acc_2
# dev.off()
