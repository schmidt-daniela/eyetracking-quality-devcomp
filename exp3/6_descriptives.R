# next: check result with gaze replay, how to calculate it in visual degrees

# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(Hmisc)
library(lme4)
library(ggtext)

# Functions ---------------------------------------------------------------
source(here("fun", "descriptives.R"))

# Adjust Parameter --------------------------------------------------------
age_group <- "4mo" # "4mo" or "6to18mo"
filenames <- list.files(path = here("data", "preproc_2", age_group))
filenames_accuracy <- filenames[str_detect(filenames, "accuracy")]
filenames_precisionrms <- filenames[str_detect(filenames, "precisionrms")]
filenames_robustness <- filenames[str_detect(filenames, "robustness")]

# Read Data ---------------------------------------------------------------
df_acc_tot <- data.frame()
for (i in filenames_accuracy) {
  filename <- i
  
  df_acc_temp <- read.table(here("data", "preproc_2", age_group, filename), header = T, sep = "\t") |>
    mutate(age_group = age_group)
  
  df_acc_tot <- df_acc_tot |>
    bind_rows(df_acc_temp)
  
  rm(df_acc_temp)
}

df_precrms_tot <- data.frame()
for (i in filenames_precisionrms) {
  filename <- i
  
  df_precrms_temp <- read.table(here("data", "preproc_2", age_group, filename), header = T, sep = "\t") |>
    mutate(age_group = age_group)
  
  df_precrms_tot <- df_precrms_tot |>
    bind_rows(df_precrms_temp)
  
  rm(df_precrms_temp)
}

df_rob_tot <- data.frame()
for (i in filenames_robustness) {
  filename <- i
  
  df_rob_temp <- read.table(here("data", "preproc_2", age_group, filename), header = T, sep = "\t") |>
    mutate(age_group = age_group)
  
  df_rob_tot <- df_rob_tot |>
    bind_rows(df_rob_temp)
  
  rm(df_rob_temp)
}

## Summarize Accuracy ----
if(age_group == "6to18mo"){
  df_acc_tot <- df_acc_tot |> 
    mutate(recording_name = str_replace_all(recording_name, regex("alina", ignore_case = TRUE), "adult"))
}

df_acc_tot_summary_1 <- df_acc_tot |> 
  drop_na(recording_name) |> 
  separate(recording_name, c("nr", "id", "condition"), remove = F) |> 
  group_by(condition, filename, participant_name) |> 
  dplyr::summarize(acc_visd = mean(acc_visd, na.rm = T)) |> 
  ungroup()

df_acc_tot_summary_2 <- df_acc_tot |> 
  distinct() |> 
  drop_na(recording_name) |> 
  separate(recording_name, c("nr", "id", "condition"), remove = F) |> 
  group_by(condition, filename) |> 
  dplyr::summarize(acc_visd = mean(acc_visd, na.rm = T)) |> 
  group_by(condition) |> 
  dplyr::summarize(acc_visd_sd = sd(acc_visd, na.rm = T),
            acc_visd_mean = mean(acc_visd, na.rm = T)) |> 
  ungroup()

## Summarize Precision (RMS) ----
if(age_group == "6to18mo"){
  df_precrms_tot <- df_precrms_tot |> 
    mutate(recording_name = str_replace_all(recording_name, regex("alina", ignore_case = TRUE), "adult"))
}

df_precrms_tot_summary_1 <- df_precrms_tot |> 
  drop_na(recording_name) |> 
  separate(recording_name, c("nr", "id", "condition"), remove = F) |> 
  group_by(condition, filename, participant_name) |> 
  dplyr::summarize(precrms_visd = mean(precrms_visd, na.rm = T)) |> 
  ungroup()

df_precrms_tot_summary_2 <- df_precrms_tot |> 
  drop_na(recording_name) |> 
  separate(recording_name, c("nr", "id", "condition"), remove = F) |> 
  group_by(condition, filename) |> 
  dplyr::summarize(precrms_visd = mean(precrms_visd, na.rm = T)) |> 
  group_by(condition) |> 
  dplyr::summarize(precrms_visd_sd = sd(precrms_visd, na.rm = T),
            precrms_visd_mean = mean(precrms_visd, na.rm = T)) |> 
  ungroup()

## Summarize Robustness ----
# if(age_group == "6to18mo"){
#   df_rob_tot <- df_rob_tot |> 
#     mutate(recording_name = str_replace_all(recording_name, regex("alina", ignore_case = TRUE), "adult"))
# }

df_rob_tot_summary_1 <- df_rob_tot |> 
  mutate(participant_name = Participant.name |> str_replace("Adult_", "")) |> 
  mutate(filename = filename |> str_replace(".tsv","")) |> 
  separate(filename, c("nr", "id", "condition"), remove = F) |> 
  group_by(condition, id, participant_name) |> 
  dplyr::summarize(robustness_left = mean(robustness_left, na.rm = T),
            robustness_right = mean(robustness_right, na.rm = T)) |> 
  ungroup()

df_rob_tot_summary_2 <- df_rob_tot |> 
  mutate(filename = filename |> str_replace(".tsv","")) |> 
  separate(filename, c("nr", "id", "condition"), remove = F) |> 
  group_by(condition, id) |> 
  dplyr::summarize(robustness_left = mean(robustness_left, na.rm = T),
            robustness_right = mean(robustness_right, na.rm = T)) |> 
  ungroup() |> 
  group_by(condition) |> 
  dplyr::summarize(robustness_left_mean = mean(robustness_left, na.rm = T),
            robustness_right_mean = mean(robustness_right, na.rm = T),
            robustness_left_sd = sd(robustness_left, na.rm = T),
            robustness_right_sd = sd(robustness_right, na.rm = T)) |> 
  ungroup()


## Inference Accuracy ----
t.test(df_acc_tot_summary_1 |> filter(condition == "own") |> pull(acc_visd), 
       df_acc_tot_summary_1 |> filter(condition == "infant") |> pull(acc_visd), 
       paired = T) # s (4mo), s (6to18mo)

t.test(df_acc_tot_summary_1 |> filter(condition == "own") |> pull(acc_visd), 
       df_acc_tot_summary_1 |> filter(condition == "adult") |> pull(acc_visd), 
       paired = T) # s (4mo), s (6to18mo)

t.test(df_acc_tot_summary_1 |> filter(condition == "infant") |> pull(acc_visd), 
       df_acc_tot_summary_1 |> filter(condition == "adult") |> pull(acc_visd), 
       paired = T) # ns (4mo), ns (6to18mo)

## Inference Precision ----
t.test(df_precrms_tot_summary_1 |> filter(condition == "own") |> pull(precrms_visd), 
       df_precrms_tot_summary_1 |> filter(condition == "infant") |> pull(precrms_visd), 
       paired = T) # ns (4mo)

t.test(df_precrms_tot_summary_1 |> filter(condition == "own") |> pull(precrms_visd), 
       df_precrms_tot_summary_1 |> filter(condition == "adult") |> pull(precrms_visd), 
       paired = T) # ns (4mo)

t.test(df_precrms_tot_summary_1 |> filter(condition == "infant") |> pull(precrms_visd), 
       df_precrms_tot_summary_1 |> filter(condition == "adult") |> pull(precrms_visd), 
       paired = T) # ns (4mo)

## Inference Robustness ----
t.test(df_rob_tot_summary_1 |> filter(condition == "own") |> rowwise() |>  mutate(robustness = mean(c(robustness_left, robustness_right), na.rm = T)) |> pull(robustness), 
       df_rob_tot_summary_1 |> filter(condition == "infant") |> rowwise() |>  mutate(robustness = mean(c(robustness_left, robustness_right), na.rm = T)) |> pull(robustness), 
       paired = T) 

t.test(df_rob_tot_summary_1 |> filter(condition == "own") |> rowwise() |>  mutate(robustness = mean(c(robustness_left, robustness_right), na.rm = T)) |> pull(robustness), 
       df_rob_tot_summary_1 |> filter(condition == "adult") |> rowwise() |>  mutate(robustness = mean(c(robustness_left, robustness_right), na.rm = T)) |> pull(robustness), 
       paired = T) 

t.test(df_rob_tot_summary_1 |> filter(condition == "adult") |> rowwise() |>  mutate(robustness = mean(c(robustness_left, robustness_right), na.rm = T)) |> pull(robustness), 
       df_rob_tot_summary_1 |> filter(condition == "infant") |> rowwise() |>  mutate(robustness = mean(c(robustness_left, robustness_right), na.rm = T)) |> pull(robustness), 
       paired = T) 


# Plot --------------------------------------------------------------------

# Plot 1 ----
n_acc <- filenames_accuracy |> length()
df_acc_tot_summary_plot1 <- df_acc_tot_summary_2 |> 
  mutate(se = acc_visd_sd / sqrt(n_acc),
         t_crit   = qt(0.975, df = n_acc - 1),   # 0.975 = zweiseitig 95 %
         ci_lower = acc_visd_mean - t_crit * se,
         ci_upper = acc_visd_mean + t_crit * se)

ggplot(df_acc_tot_summary_plot1, aes(x = condition, y = acc_visd_mean)) +
  geom_point(size = 4, colour = "#1f78b4") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = .15, linewidth = .8, colour = "#1f78b4") +
  labs(y = "Accuracy (in visual degrees)",
       x = NULL,
       title = paste("Accuracy with other (adult or infant) or own calibration.", age_group)) +
  theme_minimal()

n_precrms <- filenames_precisionrms |> length()
df_precrms_tot_summary_plot1 <- df_precrms_tot_summary_2 |> 
  mutate(se = precrms_visd_sd / sqrt(n_precrms),
         t_crit   = qt(0.975, df = n_precrms - 1),   # 0.975 = zweiseitig 95 %
         ci_lower = precrms_visd_mean - t_crit * se,
         ci_upper = precrms_visd_mean + t_crit * se)

ggplot(df_precrms_tot_summary_plot1, aes(x = condition, y = precrms_visd_mean)) +
  geom_point(size = 4, colour = "#1f78b4") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = .15, linewidth = .8, colour = "#1f78b4") +
  labs(y = "Precision (in visual degrees)",
       x = NULL,
       title = paste("Precision (RMS) with other (adult or infant) or own calibration.", age_group)) +
  theme_minimal()

# Plot 2 ----
png(here("img", paste0("plot2_accuracy_", age_group, ".png")), width = 2048, height = 1152, res = 300)
ggplot(df_acc_tot_summary_1 |> mutate(condition = factor(condition, levels = c("own", "infant", "adult"))),
       aes(x = condition, y = acc_visd, fill = condition)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.7, size = 2, color = "black") +
  labs(
    title = paste0("FACET_Infant. Accuracy. ", age_group),
    x = "Calibration Condition",
    y = "Accuracy\n(distance from center in visual degrees)"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "adult" = "salmon3",
      "infant" = "grey",
      "own" = "orange")) +
  theme(
    legend.position = "none",  # Remove legend for simplicity
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

png(here("img", paste0("plot2_precisionrms_", age_group, ".png")), width = 2048, height = 1152, res = 300)
ggplot(df_precrms_tot_summary_1 |> mutate(condition = factor(condition, levels = c("own", "infant", "adult"))),
       aes(x = condition, y = precrms_visd, fill = condition)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.7, size = 2, color = "black") +
  labs(
    title = paste0("FACET_Infant. Precision (RMS) ", age_group),
    x = "Calibration Condition",
    y = "Precision (RMS)\n(distance between gaze samples per fixation\nin visual degrees)"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "adult" = "salmon3",
      "infant" = "grey",
      "own" = "orange")) +
  theme(
    legend.position = "none",  # Remove legend for simplicity
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5)
  )
dev.off()

# Plot 3 (per person) ----
png(here("img", paste0("plot3_accuracy_adultwise_", age_group, ".png")), width = 2048, height = 1152, res = 300)
ggplot(
  df_acc_tot_summary_1 |>
    mutate(participant_name = factor(participant_name,
      levels = c("Adult_Luise", "Adult_Alina", "Adult_Maleen", "Adult_Daniela"),
      labels = c("Luise", "Alina", "Maleen", "Daniela"))) |>
    filter(participant_name %in% c("Luise", "Alina", "Maleen", "Daniela")),
  aes(x = participant_name, y = acc_visd, fill = participant_name)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.2,
               outlier.shape = NA,
               alpha = 0.7) +
  geom_jitter(width = 0.1,
              alpha = 0.7,
              size = 2,
              color = "black") +
  labs(
    title = paste0("FACET_Infant. Accuracy. ", age_group),
    x = "Calibration Condition",
    y = "Accuracy\n(distance from center in visual degrees)"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Luise" = "red4",
      "Alina" = "yellow2",
      "Maleen" = "darkgreen",
      "Daniela" = "darkgrey"
    )
  ) +
  theme(legend.position = "none",
        text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))
dev.off()

png(here("img", paste0("plot3_precisionrms_adultwise_", age_group, ".png")), width = 2048, height = 1152, res = 300)

ggplot(
  df_precrms_tot_summary_1 |>
    mutate(participant_name = factor(participant_name,
                                     levels = c("Adult_Luise", "Adult_Alina", "Adult_Maleen", "Adult_Daniela"),
                                     labels = c("Luise", "Alina", "Maleen", "Daniela"))) |>
    filter(participant_name %in% c("Luise", "Alina", "Maleen", "Daniela")),
  aes(x = participant_name, y = precrms_visd, fill = participant_name)) +
  geom_violin(trim = FALSE, alpha = 0.8) +
  geom_boxplot(width = 0.2,
               outlier.shape = NA,
               alpha = 0.7) +
  geom_jitter(width = 0.1,
              alpha = 0.7,
              size = 2,
              color = "black") +
  labs(
    title = paste0("FACET_Infant. Precision (RMS). ", age_group),
    x = "Calibration Condition",
    y = "Precision (RMS)\n(distance from center in visual degrees)"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c(
      "Luise" = "red4",
      "Alina" = "yellow2",
      "Maleen" = "darkgreen",
      "Daniela" = "darkgrey"
    )
  ) +
  theme(legend.position = "none",
        text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))
dev.off()

df_acc_tot_summary_1 |> 
  filter(participant_name %in% c("Adult_Luise", "Adult_Alina", "Adult_Maleen", "Adult_Daniela")) |> 
  group_by(filename, participant_name) |> 
  dplyr::summarize(acc_visd = mean(acc_visd)) |> 
  group_by(participant_name) |> 
  dplyr::summarize(acc_visd_sd = sd(acc_visd),
            acc_visd_mean = mean(acc_visd))

df_precrms_tot_summary_1 |> 
  filter(participant_name %in% c("Adult_Luise", "Adult_Alina", "Adult_Maleen", "Adult_Daniela")) |> 
  group_by(filename, participant_name) |> 
  dplyr::summarize(precrms_visd = mean(precrms_visd)) |> 
  group_by(participant_name) |> 
  dplyr::summarize(precrms_visd_sd = sd(precrms_visd),
            precrms_visd_mean = mean(precrms_visd))

df_rob_tot_summary_1 |> 
  filter(participant_name %in% c("Luise", "Alina", "Maleen", "Daniela")) |> 
  group_by(id, participant_name) |> 
  dplyr::summarize(robustness_left = mean(robustness_left),
            robustness_right = mean(robustness_right)) |> 
  group_by(participant_name) |> 
  dplyr::summarize(robustness_left_mean = mean(robustness_left, na.rm = T),
            robustness_left_sd = sd(robustness_left, na.rm = T))
  # dplyr::summarize(robustness_right_mean = mean(robustness_right, na.rm = T),
  #           robustness_right_sd = sd(robustness_right, na.rm = T))


# Plots for ILCD poster & paper ----

## Accuracy ----
df_plot <- df_acc_tot_summary_1 |> 
  mutate(
    condition_lab = dplyr::recode(condition,
                                  "own"    = "Own (5p)",
                                  "adult"  = "Human Adult (9p)",
                                  "infant" = "Other Infant (9p)"
    ),
    condition_lab = factor(condition_lab, levels = c("Own (5p)","Other Infant (9p)","Human Adult (9p)"))
  )

if(age_group == "6to18mo"){
  pal <- c(
    "Own (5p)"          = "#E69F00",  # Blau #0072B2
    "Human Adult (9p)" = "#E69F00",  # Orange #E69F00
    "Other Infant (9p)"= "#E69F00"   # Grün #009E73
  )
}

if(age_group == "4mo"){
  pal <- c(
    "Own (5p)"          = "#0072B2",  # Blau #0072B2
    "Human Adult (9p)" = "#0072B2",  # Orange #E69F00
    "Other Infant (9p)"= "#0072B2"   # Grün #009E73
  )
}

g_acc <- ggplot(df_plot, aes(x = condition_lab, y = acc_visd)) +
  geom_violin(aes(fill = condition_lab), trim = FALSE, width = 0.85, alpha = 0.25, color = NA) +
  geom_jitter(aes(color = condition_lab), width = 0.08, height = 0, size = 1.7, alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", size = 3.2, color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.12, 
               color = "black", linewidth = 0.8) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(-0.75, 5), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Calibration",
    y = "<span style='font-size:12pt; font-weight:600'>Accuracy (in Visual Degrees)</span><br>
       <span style='font-size:9pt; font-weight:400'>
       Distance From Stimulus Center
       </span>"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.y = ggtext::element_markdown(size = 12, lineheight = 1.05, angle = 90)
  )

png(here("img", paste0("posterplot_accuracy_", age_group, ".png")), width = 1000, height = 900, res = 200)
g_acc
dev.off()

png(here("img", paste0("paperplot_accuracy_", age_group, ".png")), width = 1152, height = 1152, res = 250)
g_acc
dev.off()

## Precision (RMS) ----
df_plot <- df_precrms_tot_summary_1 |> 
  mutate(
    condition_lab = recode(condition,
                           "own"    = "Own (5p)",
                           "adult"  = "Human Adult (9p)",
                           "infant" = "Other Infant (9p)"
    ),
    condition_lab = factor(condition_lab, levels = c("Own (5p)","Other Infant (9p)","Human Adult (9p)"))
  )

if(age_group == "6to18mo"){
  pal <- c(
    "Own (5p)"          = "#E69F00",  # Blau #0072B2
    "Human Adult (9p)" = "#E69F00",  # Orange #E69F00
    "Other Infant (9p)"= "#E69F00"   # Grün #009E73
  )
}

if(age_group == "4mo"){
  pal <- c(
    "Own (5p)"          = "#0072B2",  # Blau #0072B2
    "Human Adult (9p)" = "#0072B2",  # Orange #E69F00
    "Other Infant (9p)"= "#0072B2"   # Grün #009E73
  )
}

g_precrms <- ggplot(df_plot, aes(x = condition_lab, y = precrms_visd)) +
  geom_violin(aes(fill = condition_lab), trim = FALSE, width = 0.85, alpha = 0.25, color = NA) +
  geom_jitter(aes(color = condition_lab), width = 0.08, height = 0, size = 1.7, alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", size = 3.2, color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.12, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(-0.75, 5), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Calibration",
    y = "<span style='font-size:12pt; font-weight:600'>Precision (in Visual Degrees)</span><br>
       <span style='font-size:9pt; font-weight:400'>
       Root Mean Square Deviation Between Gaze Samples
       Within Fixations
       </span>"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.y = ggtext::element_markdown(size = 12, lineheight = 1.05, angle = 90)
  )

png(here("img", paste0("posterplot_precisionrms_", age_group, ".png")), width = 1000, height = 900, res = 200)
g_precrms
dev.off()

png(here("img", paste0("paperplot_precisionrms_", age_group, ".png")), width = 1152, height = 1152, res = 250)
g_precrms
dev.off()

## Robustness ----
df_plot <- df_rob_tot_summary_1 |> 
  mutate(
    condition_lab = recode(condition,
                           "own"    = "Own (5p)",
                           "adult"  = "Human Adult (9p)",
                           "infant" = "Other Infant (9p)"
    ),
    condition_lab = factor(condition_lab, levels = c("Own (5p)","Other Infant (9p)","Human Adult (9p)"))
  )

if(age_group == "6to18mo"){
  pal <- c(
    "Own (5p)"          = "#E69F00",  # Blau #0072B2
    "Human Adult (9p)" = "#E69F00",  # Orange #E69F00
    "Other Infant (9p)"= "#E69F00"   # Grün #009E73
  )
}

if(age_group == "4mo"){
  pal <- c(
    "Own (5p)"          = "#0072B2",  # Blau #0072B2
    "Human Adult (9p)" = "#0072B2",  # Orange #E69F00
    "Other Infant (9p)"= "#0072B2"   # Grün #009E73
  )
}

g_rob <- ggplot(df_plot |> rowwise() |>  mutate(robustness = mean(c(robustness_left, robustness_right), na.rm = T)), 
                aes(x = condition_lab, y = robustness)) +
  geom_violin(aes(fill = condition_lab), trim = FALSE, width = 0.85, alpha = 0.25, color = NA) +
  geom_jitter(aes(color = condition_lab), width = 0.08, height = 0, size = 1.7, alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", size = 3.2, color = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.12, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(-600, 3000), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Calibration",
    y = "<span style='font-size:12pt; font-weight:600'>Robustness (in ms)</span><br>
       <span style='font-size:9pt; font-weight:400'>
       Mean Duration of Consecutive Valid Gaze Samples
       </span>"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.y = ggtext::element_markdown(size = 12, lineheight = 1.05, angle = 90)
  )

png(here("img", paste0("posterplot_robustness_", age_group, ".png")), width = 1000, height = 900, res = 200)
g_rob
dev.off()

png(here("img", paste0("paperplot_robustness_", age_group, ".png")), width = 1152, height = 1152, res = 250)
g_rob
dev.off()

## GLMM for Julian ----
full_gamma <- glmer(acc_visd ~ condition + (1 | filename), data = df_acc_tot_summary_1, family = Gamma(link = "log"))
red_gamma <- glmer(acc_visd ~  (1 | filename), data = df_acc_tot_summary_1, family = Gamma(link = "log"))
as.data.frame(anova(full_gamma, red_gamma, test = "Chisq")) # s

