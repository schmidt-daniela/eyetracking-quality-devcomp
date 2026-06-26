# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Adjust Parameter --------------------------------------------------------
age_group <- "4mo" # "4mo" or "6to18mo"
filenames <- list.files(path = here("data", "raw_included_blink", age_group))

for(i in 1:length(filenames)){
  nr <- i # nr 1 is first participant

  # Define AOIs -------------------------------------------------------------
  # Remark: AOIs entail stimulus size + 1°/40px per side
  # The plus and minus adds two more 1° per side
  
  ## Objects ----
  aoi_topobject_x_topleft <- 790 - 40
  aoi_topobject_y_topleft <- 0 - 40
  aoi_topobject_x_botright <- 1130 + 40
  aoi_topobject_y_botright <- 340 + 40
  
  aoi_botobject_x_topleft <- 790 - 40
  aoi_botobject_y_topleft <- 740 - 40
  aoi_botobject_x_botright <- 1130 + 40
  aoi_botobject_y_botright <- 1080 + 40
  
  ## Popflakes ----
  aoi_topleftflake_x_topleft <- 300 - 40
  aoi_topleftflake_y_topleft <- 90 - 40
  aoi_topleftflake_x_botright <- 660 + 40
  aoi_topleftflake_y_botright <- 450 + 40
  
  aoi_botleftflake_x_topleft <- 300 - 40
  aoi_botleftflake_y_topleft <- 630 - 40
  aoi_botleftflake_x_botright <- 660 + 40
  aoi_botleftflake_y_botright <- 990 + 40
  
  aoi_toprightflake_x_topleft <- 1260 - 40
  aoi_toprightflake_y_topleft <- 90 - 40
  aoi_toprightflake_x_botright <- 1620 + 40
  aoi_toprightflake_y_botright <- 450 + 40
  
  aoi_botrightflake_x_topleft <- 1260 - 40
  aoi_botrightflake_y_topleft <- 630 - 40
  aoi_botrightflake_x_botright <- 1620 + 40
  aoi_botrightflake_y_botright <- 990 + 40
  
  aoi_centralflake_x_topleft <- 780 - 40
  aoi_centralflake_y_topleft <- 360 - 40
  aoi_centralflake_x_botright <- 1140 + 40
  aoi_centralflake_y_botright <- 720 + 40
  
  ## Attention Getter ----
  aoi_at_x_topleft <- 783 - 40
  aoi_at_y_topleft <- 363 - 40
  aoi_at_x_botright <- 1137 + 40
  aoi_at_y_botright <- 717 + 40
  
# Read Data ---------------------------------------------------------------
raw <- read.csv(here("data", "raw_included_blink", age_group, filenames[nr]), header = T, sep = "\t")
df <- raw

# Data Preparation --------------------------------------------------------
# Rename stimulus names
df <- df |> 
  filter(!(Presented.Stimulus.name %in% c("directgaze_right", "shift_right", "objectgaze_right", 
                                          "directgaze", "shift", "objectgaze", "Eyetracker Calibration", ""))) |> 
  filter(!str_detect(Presented.Stimulus.name, "cajo2pilot")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "mid_", "mid")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "pre_", "pre")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "post_", "post")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "cajotask_", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "still", "object")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "move", "object")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "mid_", "mid")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "mid_", "mid")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "top_left", "topleft")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "bot_left", "botleft")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "top_right", "topright")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "bot_right", "botright")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "center_center", "centercenter")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "up_", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "down_", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "2s_", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "30s_", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "incon_", "")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "con_", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_A", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_B", "")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_left", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_right", "")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_1_", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_2_", "")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_3_", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_4_", "")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "parallel", "")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "joint", "")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_first", "")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "checkflake", "checkflake_d"))

df <- df |> 
  separate(Presented.Stimulus.name, into = c("trial", "stimulus", "delete", "object_location"), remove = F) |> 
  rename(fix_x = Fixation.point.X,
         fix_y = Fixation.point.Y)

# Fix order
recording_order <- df |> 
  group_by(Recording.name) |> 
  slice(1) |> 
  ungroup() |> 
  arrange(Computer.timestamp) |> 
  pull(Recording.name)

df <- df |>
  filter(Recording.name == recording_order[1]) |> 
  bind_rows(df |> filter(Recording.name == recording_order[2])) |> 
  bind_rows(df |> filter(Recording.name == recording_order[3]))

## Add gaze-sample duration ----
df$gaze_sample_duration <- c(diff(df$Recording.timestamp), NA) / 1000

# Add cumulative duration per stimulus ----
df <- df |> 
  group_by(trial, stimulus) |> 
  mutate(timeline = cumsum(gaze_sample_duration))

## Exclude CAJO task
df <- df |> 
  filter(!trial %in% c("directgaze", "shift", "objectgaze"))

## Define AOI ----
df$aoi <- "not_in_aoi"

# Popflakes
topleftflake <- which(df$fix_x >= (aoi_topleftflake_x_topleft) & df$fix_x <= (aoi_topleftflake_x_botright) & 
                        df$fix_y >= (aoi_topleftflake_y_topleft) & df$fix_y <= (aoi_topleftflake_y_botright) &
                        df$stimulus %in% c("checkflake"))
df[topleftflake,"aoi"] <- "topleft"

botleftflake <- which(df$fix_x >= (aoi_botleftflake_x_topleft) & df$fix_x <= (aoi_botleftflake_x_botright) & 
                        df$fix_y >= (aoi_botleftflake_y_topleft) & df$fix_y <= (aoi_botleftflake_y_botright) &
                        df$stimulus %in% c("checkflake"))
df[botleftflake,"aoi"] <- "botleft"

toprightflake <- which(df$fix_x >= (aoi_toprightflake_x_topleft) & df$fix_x <= (aoi_toprightflake_x_botright) & 
                         df$fix_y >= (aoi_toprightflake_y_topleft) & df$fix_y <= (aoi_toprightflake_y_botright) &
                         df$stimulus %in% c("checkflake"))
df[toprightflake,"aoi"] <- "topright"

botrightflake <- which(df$fix_x >= (aoi_botrightflake_x_topleft) & df$fix_x <= (aoi_botrightflake_x_botright) & 
                         df$fix_y >= (aoi_botrightflake_y_topleft) & df$fix_y <= (aoi_botrightflake_y_botright) &
                         df$stimulus %in% c("checkflake"))
df[botrightflake,"aoi"] <- "botright"

centralflake <- which(df$fix_x >= (aoi_centralflake_x_topleft) & df$fix_x <= (aoi_centralflake_x_botright) & 
                        df$fix_y >= (aoi_centralflake_y_topleft) & df$fix_y <= (aoi_centralflake_y_botright) &
                        df$stimulus %in% c("checkflake"))
df[centralflake,"aoi"] <- "centercenter"

# Attention getter
at <- which(df$fix_x >= (aoi_at_x_topleft) & df$fix_x <= (aoi_at_x_botright) & 
              df$fix_y >= (aoi_at_y_topleft) & df$fix_y <= (aoi_at_y_botright) &
              df$stimulus == "at")
df[at,"aoi"] <- "at" 

# Objects
top_object <- which(df$fix_x >= (aoi_topobject_x_topleft) & df$fix_x <= (aoi_topobject_x_botright) & 
                      df$fix_y >= (aoi_topobject_y_topleft) & df$fix_y <= (aoi_topobject_y_botright) &
                      df$stimulus %in% c("object"))
df[top_object,"aoi"] <- "top"

bot_object <- which(df$fix_x >= (aoi_botobject_x_topleft) & df$fix_x <= (aoi_botobject_x_botright) & 
                      df$fix_y >= (aoi_botobject_y_topleft) & df$fix_y <= (aoi_botobject_y_botright) &
                      df$stimulus %in% c("object"))
df[bot_object,"aoi"] <- "bottom"

df[df$stimulus == "at" & !is.na(df$stimulus),"object_location"] <- "at"

# Add trials
trials <- df |> 
  ungroup() |> 
  select(object_location, trial, Recording.name) |> 
  distinct() |> 
  mutate(trial2 = 1:n())

df <- df |> 
  left_join(trials, by = c("object_location", "trial", "Recording.name"))

# For a trial to be included, individuals must look in the AOI of the stimulus (see “Other” for details) 
# for at least one fixation.
# Output: al included stimuli
included_trials <- df |> 
  ungroup() |> 
  drop_na(stimulus) |> 
  filter(aoi != "not_in_aoi") |> 
  select(trial2, stimulus, object_location, Recording.name) |> 
  distinct() |> 
  pull(trial2)

# Filter only included trials
df <- df |> 
  filter(trial2 %in% included_trials)

## Calibration ----
df_calibration <- df |> 
  select(Recording.name, Timeline.name, matches("validation|calibration", ignore.case = TRUE)) |> 
  distinct() |> 
  rename(rec_name = Recording.name, 
         experiment = Timeline.name, 
         cal_acc_mm = Average.calibration.accuracy..mm., 
         cal_precsd_mm = Average.calibration.precision.SD..mm.,
         cal_precrms_mm = Average.calibration.precision.RMS..mm., 
         cal_acc_deg = Average.calibration.accuracy..degrees.,
         cal_precsd_deg = Average.calibration.precision.SD..degrees., 
         cal_precrms_deg = Average.calibration.precision.RMS..degrees.,
         cal_acc_px = Average.calibration.accuracy..pixels., 
         cal_precsd_px = Average.calibration.precision.SD..pixels.,
         cal_precrms_px = Average.calibration.precision.RMS..pixels., 
         val_acc_mm = Average.validation.accuracy..mm.,
         val_precsd_mm = Average.validation.precision.SD..mm., 
         val_precrms_mm = Average.validation.precision.RMS..mm.,
         val_acc_deg = Average.validation.accuracy..degrees., 
         val_precsd_deg = Average.validation.precision.SD..degrees.,
         val_precrms_deg = Average.validation.precision.RMS..degrees., 
         val_acc_px = Average.validation.accuracy..pixels.,
         val_precsd_px = Average.validation.precision.SD..pixels., 
         val_precrms_px = Average.validation.precision.RMS..pixels.)

# Write Data --------------------------------------------------------------
write.table(df, here("data", "preproc_1", age_group, filenames[nr]), row.names = F, quote = F, sep = "\t", dec = ".")
write.table(df_calibration, here("data", "calibration_tobii", age_group, paste0("calibration_tobii_", sub("\\.tsv$", "", filenames[nr]), ".txt")), 
            row.names = F, quote = F, sep = "\t", dec = ".")

rm(df, df_calibration)
print(i)
}
