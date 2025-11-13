# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Adjust Parameter --------------------------------------------------------
folder <- "chimps_9p"
filenames <- list.files(path = here("data", "raw_included_1", folder))

for(i in 1:16){
  nr <- i # nr 1 is first ape

# Define AOIs -------------------------------------------------------------
# Remark: AOIs emtail stimulus size + 2°/80px per side
# The plus and minus adds two more 2° per side

## Objects ----
aoi_topobject_x_topleft <- 790-80
aoi_topobject_y_topleft <- 0-80
aoi_topobject_x_botright <- 1130+80
aoi_topobject_y_botright <- 340+80

aoi_botobject_x_topleft <- 790-80
aoi_botobject_y_topleft <- 740-80
aoi_botobject_x_botright <- 1130+80
aoi_botobject_y_botright <- 1080+80

## Popflakes ----
aoi_topleftflake_x_topleft <- 300-80
aoi_topleftflake_y_topleft <- 90-80
aoi_topleftflake_x_botright <- 660+80
aoi_topleftflake_y_botright <- 450+80

aoi_botleftflake_x_topleft <- 300-80
aoi_botleftflake_y_topleft <- 630-80
aoi_botleftflake_x_botright <- 660+80
aoi_botleftflake_y_botright <- 990+80

aoi_toprightflake_x_topleft <- 1260-80
aoi_toprightflake_y_topleft <- 90-80
aoi_toprightflake_x_botright <- 1620+80
aoi_toprightflake_y_botright <- 450+80

aoi_botrightflake_x_topleft <- 1260-80
aoi_botrightflake_y_topleft <- 630-80
aoi_botrightflake_x_botright <- 1620+80
aoi_botrightflake_y_botright <- 990+80

aoi_centralflake_x_topleft <- 780-80
aoi_centralflake_y_topleft <- 360-80
aoi_centralflake_x_botright <- 1140+80
aoi_centralflake_y_botright <- 720+80

## Attention Getter ----
aoi_at_x_topleft <- 783-80
aoi_at_y_topleft <- 363-80
aoi_at_x_botright <- 1137+80
aoi_at_y_botright <- 717+80

# Read Data ---------------------------------------------------------------
raw <- read.csv(here("data", "raw_included_1", folder, filenames[nr]), header = T, sep = "\t")
df <- raw

# Data Preparation --------------------------------------------------------

# Due to export mistake
if(filenames[nr] == "alex.tsv"){df <- df |> 
  filter(Recording.name != "session1_carola_alex")
}

if(folder == "chimps_2p" &
   filenames[nr] == "alex.tsv"){df <- df |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession\\b", "session2")) |>
  arrange(Recording.name)}

df <- df |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession1\\b", "session01")) |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession2\\b", "session02")) |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession3\\b", "session03")) |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession4\\b", "session04")) |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession5\\b", "session05")) |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession6\\b", "session06")) |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession7\\b", "session07")) |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession8\\b", "session08")) |>
  mutate(Recording.name = str_replace(Recording.name, "\\bsession9\\b", "session09"))
  

## Separate information about trials, etc. ----
df <- df |> mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "pre_", "pre"))
df <- df |> mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "mid_", "mid"))
df <- df |> mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "post_", "post"))

df <- df |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_cajotask", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_1_joint", "")) |>
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_2_joint", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_3_joint", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_4_joint", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_1_parallel", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_2_parallel", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_3_parallel", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_4_parallel", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "incon", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "con", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "__A", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "__B", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_20s", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "_1s", "")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "at1_up", "at1_center")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "at2_up", "at2_center")) |>  
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "at1_down", "at1_center")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "at2_down", "at2_center"))

df <- df |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "checkflake_top_left", "checkflake_topleft_x_x")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "checkflake_bot_left", "checkflake_botleft_x_x")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "checkflake_top_right", "checkflake_topright_x_x")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "checkflake_bot_right", "checkflake_botright_x_x")) |> 
  mutate(Presented.Stimulus.name = str_replace(Presented.Stimulus.name, "center_center", "centercenter_x_x"))

df <- df |> 
  separate(Presented.Stimulus.name, into = c("session", "trial", "stimulus", "position", "delete1", "delete2"), remove = F) |> 
  rename(fix_x = Fixation.point.X,
         fix_y = Fixation.point.Y)

df[which(df$stimulus %in% c("at1", "at2")), "position"] <- "center"
df$delete1 <- NULL
df$delete2 <- NULL

## Add gaze-sample duration ----
df$gaze_sample_duration <- c(diff(df$Recording.timestamp), NA) / 1000

# Add cumulative duration per stimulus ----
df <- df |> 
  group_by(trial, stimulus) |> 
  mutate(timeline = cumsum(gaze_sample_duration))

# Remove delay after contingency was elicted
df <- df |> 
  filter(!(((stimulus == "checkflake" & grepl("20s", Presented.Media.name)) | (stimulus == "at2") | (stimulus == "still")) & 
             (timeline > 1000)))

# df |> group_by(Presented.Media.name, stimulus) |> summarize(max(timeline)) |> View() # Check

## Add trial ----
trials <- df |> 
  ungroup() |> 
  select(position, trial, Recording.name) |> 
  distinct() |> 
  drop_na(position) |> 
  arrange(Recording.name) |> 
  mutate(trial2 = 1:n())

df <- df |> 
  ungroup() |> 
  left_join(trials, by = c("position", "trial", "Recording.name"))

df |> select(trial, trial2, Presented.Stimulus.name) |> distinct()
df$trial <- df$trial2
df$trial2 <- NULL

## Define position_dodge()
## Define AOI ----
df$aoi <- "not_in_aoi"

# Popflakes
topleftflake <- which(df$fix_x >= (aoi_topleftflake_x_topleft) & df$fix_x <= (aoi_topleftflake_x_botright) & 
                        df$fix_y >= (aoi_topleftflake_y_topleft) & df$fix_y <= (aoi_topleftflake_y_botright) &
                        df$stimulus %in% c("checkflake"))
df[topleftflake,"aoi"] <- "top_left"
if(nrow(df[which(df$aoi == "top_left" & df$position != "topleft"),"aoi"]) > 0){
  df[which(df$aoi == "top_left" & df$position != "topleft"),"aoi"] <- NA}

botleftflake <- which(df$fix_x >= (aoi_botleftflake_x_topleft) & df$fix_x <= (aoi_botleftflake_x_botright) & 
                        df$fix_y >= (aoi_botleftflake_y_topleft) & df$fix_y <= (aoi_botleftflake_y_botright) &
                        df$stimulus %in% c("checkflake"))
df[botleftflake,"aoi"] <- "bot_left"
if(nrow(df[which(df$aoi == "bot_left" & df$position != "botleft"),"aoi"]) > 0){
  df[which(df$aoi == "bot_left" & df$position != "botleft"),"aoi"] <- NA}

toprightflake <- which(df$fix_x >= (aoi_toprightflake_x_topleft) & df$fix_x <= (aoi_toprightflake_x_botright) & 
                         df$fix_y >= (aoi_toprightflake_y_topleft) & df$fix_y <= (aoi_toprightflake_y_botright) &
                         df$stimulus %in% c("checkflake"))
df[toprightflake,"aoi"] <- "top_right"
if(nrow(df[which(df$aoi == "top_right" & df$position != "topright"),"aoi"]) > 0){
  df[which(df$aoi == "top_right" & df$position != "topright"),"aoi"] <- "not_in_aoi"}

botrightflake <- which(df$fix_x >= (aoi_botrightflake_x_topleft) & df$fix_x <= (aoi_botrightflake_x_botright) & 
                         df$fix_y >= (aoi_botrightflake_y_topleft) & df$fix_y <= (aoi_botrightflake_y_botright) &
                         df$stimulus %in% c("checkflake"))
df[botrightflake,"aoi"] <- "bot_right"
if(nrow(df[which(df$aoi == "bot_right" & df$position != "botright"),"aoi"]) > 0){
  df[which(df$aoi == "bot_right" & df$position != "botright"),"aoi"] <- "not_in_aoi"}

centralflake <- which(df$fix_x >= (aoi_centralflake_x_topleft) & df$fix_x <= (aoi_centralflake_x_botright) & 
                        df$fix_y >= (aoi_centralflake_y_topleft) & df$fix_y <= (aoi_centralflake_y_botright) &
                        df$stimulus %in% c("checkflake"))
df[centralflake,"aoi"] <- "center_center"
if(nrow(df[which(df$aoi == "center_center" & df$position != "centercenter"),"aoi"]) > 0){
  df[which(df$aoi == "center_center" & df$position != "centercenter"),"aoi"] <- "not_in_aoi"}

# Attention getter
at <- which(df$fix_x >= (aoi_at_x_topleft) & df$fix_x <= (aoi_at_x_botright) & 
              df$fix_y >= (aoi_at_y_topleft) & df$fix_y <= (aoi_at_y_botright) &
              df$stimulus %in% c("at1", "at2"))
df[at,"aoi"] <- "at"
if(nrow(df[which(df$aoi == "at" & df$position != "center"),"aoi"]) > 0){
  df[which(df$aoi == "at" & df$position != "center"),"aoi"] <- "not_in_aoi"}

# Objects
top_object <- which(df$fix_x >= (aoi_topobject_x_topleft) & df$fix_x <= (aoi_topobject_x_botright) & 
                      df$fix_y >= (aoi_topobject_y_topleft) & df$fix_y <= (aoi_topobject_y_botright) &
                      df$stimulus %in% c("move", "still"))
df[top_object,"aoi"] <- "top"
if(nrow(df[which(df$aoi == "top" & df$position != "up"),"aoi"]) > 0){
  df[which(df$aoi == "top" & df$position != "up"),"aoi"] <- "not_in_aoi"}

bot_object <- which(df$fix_x >= (aoi_botobject_x_topleft) & df$fix_x <= (aoi_botobject_x_botright) & 
                      df$fix_y >= (aoi_botobject_y_topleft) & df$fix_y <= (aoi_botobject_y_botright) &
                      df$stimulus %in% c("move", "still"))
df[bot_object,"aoi"] <- "bottom"
if(nrow(df[which(df$aoi == "bottom" & df$position != "down"),"aoi"]) > 0){
  df[which(df$aoi == "bottom" & df$position != "down"),"aoi"] <- "not_in_aoi"}

# Rename stimulus object 
df <- df |> 
  mutate(stimulus = str_replace(stimulus, "move", "object"),
         stimulus = str_replace(stimulus, "still", "object"),
         stimulus = str_replace(stimulus, "at1", "at"),
         stimulus = str_replace(stimulus, "at2", "at"))

# For a trial to be included, individuals must look in the AOI of the stimulus (see “Other” for details) 
# for at least one fixation.
# Output: al included stimuli
included_trials <- df |> 
  ungroup() |> 
  drop_na(stimulus) |> 
  filter(aoi != "not_in_aoi") |> 
  select(trial, stimulus, position) |> 
  distinct() |> 
  pull(trial)

# Filter only included trials
df <- df |> 
  filter(trial %in% included_trials)

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
write.table(df, here("data", "raw_included_2", folder, filenames[nr]), row.names = F, quote = F, sep = "\t", dec = ".")
write.table(df, here("data", "preproc_included_1", folder, filenames[nr]), row.names = F, quote = F, sep = "\t", dec = ".")
write.table(df_calibration, here("data", "calibration_tobii", folder, paste0("calibration_tobii_", sub("\\.tsv$", "", filenames[nr]), ".txt")), 
            row.names = F, quote = F, sep = "\t", dec = ".")

rm(df, df_calibration)
print(i)
}
