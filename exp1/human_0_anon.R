## This script anonymizes eye-tracking data from exp1/data/raw.
## It requires data located in the "raw_private" folder,
## which is not included in the GitHub repository for data protection reasons.
## Nov 14 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)

# Functions ---------------------------------------------------------------
anonymize_file <- function(file, id, rec_col  = "Recording.name", part_col = "Participant.name",
                             drop_cols = c()) {
  rec_sym  <- sym(rec_col)
  part_sym <- sym(part_col)
  
  df <- read.table(file, header = T, sep = "\t") |> 
    mutate(!!rec_sym  := id, !!part_sym := id) |>
    select(-any_of(drop_cols))
  
  message("Done: ", basename(file))
  df
}

# Anonymization -----------------------------------------------------------
for(i in list.files(here("exp1", "data", "raw"))){
  files <- list.files(here("exp1", "data", "raw", i))

  for(j in files){
    dat_temp <- anonymize_file(file = here("exp1", "data", "raw", i, j), id = which(files == j), 
                               rec_col  = "Recording.name", part_col = "Participant.name",
                               drop_cols = c("Export.date", "Recording.date.UTC", "Recording.start.time", "Recording.start.time.UTC", "Recording.date", "Recording.Time"))
    write.table(dat_temp, here("exp1", "data", "raw_public", i, paste0(i, "_", which(files == j), ".tsv")),
                row.names = F, quote = F, sep = "\t", dec = ".")
    rm(dat_temp)
  }
  message("Done folder: ", i)
}
