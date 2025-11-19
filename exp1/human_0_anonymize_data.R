## This script anonymizes eye-tracking data from exp1/data/raw.
## It requires data located in the "raw_private" folder,
## which is not included in the GitHub repository for data protection reasons.
## Nov 17 2025 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)

# Functions ---------------------------------------------------------------
source(here("exp1", "R", "anonymization.R"))

# Anonymization -----------------------------------------------------------
for(i in list.files(here("exp1", "data", "raw_private"))){
  
  if (i == "chimps") {
    next # chimps don't need to get anonymized
  }
  
  files <- list.files(here("exp1", "data", "raw_private", i))
  
  for(j in files){
    dat_temp <- anonymize_file(file = here("exp1", "data", "raw_private", i, j), id = which(files == j), 
                               id_cols  = c("Recording.name", "Participant.name"),
                               drop_cols = c("Export.date", "Recording.date.UTC", "Recording.start.time", "Recording.start.time.UTC", "Recording.date"))
    write.table(dat_temp, here("exp1", "data", "raw_public", i, paste0(i, "_", which(files == j), ".tsv")),
                row.names = F, quote = F, sep = "\t", dec = ".")
    readr::problems(dat_temp)
    rm(dat_temp)
  }
  message("Done folder: ", i)
}
