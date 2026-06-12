## This script anonymizes eye-tracking data from exp3/data/raw.
## It requires data located in the "raw_1" folder,
## which is not included in the GitHub repository for data protection reasons.
## June 11 2026 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)

# Functions ---------------------------------------------------------------
source(here("exp3", "R", "anonymization.R"))

# Anonymization -----------------------------------------------------------
for(i in list.files(here("exp3", "data", "raw_1"))){
  
  files <- list.files(here("exp3", "data", "raw_1", i))
  
  for(j in files){
    # Read data
    df <- read.table(here("exp3", "data", "raw_1", i, j), header = T, sep = "\t")

    # Correct export mistake
    if(j == "1_284456.tsv"){
      df <- df |> filter(!(Recording.name %in% c("1_123456_adult", "1_123456_infant")))
      df <- df |> filter(Participant.name != "Adult_Luise")
    }
    
    if(j == "1_284456.tsv"){
      df <- df |> 
        filter(Recording.name %in% c("1_284456_adult", "1_284456_infant", "1_284456_own"))
    }
    
    # Remove: c("Export.date", "Recording.date.UTC", "Recording.start.time", "Recording.start.time.UTC", "Recording.date")
    df <- df |>
      select(-c("Export.date", "Recording.date.UTC", "Recording.start.time", "Recording.start.time.UTC", "Recording.date"))
    
    # Remove ID number in recording name
    df$Recording.name <- sub("^([0-9]+)_[0-9]+_(.*)$", "\\1_\\2", df$Recording.name)
    
    # Remove ID number in participant name
    df <- df |>
      mutate(Participant.name = case_when(
        str_detect(Participant.name, "^Adult_") ~ str_remove(Participant.name, "^Adult_") |> str_to_lower(),
        str_detect(Participant.name, "^Infant") ~ str_replace(Participant.name, "^Infant", "peer"),
        str_detect(Participant.name, "^\\d+_") ~ "own",
        TRUE ~ Participant.name
        )
      )
    print(paste("----", j, "----"))
    print(df$Participant.name |> unique())
    print(df$Recording.name |> unique())
    
    write.table(df, here("exp3", "data", "raw_2", i, paste0(i, "_", which(files == j), ".tsv")), row.names = F, quote = F, sep = "\t", dec = ".")
  }

}
