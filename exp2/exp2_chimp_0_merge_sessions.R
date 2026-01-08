## This script merges all single sessions files of one ape to one file.
## It requires data located in the folder "raw_1" and a subfolder 
# "alex_calibration_5p" or"human_calibration_9p".
## Jan 07 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Adjust Parameter --------------------------------------------------------
folder <- "human_calibration_9p" # alex_calibration_5p or human_calibration_9p or ape_calibration_2p
filenames <- list.files(path = here("exp2", "data", "raw_1", folder))
apes <-  sub("_.*", "", filenames) |> unique()
ape_size <- ifelse(folder %in% c("alex_calibration_5p", "human_calibration_9p"), 16, 17)

# Functions ---------------------------------------------------------------
source(here("exp2", "R", "cleaning.R"))

# Merge Sessions ----------------------------------------------------------
for(i in 1:ape_size){
  nr <- i # nr = 1 means first ape
  
  # Read data
  df <- data.frame()
  for(j in filenames |> str_subset(apes[nr]) |> sort()){
    df_temp <- read.table(here("exp2", "data", "raw_1", folder, j), header = TRUE, sep = "\t")
    
    # Correct naming of sessions
    df_temp <- correct_session_name(df_temp)
    
    df <- df |> 
      bind_rows(df_temp) # Bind/ merge data
    rm(df_temp)
  }
  
  # Write data
  write.table(df, here("exp2", "data", "raw_2", folder, paste0(apes[nr], ".tsv")), 
              row.names = F, quote = F, sep = "\t", dec = ".")
  rm(df)
  print(i)
}