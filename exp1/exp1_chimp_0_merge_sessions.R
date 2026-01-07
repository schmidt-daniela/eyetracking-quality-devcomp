## This script merges all single sessions files of one ape to one file.
## It requires data located in the "raw_1" folder.
## Nov 14 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Adjust Parameter --------------------------------------------------------
folder <- "chimps"
filenames <- list.files(path = here("exp1", "data", "raw_1", folder))
apes <-  sub("_.*", "", filenames) |> unique()

# Functions ---------------------------------------------------------------
source(here("exp1", "R", "cleaning.R"))

# Merge Sessions ----------------------------------------------------------
for(i in 1:17){
  nr <- i # nr = 1 means first ape
  
  # Read data
  df <- data.frame()
  for(j in filenames |> str_subset(apes[nr]) |> sort()){
    df_temp <- read.table(here("exp1", "data", "raw_1", folder, j), header = TRUE, sep = "\t")
    
    # Correct export mistake
    if(j == "alex_session1.tsv"){df_temp <- df_temp |> mutate(Recording.name = "session1")}
    if(j == "alex_session2.tsv"){df_temp <- df_temp |> mutate(Recording.name = "session2")}
    
    # Correct naming of sessions
    df_temp <- correct_session_name(df_temp)
    
    df <- df |> 
      bind_rows(df_temp) # Bind/ merge data
    rm(df_temp)
  }
  
  # Write data
  write.table(df, here("exp1", "data", "raw_2", folder, paste0(apes[nr], ".tsv")), 
              row.names = F, quote = F, sep = "\t", dec = ".")
  rm(df)
  print(i)
}