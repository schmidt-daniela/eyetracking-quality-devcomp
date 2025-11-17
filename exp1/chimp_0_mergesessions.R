## This script merges all single sessions files of one ape to one file.excludes invalid trials.
## It requires data located in the "raw_private" folder.
## Nov 14 – Daniela Schmidt

# General -----------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Adjust Parameter --------------------------------------------------------
folder <- "chimps"
filenames <- list.files(path = here("exp1", "data", "raw_private", folder))
apes <-  sub("_.*", "", filenames) |> unique()

# Merge Sessions ----------------------------------------------------------
for(i in 1:16){
  nr <- i # nr = 1 means first ape
  
  # Read data
  df <- data.frame()
  for(j in filenames |> str_subset(apes[nr]) |> sort()){
    df_temp <- read.table(here("exp1", "data", "raw_private", folder, j), header = TRUE, sep = "\t")
    df <- df |> 
      bind_rows(df_temp) # Bind/ merge data
    rm(df_temp)
}
  # Write data
  write.table(df, here("exp1", "data", "raw_public", folder, paste0(apes[nr], ".tsv")), 
              row.names = F, quote = F, sep = "\t", dec = ".")
  rm(df)
  print(i)
}

