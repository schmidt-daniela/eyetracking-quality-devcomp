# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------
library(here)
library(tidyverse)

# Adjust Parameter --------------------------------------------------------
folder <- "chimps_9p"
filenames <- list.files(path = here("data", "raw_all", folder))
apes <-  sub("_.*", "", filenames) |> unique()

for(i in 1:16){
nr <- i # nr 1 is first ape

# Read Data ---------------------------------------------------------------
df <- data.frame()
for(j in filenames |> str_subset(apes[nr]) |> sort()){
  df_temp <- read.table(here("data", "raw_all", folder, j), header = TRUE, sep = "\t")
  df <- df |> 
    bind_rows(df_temp)
  rm(df_temp)
}

# Write Data --------------------------------------------------------------
write.table(df, here("data", "raw_included_1", folder, paste0(apes[nr], ".tsv")), row.names = F, quote = F, sep = "\t", dec = ".")

rm(df)
print(i)
}
