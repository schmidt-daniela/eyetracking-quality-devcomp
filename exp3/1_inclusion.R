# Clear Workspace ---------------------------------------------------------
rm(list = ls())

# Adjust Parameter --------------------------------------------------------
sheet_name <- "4mo" # "4mo" or "6to18mo"
age_group <- "4mo" # "4mo" or "6to18mo"

# Packages ----------------------------------------------------------------
library(here)
#library(openxlsx)
library(readxl)
library(tidyverse)

# Read Data ---------------------------------------------------------------

# Read and process protocol
protocol <- read_excel(here("exp3", "doc", "facet_infant_protocol.xlsx"), sheet = sheet_name) |> 
  slice(-1)

included_infants <- protocol |> 
  drop_na(running_number_include_final) |> 
  slice(1:32) |> 
  mutate(age_group = age_group) |> 
  unite("group_id", c(age_group, running_number)) |> 
  mutate(group_id = paste0(group_id, ".tsv")) |> 
  pull(group_id)

# Save included files -----------------------------------------------------
# Read files from raw-folder and save files in included-folder based on protocol information, which was extracted above.
for (filename in included_infants) {
  filepath <- file.path(here("exp3", "data", "raw_2", age_group), filename)
  vp_data <- read.table(filepath, header = TRUE, sep = "\t")
  
  eye_color <- protocol |> 
    mutate(age_group = age_group) |> 
    unite("group_id", c(age_group, running_number)) |> 
    select(group_id, eye_color) |> 
    filter(group_id == filename |> str_replace(".tsv","")) |> 
    pull(eye_color)
  vp_data <- vp_data |> mutate(eye_color = eye_color)

  write.table(vp_data, here("exp3", "data", "raw_included", age_group, filename), row.names = F, quote = F, sep = "\t", dec = ".")
  
  print(paste(filename, "done.", sep = " "))
}

