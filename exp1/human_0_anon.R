# This script anonymizes the data.
# It required data in the folder "raw" (not shared in the Github respository due to data projection reasons)

library(tidyverse)
library(here)

anonymize_file <- function(file, id, rec_col  = "Recording.name", part_col = "Participant.name",
                             drop_cols = c("Recording.date", "Recording.Time")) {
  rec_sym  <- sym(rec_col)
  part_sym <- sym(part_col)
  
  df <- read.table(file, header = T, sep = "\t")
    mutate(!!rec_sym  := id, !!part_sym := id) |>
    select(-any_of(drop_cols))
  
  message("Done: ", basename(file))
  df
}

for(i in list.files(here("exp1", "data", "raw"))){
  
  files <- list.files(here("exp1", "data", "raw", i))
  
  for(j in files){
    dat_temp <- anonymize_file(file = here("exp1", "data", "raw", i, j), id = which(files == j), 
                               rec_col  = "Recording.name", part_col = "Participant.name",
                               drop_cols = c("Recording.date", "Recording.Time"))

  }
  
  
}

