#!/usr/bin/env Rscript
library(tidyverse)
library(metar)
library(progress)

if (!file.exists("result_files")) {
  message("Searching for files...")
  result_files <- list.files("specdb", "results.csvy", recursive = TRUE, full.names = TRUE)
  message("Done!")
} else {
  result_files <- readLines("result_files")
}

result_df1 <- str_split(result_files, "/") %>%
  invoke(rbind, .) %>%
  `colnames<-`(c("dot", "specdb", "project_code", "drop_inv", "observation_id",
                 "prospect_version_string", "drop_resultfname")) %>%
  as_tibble() %>%
  mutate(
    prospect_version = str_remove(prospect_version_string, "prospect_")
  ) %>%
  select(-prospect_version_string, -drop_inv, -drop_resultfname, -dot)

read_result <- function(file, pb) {
  pb$tick()
  read_csvy(file)
}

message("Reading results")
pb <- progress_bar$new(total = length(result_files))
result_list <- map(result_files, read_result, pb = pb)

result_df1$result_list <- result_list
result_df2 <- unnest(result_df1, result_list)

write_csv(result_df2, "spectra_db/all_results.csv")
