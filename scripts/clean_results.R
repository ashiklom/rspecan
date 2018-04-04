library(tidyverse)

raw_results <- read_csv("spectra_db/all_results.csv") %>%
  select(-specdb)

results <- raw_results %>%
  filter(
    !(parameter == "N" & Mean > 12),
    !(parameter == "Cab" & Mean > 200),
    !(parameter == "Car" & Mean > 100),
    !(parameter == "Cw" & Mean > 0.2),
    !(parameter == "Cm" & Mean > 0.2)
  )

bad_results <- anti_join(raw_results, results)

write_csv(results, "spectra_db/cleaned_results.csv")
write_csv(bad_results, "spectra_db/bad_results.csv")
