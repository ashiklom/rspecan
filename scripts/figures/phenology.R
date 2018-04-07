library(tidyverse)
library(rspecan)
library(metar)
import::from(lubridate, ymd, year, yday)

md <- read_csvy("spectra_db/cleaned_metadata.csvy")
results <- read_csv("spectra_db/cleaned_results.csv")

mdsub <- md %>%
  filter(
    project_code == "yang_pheno",
    species_code == "QURU"
  ) %>%
  mutate_if(is.character, na_if, "") %>%
  select_if(~!all(is.na(.))) %>%
  left_join(results) %>%
  mutate(
    collection_date = ymd(collectiondate),
    year = year(collection_date),
    doy = yday(collection_date),
    parameter = factor(parameter, variable_df$code)
  ) %>%
  filter(
    year == 2011,
    prospect_version == "D",
    !is.na(parameter),
    !(parameter == "Canth" & Mean > 10)
  )

mdplot <- mdsub %>%
  mutate(parameter = `levels<-`(parameter, variable_df$short_units))

plt <- ggplot(mdplot) +
  aes(x = collection_date, y = Mean, color = sun_shade) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~parameter, scales = "free_y", labeller = label_parsed) +
  theme_bw() +
  ylab("Inversion mean estimate") +
  theme(axis.title.x = element_blank())

ggsave(infile("manuscript", "figures", "trait_phenology.pdf"), plt)
