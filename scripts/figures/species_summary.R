library(tidyverse)
library(rspecan)
library(metar)

dat <- get_metadata(indir("spectra_db"))

spp <- dat %>%
  count(short_name, species_code, sort = TRUE) %>%
  mutate(species_code = fct_reorder(species_code, n, sum, .desc = TRUE))

project_colors <- read_csv(infile("spectra_db/project_colors.csv")) %>%
  df2dict("color", "short_name")

ggplot(spp) +
  aes(x = species_code, y = n, fill = short_name) +
  geom_col() +
  scale_fill_manual(values = project_colors) +
  xlab("Species") +
  ylab("Number of samples") +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

ggsave(infile("manuscript", "figures", "species_counts.pdf"), width = 8, height = 6)
