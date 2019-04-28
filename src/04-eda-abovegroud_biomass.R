# libraries ----
library(here)
library(tidyverse)

# import ----
abov_bm_df <- readRDS(here("data/project_data/working",
                           "above_biomass_clean.rds"))

# check packaging ----
glimpse(abov_bm_df)
head(abov_bm_df, n = 5)
tail(abov_bm_df, n = 5)

# check your n's ----

# how many unique block ID's ?
abov_bm_df %>%
  pull(block) %>%
  n_distinct()

# how many unique species (incl. soil)?
abov_bm_df %>%
  pull(species) %>%
  n_distinct()

# ???

d <- abov_bm_df %>%
  group_by(block, species, treatment) %>%
  summarize(n = n())

# how many modules per treatment per block?

abov_bm_df %>%
  group_by(block, treatment) %>%
  summarize(n = n()) 

# print out species code

abov_bm_df %>%
  pull(species) %>%
  unique %>%
  print

# plot ----

abov_bm_df %>%
  filter(species != "SC") %>%
  ggplot(aes(x = species, y = above_dry_g)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~treatment) +
  labs(x = "Species Code",
       y = "Aboveground Dry Biomass (g)")

abov_bm_df %>%
  filter(species != "SC") %>%
  ggplot(aes(x = species, y = above_dry_g)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~block, ncol = 5) +
  labs(x = "Species Code",
       y = "Aboveground Dry Biomass (g)") 

abov_bm_df %>%
  filter(species != "SC") %>%
  ggplot(aes(x = species, y = above_dry_g)) +
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~treatment) +
  labs(x = "Species Code",
       y = "Aboveground Dry Biomass (g)") 

# analysis of variance ----

# NOTE: this is NOT confirmatory data analysis

# p-value < 0.001: statistically clear that there is interspecific variation
anova(lm(formula = above_dry_g ~ species, data= abov_bm_df))

# high p-value: unclear that exp treatment influence aboveground biomass
anova(lm(formula = above_dry_g ~ treatment, data= abov_bm_df))


