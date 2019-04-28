# libraries -----
library(here)
library(tidyverse)

# import ----
mrd_df <- readRDS(here("data/project_data/working",
                       "max_root_depth_clean.rds"))

# check packaging ----
glimpse(mrd_df)
head(mrd_df, n = 5)
tail(mrd_df, n = 5)

# check your n's ----

# how many unique block ID's ?
mrd_df %>%
  pull(block) %>%
  n_distinct()

# how many unique species (incl. soil)?
mrd_df %>%
  pull(species) %>%
  n_distinct()

# ???

d <- mrd_df %>%
  group_by(block, species, treatment) %>%
  summarize(n = n())

# how many modules per treatment per block?

mrd_df %>%
  group_by(block, treatment) %>%
  summarize(n = n()) 

# print out species code

mrd_df %>%
  pull(species) %>%
  unique %>%
  print

# plot ----

# trait variation: max root depth by water regime treatment
mrd_df %>%
  filter(species != "SC") %>%
  ggplot(aes(x = species, y = max_root_depth_cm)) +
  geom_jitter() +
  coord_flip() +
  facet_wrap(~treatment) +
  labs(x = NULL,
       y = NULL)

# trait variation: max root depth by block treatment
mrd_df %>%
  ggplot(aes(x = species, y = max_root_depth_cm)) +
  geom_jitter() +
  coord_flip() +
  facet_wrap(~block, ncol = 5) +
  labs(x = "Species Code",
       y = "Maximum Rooting Depth (cm)")

# trait varation: influence of water regime treatment on max root depth
mrd_df %>%
  ggplot(aes(x = treatment, y = max_root_depth_cm)) + 
  geom_jitter() +
  scale_y_continuous(breaks = seq(from = 0,to = 13, by = 1)) +
  labs(x = "water regime treatment",
       y = "maximum rooting depth (cm)") +
  theme_minimal()

# analysis of variance -------

# NOTE: this is NOT confirmatory data analysis
# I'm not testing any a priori hypotheses here
# Just curious if there any differences here and there

# spp vs mrd
# p-value < 0.001: statistically clear there is interspecific variation
# hard to say if this would impact the global regression model though
aov_sp_mrd <- anova(lm(formula = max_root_depth_cm ~ species,
                       data = mrd_df))

# wd/ww vs mrd
# high p-value: unclear that exp treatments influenced max root depth
# makes sense - high spatial boundary in "extensive green roof modules"
aov_wt_mrd <- anova(lm(formula = max_root_depth_cm ~ treatment,
                       data = mrd_df))


