# libraries -----
library(here)
library(tidyverse)
library(visdat)

# import ----
mrd_df <- readRDS(here("data/project_data/working",
                       "max_root_depth_clean.rds"))

# check packaging ----
glimpse(mrd_df)
head(mrd_df, n = 5)
tail(mrd_df, n = 5)

# visualize missing data ----
vis_dat(mrd_df)
vis_miss(mrd_df)

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

mrd_df %>%
  group_by(block, species, treatment) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  keep(function(x) x > 2)

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

# analysis of variance: spp vs mrd ---------------------------------------------

# NOTE: this is NOT confirmatory data analysis

# p-value < 0.001: statistically clear there is interspecific variation
# hard to say if this would impact the global regression model though
lm_sp_mrd <- lm(formula = max_root_depth_cm ~ species, data = mrd_df)
anova(lm_sp_mrd)

# diagnostic plots
plot(lm_sp_mrd)

# analysis of variance: wd/ww vs mrd -------------------------------------------

# NOTE: this is NOT confirmatory data analysis

# high p-value: unclear that exp treatments influenced max root depth
# makes sense - high spatial boundary in "extensive green roof modules"
lm_wt_mrd <- lm(formula = max_root_depth_cm ~ treatment, data = mrd_df)
anova(lm_wt_mrd)

# diagnostic plots
plot(lm_wt_mrd) 


