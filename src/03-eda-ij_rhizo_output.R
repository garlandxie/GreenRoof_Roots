# library ----------------------------------------------------------------------
library(here)
library(tidyverse)
library(visdat)

# import -----------------------------------------------------------------------
ij_rhizo_df <- readRDS(here("data/project_data/working",
                            "ij_rhizo_output_clean.rds"))

# check packaging --------------------------------------------------------------
glimpse(ij_rhizo_df)
head(ij_rhizo_df, n = 5)
tail(ij_rhizo_df, n = 5)

# visualise missing data -------------------------------------------------------
vis_dat(ij_rhizo_df)
vis_miss(ij_rhizo_df)

# plot -------------------------------------------------------------------------

# visualise interspecific variation in mean root diameter (mm)
ij_rhizo_df %>%
  ggplot(aes(x = species, y = mean_radius_mm)) + 
  geom_boxplot() + 
  coord_flip() 

# visualise interspecific variation in root length density
ij_rhizo_df %>%
  ggplot(aes(x = species, y = rld)) + 
  geom_boxplot() + 
  coord_flip()

# quick glance to check collinearity: radius as x  vs rld as y
ij_rhizo_df %>%
  ggplot(aes(x = mean_radius_mm, y = rld)) + 
  geom_point()

# quick glance to check collinearity: radius as y vs rld as x
ij_rhizo_df %>%
  ggplot(aes(y = rld, x = mean_radius_mm)) + 
  geom_point()
