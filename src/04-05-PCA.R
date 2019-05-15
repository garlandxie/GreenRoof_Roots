# libraries --------------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)

# import -----------------------------------------------------------------------
traits_EF_clean_df <- readRDS(here::here("data/project_data/final",
                                         "traits_EF_clean_df.rds"))

# pca --------------------------------------------------------------------------

# WD
pca_matrix_wd <- traits_EF_clean_df %>%
  ungroup() %>%
  filter(treatment == "WD") %>%
  select(rmf, srl, rld, max_root_depth_cm, mean_radius_mm, plant_size) %>%
  drop_na %>% # complete cases
  prcomp(center = TRUE, scale = TRUE)

summary(pca_matrix_wd) # info on cumulative proportion

# WW
pca_matrix_ww <- traits_EF_clean_df %>%
  ungroup() %>%
  filter(treatment == "WW") %>%
  select(rmf, srl, rld, max_root_depth_cm, mean_radius_mm, plant_size) %>%
  drop_na %>% # complete cases
  prcomp(center = TRUE, scale = TRUE)

summary(pca_matrix_ww)  # info on cumulative proportion 

# save the data ----------------------------------------------------------------
saveRDS(pca_matrix_wd, here("data/project_data/final", "pca_matrix_WD.rds"))
saveRDS(pca_matrix_ww, here("data/project_data/final", "pca_matrix_WW.rds"))

