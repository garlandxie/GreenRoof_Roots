# libraries --------------------------------------------------------------------
library(here)
library(plyr)
library(dplyr)
library(visdat)
library(tidyr)
library(ggbiplot)

# import ---------------------------------------------
traits_EF_clean_df <- readRDS(here::here("data/project_data/final",
                                   "traits_EF_clean_df.rds"))

# check packaging --------------------------------------------------------------
tibble:glimpse(traits_EF_clean_df)
head(traits_EF_clean_df, n = 5)
tail(traits_EF_clean_df, n = 5)

# visualize missing data -------------------------------------------------------
vis_miss(traits_EF_clean_df)
vis_dat(traits_EF_clean_df)

# check your n's ---------------------------------------------------------------
traits_EF_clean_df %>%
  group_by(treatment, block) %>%
  summarize(n = n()) %>%
  ungroup() 

traits_EF_clean_df %>%
  group_by(treatment, block, spp) %>%
  summarize(n = n()) %>%
  ungroup() 

# correlation matrices: WW -----------------------------------------------------

# create trait matrix for WW treatment
corr_matrix_ww <- traits_EF_clean_df %>%
  ungroup() %>%
  filter(treatment == "WW") %>%
  select(rmf, srl, rld, max_root_depth_cm, mean_radius_mm) 

# pairwise scatterplot matrix for WW 
pairs(corr_matrix_ww)

# pairwise correlation matrix for WW: use Kendall's tau correlation 
cor(corr_matrix_ww, use = "complete.obs", method = "kendall")

# correlation matirx: WD -------------------------------------------------------

# create trait matrix for WD
corr_matrix_wd <- traits_EF_clean_df %>%
  ungroup() %>%
  filter(treatment == "WD") %>%
  select(rmf, srl, rld, max_root_depth_cm, mean_radius_mm) 

# pairwise scatterplot matrix for WD
pairs(corr_matrix_wd)

# pairwise correlation matrix for WD: use Kendall's tau correlation
cor(corr_matrix_wd, use = "complete.obs", method = "kendall")

# pca --------------------------------------------------------------------------

# WD
pca_matrix_wd <- traits_EF_clean_df %>%
  ungroup() %>%
  filter(treatment == "WD") %>%
  select(rmf, srl, rld, max_root_depth_cm, mean_radius_mm, plant_size) %>%
  drop_na %>% # complete cases
  prcomp(center = TRUE, scale = TRUE)

summary(pca_matrix_wd) # info on cumulative proportion
ggbiplot(pca_matrix_wd) # plot loadings

# WW
pca_matrix_ww <- traits_EF_clean_df %>%
  ungroup() %>%
  filter(treatment == "WW") %>%
  select(rmf, srl, rld, max_root_depth_cm, mean_radius_mm, plant_size) %>%
  drop_na %>% # complete cases
  prcomp(center = TRUE, scale = TRUE)

summary(pca_matrix_ww)  # info on cumulative proportion 
ggbiplot(pca_matrix_ww) # plot loadings
