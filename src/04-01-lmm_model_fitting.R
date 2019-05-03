# libraries ------
library(lmerTest)
library(lme4)
library(here)
library(dplyr)
library(purrr)

# import -----------------------------------------------------------------------
traits_EF_clean_df <- readRDS(here("data/project_data/final",
                                   "traits_EF_clean_df.rds"))

# check packaging --------------------------------------------------------------
tibble:glimpse(traits_EF_clean_df)
head(traits_EF_clean_df, n = 5)
tail(traits_EF_clean_df, n = 5)

# split dataset ----------------------------------------------------------------
traits_EF_WW <- traits_EF_clean_df %>% 
  filter(treatment == "WW") 

traits_EF_WD <- traits_EF_clean_df %>% 
  filter(treatment == "WD")

# model fitting: avg water capture ---------------------------------------------

# WW 
lmm_avg_ret_WW <- lmer(
  formula = avg_water_capture ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block) + (1|spp), # random vars
  REML = TRUE,
  data = traits_EF_WW)

summary(lmm_avg_ret_WW)

# WD
lmm_avg_ret_WD <- lmer(
  formula = avg_water_capture ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block) + (1|spp), # random vars
  REML = TRUE,
  data = traits_EF_WD)


# model fitting: avg water loss ------------------------------------------------

# WW 
lmm_avg_ET_WW <- lmer(
  formula = avg_water_loss ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block) + (1|spp), # random vars
  REML = TRUE, 
  data = traits_EF_WW)

# WD 
lmm_avg_ET_WD <- lmer(
  formula = avg_water_loss ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block) + (1|spp), # random vars
  REML = TRUE,
  data = traits_EF_WD)

# model fitting: total water capture -------------------------------------------

# WW 
lmm_total_ret_WW <- lmer(
  formula = total_water_capture ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block) + (1|spp), # random vars
  REML = TRUE, 
  data = traits_EF_WW)

# WD
lmm_total_ret_WD <- lmer(
  formula = total_water_capture ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block) + (1|spp), # random vars
  REML = TRUE, 
  data = traits_EF_WD)

# model fitting: total water loss ----------------------------------------------

# WW 
lmm_total_ET_WW <- lmer(
  formula = total_water_loss ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block) + (1|spp), # random vars
  REML = TRUE, 
  data = traits_EF_WW)

# WD 
lmm_total_ET_WD <- lmer(
  formula = total_water_loss ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block) + (1|spp), # random vars
  REML = TRUE, 
  data = traits_EF_WD)
