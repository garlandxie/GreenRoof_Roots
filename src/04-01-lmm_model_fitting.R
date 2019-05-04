# libraries ------
library(lmerTest)
library(lme4)
library(here)
library(dplyr)
library(piecewiseSEM)

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
    (1|block), # random vars
  REML = TRUE,
  data = traits_EF_WW)

# WD
lmm_avg_ret_WD <- lmer(
  formula = avg_water_capture ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block), # random vars
  REML = TRUE,
  data = traits_EF_WD)


# model fitting: avg water loss ------------------------------------------------

# WW 
lmm_avg_ET_WW <- lmer(
  formula = avg_water_loss ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block), # random vars
  REML = TRUE, 
  data = traits_EF_WW)

# WD 
# NOTE: previous attempts had problems with convergence, simplified models
lmm_avg_ET_WD <- lmer(
  formula = avg_water_loss ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block), # random vars 
  REML = TRUE,
  data = traits_EF_WD)

# model fitting: total water capture -------------------------------------------

# WW 
lmm_total_ret_WW <- lmer(
  formula = total_water_capture ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block), # random vars
  REML = TRUE, 
  data = traits_EF_WW)

# WD
lmm_total_ret_WD <- lmer(
  formula = total_water_capture ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block), # random vars
  REML = TRUE, 
  data = traits_EF_WD)

# model fitting: total water loss ----------------------------------------------

# WW 
lmm_total_ET_WW <- lmer(
  formula = total_water_loss ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block), # random vars
  REML = TRUE, 
  data = traits_EF_WW)

# WD 
# NOTE: previous attempts had problems with convergence, simplified models
# so removed block as a random var from previous conditional boxplots
lmm_total_ET_WD <- lmer(
  formula = total_water_loss ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block), # random vars
  REML = TRUE, 
  data = traits_EF_WD)

# rsquared ---------------------------------------------------------------------

# avg stormwater retention
rsquared(lmm_avg_ret_WD)
rsquared(lmm_avg_ret_WW)

# avg ET 
rsquared(lmm_avg_ET_WW)
rsquared(lmm_avg_ET_WD)

# total stormwater retention
rsquared(lmm_total_ret_WW)
rsquared(lmm_total_ret_WD)

# model summaries --------------------------------------------------------------

# avg stormwater retention
summary(lmm_avg_ret_WW)
summary(lmm_avg_ret_WD)

# avg ET 
summary(lmm_avg_ET_WW)
summary(lmm_avg_ET_WD)

# total stormwater retention
summary(lmm_total_ret_WW)
summary(lmm_total_ret_WD)

# total ET 
summary(lmm_total_ET_WW)
summary(lmm_total_ET_WD)

# multicollinearity: vifs for lmm's --------------------------------------------

# VIF = 1 ==> no multicollinearity
# general cutoff: VIF < 2

vif.mer <- function (fit) {
  ## adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

# avg ret 
vif.mer(lmm_avg_ret_WD)
vif.mer(lmm_avg_ret_WW)

# avg ET 
vif.mer(lmm_avg_ET_WD)
vif.mer(lmm_avg_ET_WW)

# total ret 
vif.mer(lmm_total_ret_WD)
vif.mer(lmm_total_ret_WW)

# total ET
vif.mer(lmm_total_ET_WD)
vif.mer(lmm_total_ET_WW)

# model adequacy ---------------------------------------------------------------

# diagnostic plots: residuals vs fitted

plot(lmm_total_ET_WD)
plot(lmm_total_ET_WW)
plot(lmm_total_ret_WD)
plot(lmm_total_ret_WW)

plot(lmm_avg_ET_WW)
plot(lmm_avg_ET_WD)
plot(lmm_avg_ret_WW)
plot(lmm_avg_ret_WD)

# diagnostic plots: residual distribution

qqnorm(residuals(lmm_total_ET_WD))
qqnorm(residuals(lmm_total_ET_WW))
qqnorm(residuals(lmm_total_ret_WD))
qqnorm(residuals(lmm_total_ret_WW))

qqnorm(residuals(lmm_avg_ET_WW))
qqnorm(residuals(lmm_avg_ET_WD))
qqnorm(residuals(lmm_avg_ret_WD))
qqnorm(residuals(lmm_avg_ret_WW))

# confidence intervals ---------------------------------------------------------

# calculate 95% CI for linear mixed effect models
confint_lmm_profile <- purrr::partial(confint.merMod, method = "profile")

# avg ET
confint_lmm_profile(lmm_avg_ET_WD)
confint_lmm_profile(lmm_avg_ET_WW)

# avg retention
confint_lmm_profile(lmm_avg_ret_WD)
confint_lmm_profile(lmm_avg_ret_WW)

# total retention
confint_lmm_profile(lmm_total_ret_WD)
confint_lmm_profile(lmm_total_ret_WW)

# total ET
confint_lmm_profile(lmm_total_ET_WD)
confint_lmm_profile(lmm_total_ET_WW)

