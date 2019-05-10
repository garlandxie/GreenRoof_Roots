# libraries ------
library(lmerTest)
library(lme4)
library(here)
library(dplyr)
library(piecewiseSEM)
library(ggplot2)
library(visreg)
library(tidyr)

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
lmm_total_ret_WW <- lmer(
  formula = total_water_capture ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block), # random vars
  REML = TRUE, # restricted maximum-likelihood (unbiased estimator)
  data = traits_EF_WW)

# WD
lmm_total_ret_WD <- lmer(
  formula =  total_water_capture ~ # response var
    scale(srl) + scale(mean_radius_mm) + scale(rld) + 
    scale(rmf) + scale(max_root_depth_cm) + # fixed vars
    scale(plant_size) + # covariate var 
    (1|block), # random vars
  REML = TRUE, # restricted maximum-likelihood (unbiased estimator)
  data = traits_EF_WD)

# model summary ----------------------------------------------------------------

# nominal p-values are calculated using Scatterweithe's approximation for df
summary(lmm_total_ret_WW)
summary(lmm_total_ret_WD)

# rsquared ---------------------------------------------------------------------

# Nakagawa and Schielzeth's R-squared values for LMM's
# Maginal R-2: accounts for only fixed effects
# Conditional R-2: accounts for both fixed and random effects
rsquared(lmm_total_ret_WD)
rsquared(lmm_total_ret_WW)


# variance inflation factor ----------------------------------------------------

# VIF = 1 ==> no multicollinearity
# general cutoff: VIF < 2
# custom code should accounts for mixed effect models 

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
vif.mer(lmm_total_ret_WD)
vif.mer(lmm_total_ret_WW)

# model adequacy ---------------------------------------------------------------

# diagnostic plots: residuals vs fitted
# checking assumption of homogeneity of variance
plot(lmm_total_ret_WD)
plot(lmm_total_ret_WW)

# diagnostic plots: histogram of residuals
# checking assumption of normally disributed residuals
hist(residuals(lmm_total_ret_WD))
hist(residuals(lmm_total_ret_WW))

# diagnostic plots: leverage
# checking assumptions regarding sensitivity to data (esp outliers)
lev_pearson_WD <- data.frame(lev = hatvalues(lmm_total_ret_WD), 
                             pearson = residuals(lmm_total_ret_WD, 
                                                 type = "pearson"))

ggplot(lev_pearson_WD, aes(x = lev, y = pearson)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE, col = "red") + 
  theme_bw()

lev_pearson_WW <- data.frame(lev = hatvalues(lmm_total_ret_WW), 
                             pearson = residuals(lmm_total_ret_WW, 
                                                 type = "pearson"))

ggplot(lev_pearson_WW, aes(x = lev, y = pearson)) + 
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE, col = "red") + 
  theme_bw()

# intra-class correlation
# how much variation in the data is attributed to the random effect

r1Var_WD <- as.numeric(VarCorr(lmm_total_ret_WD)[["g1"]])
residVar_WD <- attr(VarCorr(lmm_total_ret_WD), "sc")^2
r1Var_WD
residVar_WD
r1Var_WD / (r1Var_WD + residVar_WD)

# confidence intervals ---------------------------------------------------------

# calcuate profile 95% CI 
confint.merMod(lmm_total_ret_WD, method = "profile")
confint.merMod(lmm_total_ret_WW, method = "profile")
