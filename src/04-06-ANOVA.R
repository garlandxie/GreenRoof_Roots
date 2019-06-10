# libraries --------------------------------------------------------------------
library(car)
library(here)
library(emmeans)
library(tidyr)
library(dplyr)
library(ggplot2)

# import -----------------------------------------------------------------------
df <- readRDS(here::here("data/project_data/final","traits_EF_clean_df.rds"))

# clean ------------------------------------------------------------------------

# list of candidate species
spp_list <- c("VAMA", "EMNI", "DASP", "SITR", 
              "SEAL", "SESE", "PLMA", "SOBI", "SEAC")

# get stormwater data on plants that survived at the end of expt
df1 <- df %>%
  ungroup() %>%
  filter(spp %in% spp_list) %>%
  drop_na() %>%
  select(block, spp, treatment, ind, 
         avg_water_capture, avg_water_loss, 
         total_water_capture, total_water_loss)

# get stormwater data on soil-only treatments
df2 <- df %>%
  ungroup() %>%
  filter(spp == "SC") %>%
  select(block, spp, treatment, ind, 
         avg_water_capture, avg_water_loss, 
         total_water_capture, total_water_loss)

# combine 
clean_df <- rbind(df1, df2)

# ANOVA: avg water loss --------------------------------------------------------

# two-way ANOVA with an unbalanced design: avg_ET_aov
# factors: species identity (eg. SOBI) and watering regime treatments (WW, WD)
# log-transformation on dependent variable to stabilize variance
avg_ET_aov <- clean_df %>%
  mutate(spp = case_when(
    spp  == "SOBI" ~ "Solidago Bicolor",
    spp  == "SYNO" ~ "Symphyotrichum novae-angliae",
    spp  == "PLMA" ~ "Plantago maritima",
    spp  == "SEAC" ~ "Sedum acre",
    spp  == "SEAL" ~ "Sedum album",
    spp  == "SESE" ~ "Sedum sexangular",
    spp  == "VAMA" ~ "Vaccinium macrocarpon",
    spp  == "EMNI" ~ "Empetrum nigrum",
    spp  == "DASP" ~ "Danthonia spicata",
    spp  == "SITR" ~ "Sibbaldiopsis tridentata",
    spp  == "SC"   ~ "Soil-Only",
    TRUE ~ spp),
    spp = fct_reorder(spp, avg_water_loss))  %>%
  lm(log(avg_water_capture) ~ spp + treatment, data = .)

# dianostic plots: 
# (1) residuals vs fitted
# (2) normal Q-Q
# (3) scale-location
# (4) constant leverage: residuals vs factor levels
plot(avg_ET_aov)

# reference grid: ref_grid_avg_ET
ref_grid(avg_ET_aov)@grid

# type II sum of squares to account for unbalanced design: avg_ET_aov
Anova(avg_ET_aov, type = "II")

# estimated marginal means (least square means): avg_ET_lsm
avg_ET_lsm <- lsmeans(avg_ET_aov, specs = "spp", by = "treatment")

# obtain the reference grid
ref_grid(avg_ET_aov)@grid

# multiple pairwise comparisons using Tukey HSD test
# shows letters for plotting purposes 
plot(avg_ET_lsm, comparison = TRUE, 
     type = "response", alpha = 0.05, adjust = "tukey") +
  labs(y = NULL, x = "Average ET")

# ANOVA: avg water capture -----------------------------------------------------

# two-way ANOVA with an unbalanced design: avg_ET_aov
# factors: species identity (eg. SOBI) and watering regime treatments (WW, WD)
# log-transformation on dependent variable to stabilize variance
avg_ret_aov <- clean_df %>%
  mutate(spp = case_when(
           spp  == "SOBI" ~ "Solidago Bicolor",
           spp  == "SYNO" ~ "Symphyotrichum novae-angliae",
           spp  == "PLMA" ~ "Plantago maritima",
           spp  == "SEAC" ~ "Sedum acre",
           spp  == "SEAL" ~ "Sedum album",
           spp  == "SESE" ~ "Sedum sexangular",
           spp  == "VAMA" ~ "Vaccinium macrocarpon",
           spp  == "EMNI" ~ "Empetrum nigrum",
           spp  == "DASP" ~ "Danthonia spicata",
           spp  == "SITR" ~ "Sibbaldiopsis tridentata",
           spp  == "SC"   ~ "Soil-Only",
           TRUE ~ spp),
         spp = fct_reorder(spp, avg_water_capture))  %>%
  lm(log(avg_water_capture) ~ spp + treatment, data = .)

# dianostic plots: 
# (1) residuals vs fitted
# (2) normal Q-Q
# (3) scale-location
# (4) constant leverage: residuals vs factor levels
plot(avg_ret_aov)

# reference grid: ref_grid_avg_ET
ref_grid(avg_ret_aov)@grid

# type II sum of squares to account for unbalanced design: avg_ET_aov
Anova(avg_ret_aov, type = "II")

# estimated marginal means (least square means): avg_ET_lsm
avg_ret_lsm <- lsmeans(avg_ret_aov, specs = "spp", by = "treatment")

# pairwise compariso plots
# avoids the common use of a statistical significance threshold
# authors also want users to avoid CLD
plot(avg_ret_lsm, comparison = TRUE, 
     type = "response", alpha = 0.05, adjust = "tukey") +
  labs(y = NULL, x = "Average Stormwater Capture")


# ANOVA: total water capture ---------------------------------------------------

# two-way ANOVA with an unbalanced design: avg_ET_aov
# factors: species identity (eg. SOBI) and watering regime treatments (WW, WD)
# log-transformation on dependent variable to stabilize variance
total_ret_aov <- clean_df %>%
  mutate(spp = case_when(
    spp  == "SOBI" ~ "Solidago Bicolor",
    spp  == "SYNO" ~ "Symphyotrichum novae-angliae",
    spp  == "PLMA" ~ "Plantago maritima",
    spp  == "SEAC" ~ "Sedum acre",
    spp  == "SEAL" ~ "Sedum album",
    spp  == "SESE" ~ "Sedum sexangular",
    spp  == "VAMA" ~ "Vaccinium macrocarpon",
    spp  == "EMNI" ~ "Empetrum nigrum",
    spp  == "DASP" ~ "Danthonia spicata",
    spp  == "SITR" ~ "Sibbaldiopsis tridentata",
    spp  == "SC"   ~ "Soil-Only",
    TRUE ~ spp),
    spp = fct_reorder(spp, total_water_capture))  %>%
  lm(log(total_water_capture) ~ spp + treatment, data = .)

# dianostic plots: 
# (1) residuals vs fitted
# (2) normal Q-Q
# (3) scale-location
# (4) constant leverage: residuals vs factor levels
plot(total_ret_aov)

# reference grid: ref_grid_avg_ET
ref_grid(total_ret_aov)@grid

# type II sum of squares to account for unbalanced design: avg_ET_aov
Anova(total_ret_aov, type = "II")

# estimated marginal means (least square means): avg_ET_lsm
total_ret_lsm <- lsmeans(total_ret_aov, specs = "spp", by = "treatment")

# multiple pairwise comparisons using Tukey HSD test
# shows letters for plotting purposes 
plot(total_ret_lsm, comparison = TRUE, 
     type = "response", alpha = 0.05, adjust = "tukey") +
  labs(y = NULL, x = "Average Stormwater Capture")


# ANOVA: total water loss -------------------------------------------------------

# two-way ANOVA with an unbalanced design: total_ET_aov
# factors: species identity (eg. SOBI) and watering regime treatments (WW, WD)
# log-transformation on dependent variable to stabilize variance

total_ET_aov <- clean_df %>%
  mutate(spp = case_when(
    spp  == "SOBI" ~ "Solidago Bicolor",
    spp  == "SYNO" ~ "Symphyotrichum novae-angliae",
    spp  == "PLMA" ~ "Plantago maritima",
    spp  == "SEAC" ~ "Sedum acre",
    spp  == "SEAL" ~ "Sedum album",
    spp  == "SESE" ~ "Sedum sexangular",
    spp  == "VAMA" ~ "Vaccinium macrocarpon",
    spp  == "EMNI" ~ "Empetrum nigrum",
    spp  == "DASP" ~ "Danthonia spicata",
    spp  == "SITR" ~ "Sibbaldiopsis tridentata",
    spp  == "SC"   ~ "Soil-Only",
    TRUE ~ spp),
    spp = fct_reorder(spp, total_water_loss))  %>%
  lm(log(total_water_loss) ~ spp + treatment, data = .)

# dianostic plots: 
# (1) residuals vs fitted
# (2) normal Q-Q
# (3) scale-location
# (4) constant leverage: residuals vs factor levels
plot(total_ET_aov)

# reference grid: ref_grid_avg_ET
ref_grid(total_ET_aov)@grid

# type II sum of squares to account for unbalanced design: avg_ET_aov
Anova(total_ET_aov, type = "II")

# estimated marginal means (least square means): avg_ET_lsm
total_ET_lsm <- lsmeans(total_ET_aov, specs = "spp", by = "treatment")

# multiple pairwise comparisons using Tukey HSD test
# shows letters for plotting purposes 
plot(total_ET_lsm, 
     comparison = TRUE, 
     type = "response", 
     alpha = 0.05, 
     adjust = "tukey") +
  
  labs(y = NULL,
       x = "Average Stormwater Capture")

