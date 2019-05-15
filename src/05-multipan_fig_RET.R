# libraries --------------------------------------------------------------------
library(broom)
library(ggstatsplot)
library(here)
library(ggplot2)
library(dplyr)

# import -----------------------------------------------------------------------
lmm_avg_water_ret_WD <- readRDS(here("data/project_data/final",
                                     "lmm_avg_ret_WD.rds"))

lmm_avg_water_ret_WW <- readRDS(here("data/project_data/final",
                                     "lmm_avg_ret_WW.rds"))

lmm_total_ret_WD <- readRDS(here("data/project_data/final",
                                 "lmm_total_ret_WD.rds"))

lmm_total_ret_WW <- readRDS(here("data/project_data/final",
                                 "lmm_total_ret_WW.rds"))


# plot: avg ret - WD -----------------------------------------------------------
plot_avg_ret_WD <- lmm_avg_water_ret_WD %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__Observation",
         term != "(Intercept)",
         term != "sd__(Intercept)") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root Length Density",
    term == "scale(srl)" ~ "Specific Root Length",
    term == "scale(max_root_depth_cm)" ~ "Max Root Depth",
    term == "scale(plant_size)" ~ "Total Biomass",
    term == "scale(mean_radius_mm)" ~ "Mean Root Diameter",
    TRUE ~ term)) %>%
  
  # reorder rows manually
  slice(match(c("Root Length Density",
                "Mean Root Diameter", 
                "Total Biomass", 
                "Max Root Depth", 
                "Specific Root Length"),
              term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              point.color = "red",
              stats.labels = FALSE,
              conf.int = TRUE) + 
  
  labs(x = NULL, y = NULL)

# plot: avg ret - WW -----------------------------------------------------------

plot_avg_ret_WW <- lmm_avg_water_ret_WW %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__Observation",
         term != "(Intercept)",
         term != "sd__(Intercept)") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root Length Density",
    term == "scale(srl)" ~ "Specific Root Length",
    term == "scale(max_root_depth_cm)" ~ "Max Root Depth",
    term == "scale(plant_size)" ~ "Total Biomass",
    term == "scale(mean_radius_mm)" ~ "Mean Root Diameter",
    TRUE ~ term)) %>%
  
  # reorder rows manually
  slice(match(c("Root Length Density",
                "Mean Root Diameter", 
                "Total Biomass", 
                "Max Root Depth", 
                "Specific Root Length"),
              term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              point.color = "blue",
              stats.labels = FALSE,
              conf.int = TRUE) + 
  
  labs(x = NULL, y = NULL)
      
# plot: total ret - WD ---------------------------------------------------------
plot_total_ret_WD <- lmm_total_ret_WD %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__Observation",
         term != "(Intercept)",
         term != "sd__(Intercept)") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root Length Density",
    term == "scale(srl)" ~ "Specific Root Length",
    term == "scale(max_root_depth_cm)" ~ "Max Root Depth",
    term == "scale(plant_size)" ~ "Total Biomass",
    term == "scale(mean_radius_mm)" ~ "Mean Root Diameter",
    TRUE ~ term)) %>%
  
  # reorder rows manually
  slice(match(c("Root Length Density",
                "Mean Root Diameter", 
                "Total Biomass", 
                "Max Root Depth", 
                "Specific Root Length"),
              term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              point.color = "red",
              stats.labels = FALSE,
              conf.int = TRUE) +
  
  labs(x = NULL, y = NULL)
  
# plot: total ret - WW ---------------------------------------------------------

plot_total_ret_WW <- lmm_total_ret_WW %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__Observation",
         term != "(Intercept)",
         term != "sd__(Intercept)") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root Length Density",
    term == "scale(srl)" ~ "Specific Root Length",
    term == "scale(max_root_depth_cm)" ~ "Max Root Depth",
    term == "scale(plant_size)" ~ "Total Biomass",
    term == "scale(mean_radius_mm)" ~ "Mean Root Diameter",
    TRUE ~ term)) %>%
  
  # reorder rows manually
  slice(match(c("Root Length Density",
                "Mean Root Diameter", 
                "Total Biomass", 
                "Max Root Depth", 
                "Specific Root Length"),
              term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              point.color = "blue",
              stats.labels = FALSE,
              conf.int = TRUE) +
  
  labs(x = NULL, y = NULL)

# multipanel figure  -----------------------------------------------------------

# multi-panel figure -----------------------------------------------------------

list_labels <- list("A", "B", "C", "D")

multipan_fig <- plot_grid(plot_avg_ret_WD,
                          plot_avg_ret_WW,
                          plot_total_ret_WD,
                          plot_total_ret_WW,
                          labels = list_labels)

# save the data ----------------------------------------------------------------
ggsave(plot = multipan_fig,
       here("output/figures", "fig1-multipan-fig-RET.png"),
       width = 7.5, height = 5.4, 
       device = "png")