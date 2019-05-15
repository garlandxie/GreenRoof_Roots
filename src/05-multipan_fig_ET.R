# libraries --------------------------------------------------------------------
library(broom)
library(ggstatsplot)
library(here)
library(dplyr)
library(ggplot2)
library(cowplot)

# import -----------------------------------------------------------------------
lmm_avg_water_loss_WD <- readRDS(here("data/project_data/final",
                                      "lmm_avg_loss_WD.rds"))

lmm_avg_water_loss_WW <- readRDS(here("data/project_data/final",
                                      "lmm_avg_loss_WW.rds"))

lmm_total_ET_WD <- readRDS(here("data/project_data/final",
                                "lmm_total_ET_WD.rds"))

lmm_total_ET_WW <- readRDS(here("data/project_data/final",
                                "lmm_total_ET_WW.rds"))

# plot: avg water loss - WD  ---------------------------------------------------

plot_avg_ET_WD <- lmm_avg_water_loss_WD %>%
  
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
  
  slice(match(c("Root Length Density",
                "Mean Root Diameter", 
                "Total Biomass", 
                "Max Root Depth", 
                "Specific Root Length"),
        term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              stats.labels = FALSE,
              conf.int = TRUE,
              point.color = "red") + 
  
  labs(x = NULL, y = NULL)


# plot: avg water loss - WW  ---------------------------------------------------

plot_avg_ET_WW <- lmm_avg_water_loss_WW %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__Observation") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root Length Density",
    term == "scale(srl)" ~ "Specific Root Length",
    term == "scale(max_root_depth_cm)" ~ "Max Root Depth",
    term == "scale(plant_size)" ~ "Total Biomass",
    term == "scale(mean_radius_mm)" ~ "Mean Root Diameter",
    TRUE ~ term)) %>%
  
  slice(match(c("Root Length Density",
                "Mean Root Diameter", 
                "Total Biomass", 
                "Max Root Depth", 
                "Specific Root Length"),
              term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              stats.labels = FALSE,
              conf.int = TRUE,
              point.color = "blue") + 
  
  labs(x = NULL, y = NULL)


# plot: total ET - WD ----------------------------------------------------------

plot_total_ET_WD <- lmm_total_ET_WD %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__Observation") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root Length Density",
    term == "scale(srl)" ~ "Specific Root Length",
    term == "scale(max_root_depth_cm)" ~ "Max Root Depth",
    term == "scale(plant_size)" ~ "Total Biomass",
    term == "scale(mean_radius_mm)" ~ "Mean Root Diameter",
    TRUE ~ term)) %>%
  
  slice(match(c("Root Length Density",
                "Mean Root Diameter", 
                "Total Biomass", 
                "Max Root Depth", 
                "Specific Root Length"),
              term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              stats.labels = FALSE,
              point.color = "red",
              conf.int = TRUE) + 
  
  labs(x = NULL, y = NULL)

# plot: total ET - WW ----------------------------------------------------------

plot_total_ET_WW <- lmm_total_ET_WW %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # remove random effect coefficient
  filter(term != "sd__Observation") %>%
  
  # rename 
  mutate(term = case_when(
    term == "scale(rld)" ~ "Root Length Density",
    term == "scale(srl)" ~ "Specific Root Length",
    term == "scale(max_root_depth_cm)" ~ "Max Root Depth",
    term == "scale(plant_size)" ~ "Total Biomass",
    term == "scale(mean_radius_mm)" ~ "Mean Root Diameter",
    TRUE ~ term)) %>%
  
  slice(match(c("Root Length Density",
                "Mean Root Diameter", 
                "Total Biomass", 
                "Max Root Depth", 
                "Specific Root Length"),
              term)) %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              stats.labels = FALSE,
              point.color = "blue",
              conf.int = TRUE) +
  
  labs(x = NULL, y = NULL)

# multi-panel figure -----------------------------------------------------------

list_labels <- list("A", "B", "C", "D")

multipan_fig <- plot_grid(plot_avg_ET_WD,
                          plot_avg_ET_WW,
                          plot_total_ET_WD,
                          plot_total_ET_WW,
                          labels = list_labels)

# save the data ----------------------------------------------------------------
ggsave(plot = multipan_fig,
       here("output/figures", "fig1-multipan-fig-ET.png"),
       width = 7.5, height = 5.4, 
       device = "png")