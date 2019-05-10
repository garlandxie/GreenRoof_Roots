# libraries --------------------------------------------------------------------
library(broom)
library(ggstatsplot)
library(here)
library(ggplot2)

# import -----------------------------------------------------------------------
lmm_avg_water_loss_WD <- readRDS(here("data/project_data/final",
                                       "lmm_avg_loss_WD.rds"))

lmm_avg_water_loss_WW <- readRDS(here("data/project_data/final",
                                       "lmm_avg_loss_WW.rds"))

# plot -------------------------------------------------------------------------
lmm_avg_water_loss_WD %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              sort = "ascending", # ranked by coefficient values
              stats.labels = FALSE,
              conf.int = TRUE,
              title = "EF: Avg Water Loss, TRT: WD") %>%
  
  # save the plot in the figures folder
  ggsave(here("output/figures", "fig1-coef_plot-avg_water_loss_WD.png"),
         plot = .,
         width = 6.8, height = 4.94, units = "in",
         device = "png")

lmm_avg_water_loss_WW %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              sort = "ascending",  # ranked by coefficient values
              stats.labels = FALSE,
              conf.int = TRUE,
              title = "EF: Avg Water Loss, TRT: WW") %>%
  
  ggsave(plot = .,
         here("output/figures", "fig2-coef_plot-avg_water_loss_WW.png"),
         width = 6.8, height = 4.94, units = "in", device = "png")
