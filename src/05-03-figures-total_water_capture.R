# libraries --------------------------------------------------------------------
library(broom)
library(ggstatsplot)
library(here)
library(ggplot2)

# import -----------------------------------------------------------------------
lmm_total_ret_WD <- readRDS(here("data/project_data/final",
                                     "lmm_total_ret_WD.rds"))

lmm_total_ret_WW <- readRDS(here("data/project_data/final",
                                     "lmm_total_ret_WW.rds"))

# plot -------------------------------------------------------------------------
lmm_total_ret_WD %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              sort = "ascending", # ranked by coefficient values
              stats.labels = FALSE,
              conf.int = TRUE,
              title = "EF: Total Retention, TRT: WD") %>%
  
  # save the plot in the figures folder
  ggsave(here("output/figures", "fig5-coef_plot-total_ret_WD.png"),
         plot = .,
         width = 6.8, height = 4.94, units = "in",
         device = "png")

lmm_avg_water_ret_WW %>%
  
  # convert lm object into a tibble
  tidy() %>%
  
  # regression coefficient plot
  ggcoefstats(statistic = "t", # t-statistic value
              conf.method = "profile", 
              sort = "ascending",  # ranked by coefficient values
              stats.labels = FALSE,
              conf.int = TRUE,
              title = "EF: Avg Retention, TRT: WW") %>%
  
  ggsave(plot = .,
         here("output/figures", "fig6-coef_plot-total_ret_WW.png"),
         width = 6.8, height = 4.94, units = "in", device = "png")