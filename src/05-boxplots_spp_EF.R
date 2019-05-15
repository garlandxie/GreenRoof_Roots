library(here)
library(ggplot2)
library(dplyr)
library(forcats)

# import -----------------------------------------------------------------------

traits_EF_clean_df <- readRDS(here("data/project_data/final",
                                   "traits_EF_clean_df.rds"))

# plot: average ET -------------------------------------------------------------

spp_avg_ET <- traits_EF_clean_df %>%
  
  # ggplot
  ggplot(aes(x = fct_reorder(factor(spp),  avg_water_loss), 
             y = avg_water_loss, 
             col = treatment)) + 
  geom_jitter(alpha = 0.1, col = "black") + 
  geom_boxplot() + 
  facet_wrap(~treatment) + 
  coord_flip() +
  labs(x = NULL, y = "Average Evapotranspiration") + 
  theme_minimal()
  
  # ggsave
  ggsave(plot = spp_avg_ET,
         here("output/figures", "boxplot_spp_avg_ET.png"),
         width = 7.5, height = 5, 
         device = "png")
  

  
# plot: total ET -------------------------------------------------------------
  
spp_total_ET <- traits_EF_clean_df %>%
    
    # ggplot
    ggplot(aes(x = fct_reorder(factor(spp),  total_water_loss), 
               y = total_water_loss, 
               col = treatment)) + 
    geom_jitter(alpha = 0.1, col = "black") + 
    geom_boxplot() + 
    facet_wrap(~treatment) + 
    coord_flip() +
    labs(x = NULL, y = "Cumulative Evapotranspiration") + 
    theme_minimal()
  
  # ggsave
  ggsave(plot = spp_total_ET,
         here("output/figures", "boxplot_spp_total_ET.png"),
         width = 7.5, height = 5, 
         device = "png")

# plot: avg water ret ---------------------------------------------------------
  
spp_avg_RET <- traits_EF_clean_df %>%
    
    # ggplot
    ggplot(aes(x = fct_reorder(factor(spp),  avg_water_capture), 
               y = avg_water_capture, 
               col = treatment)) + 
    geom_jitter(alpha = 0.1, col = "black") + 
    geom_boxplot() + 
    facet_wrap(~treatment) + 
    coord_flip() +
    labs(x = NULL, y = "Average Stormwater Retention") + 
    theme_minimal() 
  
  # ggsave
  ggsave(plot = spp_avg_RET,
         here("output/figures", "boxplot_spp_avg_RET.png"),
         width = 7.5, height = 5, 
         device = "png")
  
# plot: total RET --------------------------------------------------------------

spp_total_RET <- traits_EF_clean_df %>%
    
    # ggplot
    ggplot(aes(x = fct_reorder(factor(spp),  total_water_capture), 
               y = total_water_capture, 
               col = treatment)) + 
    geom_jitter(alpha = 0.1, col = "black") + 
    geom_boxplot() + 
    facet_wrap(~treatment) + 
    coord_flip() +
    labs(x = NULL, y = "Cumulative Stormwater Retention") + 
    theme_minimal() 
  
  # ggsave
  ggsave(plot = spp_total_RET,
         here("output/figures", "boxplot_spp_total_RET.png"),
         width = 7.5, height = 5, 
         device = "png")
  
  

