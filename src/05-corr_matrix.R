# libraries --------------------------------------------------------------------
library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

# import -----------------------------------------------------------------------

df <- readRDS(here("data/project_data/final", "traits_EF_clean_df.rds")) 

# cor matrix: WW ---------------------------------------------------------------

cormat <- df %>%
  ungroup() %>%
  rename(RMF = rmf, 
         SRL = srl, 
         PS  = plant_size, 
         MRD = max_root_depth_cm, 
         MD  = mean_radius_mm, 
         RLD = rld) %>%
  drop_na() %>%
  ggpairs(mapping = aes(color = treatment), 
          columns = c("RMF", "SRL", "PS", "MRD", "MD", "RLD"))

ggsave(plot = cormat, 
       here("output/figures", "cormat.png"),
       width = 7.5, height = 5, 
       device = "png") 
