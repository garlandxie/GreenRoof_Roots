# libraries --------------------------------------------------------------------
library(here)
library(ggplot2)
library(dplyr)

# import -----------------------------------------------------------------------

df <- readRDS(here("data/project_data/final", "traits_EF_clean_df.rds"))

# clean ------------------------------------------------------------------------

table_df <- df %>%
  ungroup() %>%
  rename(RMF = rmf, 
         SRL = srl, 
         PS  = plant_size, 
         MRD = max_root_depth_cm, 
         MD  = mean_radius_mm, 
         RLD = rld) %>%
  drop_na()

summ_table <- table_df %>%
  group_by(treatment, spp) %>%
  summarize(avg_RLD = mean(RLD, na.rm = TRUE) ,
            sd_RLD  = sd(RLD, na.rm = TRUE),
            avg_SRL = mean(SRL, na.rm = TRUE), 
            sd_SRL  = sd(SRL, na.rm = TRUE), 
            avg_PS  = mean(PS, na.rm = TRUE), 
            sd_PS   = sd(PS, na.rm = TRUE), 
            avg_MRD = mean(MRD, na.rm = TRUE), 
            sd_MRD  = sd(MRD, na.rm = TRUE),
            avg_MD  = mean(MRD, na.rm = TRUE), 
            sd_MD   = sd(MD, na.rm = TRUE)
  )