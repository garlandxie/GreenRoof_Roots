# libraries --------------------------------------------------------------------
library(here)
library(tidyverse)
library(visdat)

# import -----------------------------------------------------------------------
pot_weights_df <- readRDS(here("data/project_data/working",
                                "pot_weight_clean_wide.rds"))

# check the packaging ----------------------------------------------------------
glimpse(pot_weights_df)
head(pot_weights_df, n = 5)
tail(pot_weights_df, n = 5)

# visualize missing data -------------------------------------------------------
vis_dat(pot_weights_df)
vis_miss(pot_weights_df)

# clean ---------------------------------------------------------------
ecosystem_func_df <- pot_weights_df %>%
  
  # calculate green roof EFs for each module for each session
  mutate(water_capture = ten_min_delay_T2 - pre_water_T1,
         water_loss    = ten_min_delay_T2 - weight_48hr_T4) %>%
  
  # calculate green roof EFS for each module across all sessions
  group_by(block, species, treatment, ind) %>%
  summarize(
    total_water_capture = sum(water_capture, na.rm = TRUE),
    total_water_loss    = sum(water_loss, na.rm = TRUE),
    
    # note: should standardize by soil treatments (sensu Lundholm 2015)
    avg_water_capture   = mean(water_capture, na.rm = TRUE),
    avg_water_loss      = mean(water_loss, na.rm = TRUE),
    
    # number of unique sessions: should be 12 for WD and 20 for WW
    n_sessions = n()
  )

# plot: influence of species composition on ecosystem functions ----------------

# avg water capture
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(species, avg_water_capture),
             y = avg_water_capture)) + 
  geom_boxplot() +
  coord_flip() + 
  facet_wrap(~treatment)

# avg water loss 
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(species, avg_water_loss),
             y = avg_water_capture)) + 
  geom_boxplot() +
  coord_flip() + 
  facet_wrap(~treatment)

# total water capture
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(species, total_water_capture),
             y = total_water_capture)) + 
  geom_boxplot() +
  coord_flip() + 
  facet_wrap(~treatment)

# total water loss - applied log transform to show the trends more clearly
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(species, log(total_water_loss)),
             y = log(total_water_loss))) + 
  geom_boxplot() +
  coord_flip() + 
  facet_wrap(~treatment)

# plot: relationship between ecosystem functions -------------------------------

# total stormwater capture vs total ET
ecosystem_func_df %>%
  ggplot(aes(x = total_water_capture, y = total_water_loss)) + 
  geom_smooth(method = "lm", col = "red") + 
  geom_point(alpha = 0.3) + 
  facet_wrap(~treatment) + 
  labs(y = "Total ET",
       x = "Total Stormwater Capture")

ecosystem_func_df %>%
  ggplot(aes(x = total_water_loss, y = total_water_capture)) + 
  geom_smooth(method = "lm", col = "red") + 
  geom_point(alpha = 0.3) + 
  facet_wrap(~treatment) + 
  labs(y = "Total ET",
       x = "Total Stormwater Capture")

# avg water capture vs avg water loss 
ecosystem_func_df %>%
  ggplot(aes(x = avg_water_loss, y = avg_water_capture)) + 
  geom_smooth(method = "lm", col = "red") + 
  geom_point(alpha = 0.3) + 
  facet_wrap(~treatment) + 
  labs(y = "Total ET",
       x = "Total Stormwater Capture")

ecosystem_func_df %>%
  ggplot(aes(x = avg_water_capture, y = avg_water_loss)) + 
  geom_smooth(method = "lm", col = "red") + 
  geom_point(alpha = 0.3) + 
  facet_wrap(~treatment) + 
  labs(y = "Total ET",
       x = "Total Stormwater Capture")

# plots: influence of experimental block design --------------------------------

# total water capture
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(species, total_water_capture), 
             y = total_water_capture)) +
  geom_boxplot() + 
  coord_flip() + 
  facet_wrap(~block, ncol = 5) + 
  theme(axis.text.x = element_blank())

# total water loss
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(species, total_water_loss), 
             y = total_water_loss)) +
  geom_boxplot() + 
  coord_flip() + 
  facet_wrap(~block, ncol = 5) + 
  labs(x = NULL) + 
  theme(axis.text.x = element_blank())

# avg water loss 
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(species, avg_water_loss), 
             y = avg_water_loss)) +
  geom_boxplot() + 
  coord_flip() + 
  facet_wrap(~block, ncol = 5) + 
  labs(x = NULL) + 
  theme(axis.text.x = element_blank())

# avg water capture
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(species, avg_water_capture), 
             y = avg_water_capture)) +
  geom_boxplot() + 
  coord_flip() + 
  facet_wrap(~block, ncol = 5) + 
  labs(x = NULL) + 
  theme(axis.text.x = element_blank())

