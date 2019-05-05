# libraries --------------------------------------------------------------------
library(here)
library(tidyverse)
library(visdat)
library(lubridate)
library(lmodel2)

# import -----------------------------------------------------------------------
pot_weights_df <- readRDS(here::here("data/project_data/working",
                                "pot_weight_clean_wide.rds"))

pot_weights_raw <- readRDS(here::here("data/project_data/working", 
                                "pot_weight_raw.rds"))

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
  group_by(block, spp, treatment, ind) %>%
  
  summarize(
    total_water_capture = sum(water_capture, na.rm = TRUE),
    total_water_loss    = sum(water_loss, na.rm = TRUE),
    
    # note: should standardize by soil treatments (sensu Lundholm 2015)
    avg_water_capture   = mean(water_capture, na.rm = TRUE),
    avg_water_loss      = mean(water_loss, na.rm = TRUE),
    
    # number of unique sessions: should be 12 for WD and 20 for WW
    n_sessions = n()
  )


# plot: time series of ET and capture ------------------------------------------

# create POSiXct date column
time_series_df <- pot_weights_raw %>%
  separate(col = "pot_ID", into = c("spp", "ind"), sep = "-") %>%
  mutate(date = as_date(paste(sample_year, sample_month, sample_day))) %>%
  select(date, session, block, spp, ind, treatment, period, weight_grams)

# pot-weight over time 
time_series_df %>%
  ggplot(aes(x = date, y = weight_grams)) + 
  geom_jitter(alpha = 0.01) + 
  geom_smooth(method = "loess", se = TRUE) + 
  facet_wrap(~period, ncol = 4) +
  labs(x = "date", 
       y = "pot weight (gm)") +
  theme(axis.text.x = element_text(angle = 90))

# influence of species composition on pot weight throughout time
time_series_df %>%
  ggplot(aes(x = date, y = weight_grams, col = spp)) + 
  geom_smooth(method = "loess", se = TRUE, show.legend = FALSE) + 
  facet_wrap(~period, ncol = 4) +
  labs(x = "date", 
       y = "pot weight (gm)") +
  theme(axis.text.x = element_text(angle = 90))

# influence of water regime treatment on pot weight throughout time
time_series_df %>%
  ggplot(aes(x = date, y = weight_grams, col = treatment)) + 
  geom_smooth(method = "loess", se = TRUE) + 
  facet_wrap(~period, ncol = 4) +
  labs(x = "date", 
       y = "pot weight (gm)") + 
  theme(axis.text.x = element_text(angle = 90))

# influence of block design on pot weight over time
time_series_df %>%
  ggplot(aes(x = date, y = weight_grams)) + 
  geom_jitter(alpha = 0.1) + 
  geom_smooth(method = "loess", se = FALSE) + 
  facet_wrap(block~period, ncol = 4) + 
  coord_flip() + 
  labs(x = NULL, y = NULL) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
  )
# plot: influence of species composition on ecosystem functions ----------------

# avg water capture
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(spp, avg_water_capture),
             y = avg_water_capture)) + 
  geom_boxplot() +
  coord_flip() + 
  facet_wrap(~treatment) +
  labs(x = "Average Water Capture",
       y = "Spp")

# avg water loss 
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(spp, avg_water_loss),
             y = avg_water_capture)) + 
  geom_boxplot() +
  coord_flip() + 
  facet_wrap(~treatment) +
  labs(x = "Average Water Loss",
       y = "Spp")

# total water capture
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(spp, total_water_capture),
             y = total_water_capture)) + 
  geom_boxplot() +
  coord_flip() + 
  facet_wrap(~treatment) +
  labs(x = "Total Water Capture",
       y = "Spp")

# total water loss - applied log transform to show the trends more clearly
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(spp, log(total_water_loss)),
             y = log(total_water_loss))) + 
  geom_boxplot() +
  coord_flip() + 
  facet_wrap(~treatment) + 
  labs(x = "Log(Total Water Loss)",
       y = "Spp")

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
  ggplot(aes(x = fct_reorder(spp, total_water_capture), 
             y = total_water_capture)) +
  geom_boxplot() + 
  coord_flip() + 
  facet_wrap(~block, ncol = 5) + 
  theme(axis.text.x = element_blank())

# total water loss
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(spp, total_water_loss), 
             y = total_water_loss)) +
  geom_boxplot() + 
  coord_flip() + 
  facet_wrap(~block, ncol = 5) + 
  labs(x = NULL) + 
  theme(axis.text.x = element_blank())

# avg water loss 
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(spp, avg_water_loss), 
             y = avg_water_loss)) +
  geom_boxplot() + 
  coord_flip() + 
  facet_wrap(~block, ncol = 5) + 
  labs(x = NULL) + 
  theme(axis.text.x = element_blank())

# avg water capture
ecosystem_func_df %>%
  ggplot(aes(x = fct_reorder(spp, avg_water_capture), 
             y = avg_water_capture)) +
  geom_boxplot() + 
  coord_flip() + 
  facet_wrap(~block, ncol = 5) + 
  labs(x = NULL) + 
  theme(axis.text.x = element_blank())

# sma --------------------------------------------------------------------------

# NOTE: this is NOT confirmatory data analysis
# water capture and ET should be a circular relationship
# so a standard major axis (SMA) regression should be done here 

# split datasets 
EF_WW <- ecosystem_func_df %>%
  filter(treatment == "WD") 

EF_WD <- ecosystem_func_df %>%
  filter(treatment == "WD")

# sma regression
lmodel2(formula = total_water_capture ~ total_water_loss, data = EF_WW)
lmodel2(formula = total_water_capture ~ total_water_loss, data = EF_WD)


