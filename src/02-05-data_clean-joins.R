# libraries ----
library(here)
library(dplyr)

# import ----
max_root_depth_df <- readRDS(here("data/project_data/working",
                                  "max_root_depth_clean.rds"))

pot_weight_clean_wide <- readRDS(here("data/project_data/working",
                                      "pot_weight_clean_wide.rds"))

above_bm_df <- readRDS(here("data/project_data/working",
                            "above_biomass_clean.rds"))

below_bm_df <- readRDS(here("data/project_data/working", 
                            "belowground_biomass_clean.rds"))

ij_rhizo_df <- readRDS(here("data/project_data/working",
                            "ij_rhizo_output_clean.rds"))

# create df - rmf -------------------------------------------------------------------

rmf_df <- left_join(above_bm_df, below_bm_df) %>%
  mutate(rmf = total_root_g/sum(above_dry_g, total_root_g, na.rm = TRUE)) %>%
  select(block, spp, ind, treatment, rmf)

# create df - plant size ------------------------------------------------------------

plant_size_df <- left_join(above_bm_df, below_bm_df) %>%
  mutate(plant_size = above_dry_g + total_root_g) %>%
  select(block, spp, ind, treatment, plant_size)

# create df - specific root length  --------------------------------------------

srl_mrd_df <- left_join(ij_rhizo_df, below_bm_df) %>%
  mutate(srl = raw_total_length_m/fine_root_g) %>%
  select(block, spp, ind, treatment, srl, mean_radius_mm, rld)

# create df - ecosystem functions  ---------------------------------------------

EF_df <- pot_weight_clean_wide %>%
  
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
    avg_water_loss      = mean(water_loss, na.rm = TRUE)
    
    )

# join -------------------------------------------------------------------------

# recursively perform left join operations
# final dataset should have both EF's and candidate traits
list_dfs <- list(EF_df, rmf_df, srl_mrd_df, plant_size_df, max_root_depth_df)

traits_EF_clean_df <- Reduce(left_join, list_dfs)


# save the data ----------------------------------------------------------------

saveRDS(traits_EF_clean_df, here("data/project_data/final",
                                 "traits_EF_clean_df.rds"))
