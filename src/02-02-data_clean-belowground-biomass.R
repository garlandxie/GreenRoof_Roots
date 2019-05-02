# libraries -----
library(here)
library(dplyr)

# import ----
belowground_bm_raw <- readRDS(here("data/project_data/working",
                                   "belowground_biomass_raw.rds"))

# check packaging ----
tibble::glimpse(belowground_bm_raw)
head(belowground_bm_raw, n = 5)
tail(belowground_bm_raw, n = 5)

# clean ----

# calculate dry weight of fine roots below 2 mm
fine_root_g <- belowground_bm_raw %>% 
  tidyr::separate(col = "pot_ID", into = c("spp", "ind"), sep = "-") %>% 
  rename(root_type = `fine/coarse`) %>% 
  filter(root_type == "fine") %>%
  group_by(block, spp, ind, treatment) %>%
  summarize(fine_root_g = sum(below_dry_g, na.rm = TRUE))

# calculate dry weight of belowground biomass (coarse + fine)
total_root_g <-  belowground_bm_raw %>% 
  tidyr::separate(col = "pot_ID", into = c("spp", "ind"), sep = "-") %>% 
  group_by(block, spp, ind, treatment) %>%
  summarize(total_root_g = sum(below_dry_g, na.rm = TRUE)) %>%

# join operation ---------------------------------------------------------------
root_bm_clean <- inner_join(fine_root_g, total_root_g, 
                            by = c("block", "spp", "treatment", "ind")) 

# save data --------------------------------------------------------------------
saveRDS(root_bm_clean, here("data/project_data/working", 
                            "belowground_biomass_clean.rds"))
