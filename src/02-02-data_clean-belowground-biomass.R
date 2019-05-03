# libraries -----
library(here)
library(dplyr)
library(tidyr)

# import ----
belowground_bm_raw <- readRDS(here("data/project_data/working",
                                   "belowground_biomass_raw.rds"))

# check packaging ----
tibble::glimpse(belowground_bm_raw)
head(belowground_bm_raw, n = 5)
tail(belowground_bm_raw, n = 5)

# clean ----

# convert all "-" to NA's
belowground_bm_raw[belowground_bm_raw == "-"] <- NA

# calculate dry weight of fine roots below 2 mm
fine_root_g <- belowground_bm_raw %>% 
  tidyr::separate(col = "pot_ID", into = c("spp", "ind"), sep = "-") %>% 
  rename(root_type = `fine/coarse`) %>% 
  mutate(ind = as.numeric(ind),
         below_dry_g = as.numeric(below_dry_g)) %>%
  filter(root_type == "fine") %>%
  group_by(block, spp, ind, treatment) %>%
  summarize(fine_root_g = sum(below_dry_g, na.rm = TRUE))

# calculate dry weight of belowground biomass (coarse + fine)
total_root_g <- belowground_bm_raw %>% 
  tidyr::separate(col = "pot_ID", into = c("spp", "ind"), sep = "-") %>% 
  mutate(ind = as.numeric(ind),
         below_dry_g = as.numeric(below_dry_g)) %>%
  group_by(block, spp, ind, treatment) %>%
  summarize(total_root_g = sum(below_dry_g, na.rm = TRUE))

# join operation ---------------------------------------------------------------

# inner join
root_bm_clean <- inner_join(fine_root_g, total_root_g, 
                            by = c("block", "spp", "treatment", "ind")) 

# assign appropriate data types to cols
root_bm_clean <- root_bm_clean %>%
  ungroup() %>%
  mutate(block = factor(block),
         treatment = factor(treatment))
  
# save data --------------------------------------------------------------------
saveRDS(root_bm_clean, here("data/project_data/working", 
                            "belowground_biomass_clean.rds"))
