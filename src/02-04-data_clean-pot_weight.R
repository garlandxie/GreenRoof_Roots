# libraries --------------------------------------------------------------------
library(here)
library(tidyverse)

# import -----------------------------------------------------------------------
pot_weight_raw <- readRDS(here("data/project_data/working", 
                               "pot_weight_raw.rds"))

# check packaging --------------------------------------------------------------
glimpse(pot_weight_raw)
head(pot_weight_raw, n = 5)
tail(pot_weight_raw, n = 5)

# clean ------------------------------------------------------------------------

# change all "-" values to NA
# is there a tidy version to do this?
pot_weight_raw[pot_weight_raw == "-"] <- NA

# convert block and treatment into factor class
pot_weight_clean <- pot_weight_raw %>%
  mutate(block = as.factor(block),
         treatment = as.factor(treatment))

# select appropriate cols first
pot_weight_clean <- pot_weight_clean %>%
  select(session, block, pot_ID, treatment, period, weight_grams)

# separate
pot_weight_clean <- pot_weight_clean %>%
  separate(col = "pot_ID", into = c("species", "ind"), sep = "-")

# convert into numeric values
pot_weight_clean <- pot_weight_clean %>%
  mutate(ind = as.numeric(ind))

pot_weight_clean <- pot_weight_clean %>%
  arrange(session, block, species, ind, treatment, period)

# spread -----------------------------------------------------------------------

# remember: fix DASP-5! there are duplicates
pot_weight_wide <- pot_weight_clean %>%
  spread(period, weight_grams) %>%
  rename("pre_water_T1"     = T1,
         "ten_min_delay_T2" = T2,
         "weight_24hr_T3"   = T3,
         "weight_48hr_T4"   = T4)

# save the data ----------------------------------------------------------------
saveRDS(pot_weight_wide, 
        here("data/project_data/working", 
             "pot_weight_clean_wide.rds"))
