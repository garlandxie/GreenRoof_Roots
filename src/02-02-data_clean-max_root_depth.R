# libraries ----
library(here)
library(dplyr)
library(tidyr)

# import ----
max_root_depth_raw <- readRDS(here("data/project_data/working", 
                                   "max_root_depth_raw.rds"))

# clean ----

# change all "-" values to NA
# is there a tidy version to do this?
max_root_depth_raw[max_root_depth_raw == "-"] <- NA

# convert block and treatment into factor class
max_root_depth_clean <- max_root_depth_raw %>%
  mutate(block = as.factor(block),
         treatment = as.factor(treatment))

# split pot-id column to species ID and individual ID
max_root_depth_clean <- max_root_depth_clean %>%
  separate(col = pot_ID, into = c("spp", "ind"), sep = "-")

# convert block and treatment into a factor class
max_root_depth_clean <- max_root_depth_clean %>%
  mutate(block = as.factor(block),
         trt   = as.factor(treatment))

# convert appropriate cols into numeric values
max_root_depth_clean <- max_root_depth_clean %>%
  mutate(max_root_depth_cm = as.numeric(max_root_depth_cm),
         ind = as.numeric(ind))

# select appropriate cols 
max_root_depth_clean <- max_root_depth_clean %>%
  select(block, treatment, spp, ind, max_root_depth_cm)

# save the data! ----
saveRDS(max_root_depth_clean, 
        file = here("data/project_data/working", 
                    "max_root_depth_clean.rds"))
