# libraries ----
library(here)
library(dplyr)
library(tidyr)

# import ----
above_biomass_raw <- readRDS(here("data/project_data/working",
                                  "aboveground_biomass_raw.rds"))

# clean ----

# change all "-" values to NA
# is there a tidy version to do this?
above_biomass_raw[above_biomass_raw== "-"] <- NA

# convert block and treatment into factor class
above_biomass_clean <- above_biomass_raw %>%
  mutate(block = as.factor(block),
         treatment = as.factor(treatment))

# split pot-id column to species ID and individual ID
above_biomass_clean <- above_biomass_clean %>%
  separate(col = pot_ID, into = c("spp", "ind"), sep = "-")

# convert appropriate cols into numeric values
above_biomass_clean <- above_biomass_clean %>%
  mutate(above_dry_g = as.numeric(above_dry_g),
         ind = as.numeric(ind))

# select appropriate cols 
above_biomass_clean <- above_biomass_clean %>%
  select(block, treatment, spp, ind, above_dry_g)

# save the data! ----
saveRDS(above_biomass_clean, 
        file = here("data/project_data/working", 
                    "above_biomass_clean.rds"))
