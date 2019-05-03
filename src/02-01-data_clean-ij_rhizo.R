# libraries ----
library(here)
library(dplyr)
library(janitor)
library(tidyr)

# import ----
ij_rhizo_raw <- readRDS(here("data/project_data/working", 
                             "ij_rhizo_output_raw.rds"))

# clean ----

# change all "-" values to NA
# is there a tidy version to do this?
ij_rhizo_raw[ij_rhizo_raw == "-"] <- NA

# clean up column names
ij_rhizo_clean <- ij_rhizo_raw %>%
  clean_names()

# split pot-id column to species ID and individual ID
ij_rhizo_clean <- ij_rhizo_raw %>%
  separate(col = pot_id, into = c("spp", "ind"), sep = "-")

# convert block and treatment into a factor class
ij_rhizo_clean <- ij_rhizo_clean %>%
  mutate(block = as.factor(block),
         trt   = as.factor(trt)) %>%
  rename(treatment = trt)

# convert root trait values into numeric values
ij_rhizo_clean <- ij_rhizo_clean %>%
  mutate(mean_radius_mm = as.numeric(mean_radius_mm),
         raw_total_length_mm = as.numeric(raw_total_length_mm),
         `volume_cm-cubed` = as.numeric(`volume_cm-cubed`),
         ind = as.numeric(ind))

# convert raw total length from mm to m 
ij_rhizo_clean <- ij_rhizo_clean %>%
  mutate(raw_total_length_m = raw_total_length_mm/1000)

# create root length density value (total root length/soil volume)
ij_rhizo_clean <- ij_rhizo_clean %>%
  mutate(rld = raw_total_length_m/`volume_cm-cubed`)

# remove columns
ij_rhizo_clean <- ij_rhizo_clean %>%
  select(block, spp, ind, treatment, 
         raw_total_length_m, mean_radius_mm, rld)

# save the data! ----
saveRDS(ij_rhizo_clean, here("data/project_data/working", 
                             "ij_rhizo_output_clean.rds"))



