# libraries ----
library(dplyr)
library(googlesheets)
library(here)

# authorize ----

# NOTE: this requires login to gmail account
# Once all data has been collected, then change this script to import csv files

gs_auth()

# google spreadsheet shareable links ----
ij_rhizo_link <- "https://docs.google.com/spreadsheets/d/1dUyBGwe6hWNTaPMpv4cr7lyhuMrHjzah947Qp81zjrE/edit?usp=sharing"
msc_data_link <- "https://docs.google.com/spreadsheets/d/1jtiaNA2VG_fwnsMAfH-Bu1JdC0Js7Lod4JqLxoMwbek/edit?usp=sharing"

# import - ij rhizo output df ----
gs_url(ij_rhizo_link) %>%
  gs_read() %>%
  saveRDS(file = here("data/project_data/working", 
                      "ij_rhizo_output_raw.rds"))
  
# import - pot-weight df ----
gs_url(msc_data_link) %>%
  gs_read(ws = "weight") %>%
  saveRDS(file = here("data/project_data/working", 
                      "pot_weight_raw.rds"))

# import - above-ground biomass df ----
gs_url(msc_data_link) %>%
  gs_read(ws = "above_biomass") %>%
  saveRDS(file = here("data/project_data/working", 
                      "aboveground_biomass_raw.rds"))

# import - belowground-biomass df ----
gs_url(msc_data_link) %>%
  gs_read(ws = "below_biomass") %>%
  saveRDS(file = here("data/project_data/original", 
                      "belowground_biomass_raw.rds"))

# import - max root depth df ----
gs_url(msc_data_link) %>%
  gs_read(ws = "max_root_depth") %>%
  saveRDS(file = here("data/project_data/original", 
                      "max_root_depth_raw.rds"))

