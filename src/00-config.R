# libraries --------------------------------------------------------------------
library(here)

# scripts in a data pipeline
# pipeline: import --> clean --> eda --> modelling 

# import -----------------------------------------------------------------------
source(here("src", "01-import_from_google_drive.R"))

# data clean -------------------------------------------------------------------
source(here("src", "02-01-data_clean-ij_rhizo.R"))
source(here("src", "02-02-data_clean-belowground-biomass.R"))
source(here("src", "02-02-data_clean-max_root_depth.R"))
source(here("src", "02-03-data_clean-above_biomass.R"))
source(here("src", "02-04-data_clean-pot_weight.R"))
source(here("src", "02-05-data_clean-joins.R"))

# eda --------------------------------------------------------------------------
source(here("src", "03-eda-aboveground_biomass.R"))
source(here("src", "03-eda-ecosystem_functions.R"))
source(here("src", "03-final_dataset.R"))
source(here("src", "03-ij_rhizo_output.R"))
source(here("src", "03-eda-max_root_depth.R"))

# modelling --------------------------------------------------------------------
source("04-01-lmm_model_fitting.R")

