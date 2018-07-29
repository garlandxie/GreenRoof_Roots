# Libraries -------------------------------------------------------------------
library(googlesheets)
library(dplyr)

# Get access to Google Sheets -------------------------------------------------

# get shareable link of googlesheet file (for reproducibility)
url_msc_data <- "https://docs.google.com/spreadsheets/d/1jtiaNA2VG_fwnsMAfH-Bu1JdC0Js7Lod4JqLxoMwbek/edit?usp=sharing"

# access the googlesheet file by url
msc_data <- gs_url(url_msc_data) 

# return content of 'Weight' ws as a data frame
msc_weight <- msc_data %>% gs_read(ws = "Weight")

# 