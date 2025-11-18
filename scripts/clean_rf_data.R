#takes the random forest predicted wolf kills for the winter (Nov, Dec, Mar) and minimizes it to a usable format for calculating covariates

library(dplyr)

#reading in RF predicted wolf kills (only has winter study periods)
kill_data_rf <- readr::read_rds("data/raw/mergedkills_wolf_winter_RF_spec95.rds") %>% 
  janitor::clean_names() %>% 
  #adding dedicated date column
  mutate(date = as.Date(kill_start_date)) %>% 
  #renaming coordinate columns
  rename(easting = x,
         northing = y)
