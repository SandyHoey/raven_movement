#looking to see how often ravens visit wolf kills during different movement decisions

library(dplyr)
`%like%` <- data.table::`%like%`

# reading in wolf kill data (both wolf project database and RF predictive)
wp_kills <- readr::read_csv("data/raw/wolf_project_carcass_data.csv") %>% 
  janitor::clean_names() %>% 
  # removing cat kills
  filter(cod %like% "WOLF") %>% 
  #getting best coordinates
  mutate(easting = case_when(!is.na(ground_east) ~ ground_east,
                             !is.na(aerial_east) ~ aerial_east,
                             !is.na(est_ground_east) ~ est_ground_east),
         northing = case_when(!is.na(ground_north) ~ ground_north,
                              !is.na(aerial_north) ~ aerial_north,
                              !is.na(est_ground_north) ~ est_ground_north),
         #fixing date column format
         dod = lubridate::mdy(dod))

rf_kills <- readr::read_rds("data/raw/mergedkills_wolf_winter_RF_spec95.rds") %>% janitor::clean_names() %>% ungroup

# creating a single combined kill dataframe with the start date and coordinates
wolf_kills <- wp_kills %>% 
  dplyr::select(dod, easting, northing) %>% 
  bind_rows(rf_kills %>% 
              mutate(dod = as.Date(kill_start_date)) %>% 
              dplyr::select(dod, x, y) %>% 
              rename(easting = x,
                     northing = y))


# leaving the territory, but not visiting the hunting area-------------------------------------------------------------------------

# reading in GPS data for this particular case
leave_no_hunt_gps <- readr::read_csv("data/clean/raven_gps_outside_terr_no_hunt.csv")
