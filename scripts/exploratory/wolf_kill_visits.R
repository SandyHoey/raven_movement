#looking to see how often ravens visit wolf kills during different movement decisions

library(dplyr)
library(sf)
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
                     northing = y)) %>% 
  filter(complete.cases(.)) %>% 
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12")


# leaving the territory, but not visiting the hunting area-------------------------------------------------------------------------

# reading in GPS data for this particular case
leave_no_hunt_gps <- readr::read_csv("data/clean/raven_gps_outside_terr_no_hunt.csv") %>% 
  dplyr::select(study_local_timestamp, utm_easting, utm_northing) %>% 
  filter(complete.cases(.)) %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("utm_easting", "utm_northing"), crs = "+proj=utm +zone=12")


# adding column with distance to closest wolf kill
# wolf kills are available for that entire winter
leave_no_hunt_gps$dist2kill <- NA
# loop comparing each GPS point to all wolf kills
for(i in 1:nrow(leave_no_hunt_gps)){
  tmp_gps <- leave_no_hunt_gps[i,]
  
  tmp_kills <- wolf_kills %>% 
    # only kills that were made within last 2 weeks
    filter(difftime(as.Date(tmp_gps$study_local_timestamp), dod, 
                    units = "days") <= 14)
  
  # calculating distance, but only for the closest wolf kill
  leave_no_hunt_gps$dist2kill[i,] <- st_distance(tmp_gps, tmp_kills, by_element = T)
  
}

