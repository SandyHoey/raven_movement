#looking to see how often ravens visit wolf kills during different movement decisions

library(dplyr)
library(here)
library(sf)
`%like%` <- data.table::`%like%`

# reading in wolf kill data (both wolf project database and RF predictive)
wp_kills <- readr::read_csv(here("data/raw/wolf_project_carcass_data.csv")) %>% 
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

rf_kills <- readr::read_rds(here("data/raw/mergedkills_wolf_winter_RF_spec95.rds")) %>% janitor::clean_names() %>% ungroup

# creating a single combined kill dataframe with the start date and coordinates
wolf_kills <- wp_kills %>% 
  # only useful columns
  dplyr::select(dod, easting, northing) %>% 
  # adding RF predictive kills
  bind_rows(rf_kills %>% 
              mutate(dod = as.Date(kill_start_date)) %>% 
              dplyr::select(dod, x, y) %>% 
              rename(easting = x,
                     northing = y)) %>% 
  filter(complete.cases(.)) %>% 
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12")


# leaving the territory, but not visiting the hunting area-------------------------------------------------------------------------

# reading in GPS data for this particular case
leave_no_hunt_gps <- readr::read_csv(here("data/clean/raven_gps_outside_terr_no_hunt.csv")) %>% 
  # only useful columns
  dplyr::select(individual_local_identifier, study_local_timestamp, utm_easting, utm_northing) %>%
  # simpler column names
  rename(raven_id = individual_local_identifier,
         date = study_local_timestamp,
         easting = utm_easting,
         northing = utm_northing) %>% 
  # remove time from date column
  mutate(date = as.Date(date)) %>%
  # only complete rows
  filter(complete.cases(.)) %>% 
  # add crs
  st_drop_geometry() %>% 
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12")


# adding column with distance to closest wolf kill
# wolf kills are available for 2 weeks after dod
leave_no_hunt_gps$dist2kill <- NA
# loop comparing each GPS point to all wolf kills
for(i in 1:nrow(leave_no_hunt_gps)){
  tmp_gps <- leave_no_hunt_gps[i,]
  
  tmp_kills <- wolf_kills %>% 
    # only kills that were made within last 2 weeks
    filter(as.numeric(difftime(tmp_gps$date, dod, 
                    units = "days")) <= 14,
           as.numeric(difftime(tmp_gps$date, dod, 
                               units = "days")) >= 0)
  
  #if there are no kills, then make distance NA
  if(nrow(tmp_kills) > 0){
    # calculating distance, but only for the closest wolf kill
    leave_no_hunt_gps[i, "dist2kill"] <- as.numeric(min(st_distance(tmp_gps, tmp_kills, units = "meters")))
  } else(leave_no_hunt_gps[i, "dist2kill"] <- NA)
}

# how many days each raven has GPS points are within 500 meters
leave_no_hunt_wolf_kills <- leave_no_hunt_gps %>% 
  st_drop_geometry() %>% 
  # only GPS points within 500 m of kill
  filter(dist2kill < 500) %>% 
  #a single row for each raven/day
  group_by(raven_id, date) %>% 
  slice(1) %>% 
  # remove date grouping (only ID)
  ungroup(date) %>% 
  summarize(days_visit = n()) %>% 
  #adding the total number of days the raven left the territory by didn't visit hunting
  left_join(leave_no_hunt_gps %>% 
              st_drop_geometry %>% 
              group_by(raven_id, date) %>% 
              slice(1) %>% 
              ungroup(date) %>% 
              summarize(total_days = n())) %>% 
  #calculating proportion of days visiting wolf kill outside territory without visiting the hunting area
  mutate(prop_visit = days_visit/total_days)


# visiting the hunting area -------------------------------------------------------------------------

hunt_gps <- readr::read_csv(here("data/clean/raven_gps_covariates.csv")) %>% 
  #only days with visit to hunting area
  filter(hunt_bin == 1) %>% 
  # only useful columns
  dplyr::select(individual_local_identifier, study_local_timestamp, utm_easting, utm_northing) %>%
  # simpler column names
  rename(raven_id = individual_local_identifier,
         date = study_local_timestamp,
         easting = utm_easting,
         northing = utm_northing) %>% 
  # remove time from date column
  mutate(date = as.Date(date)) %>%
  # only complete rows
  filter(complete.cases(.)) %>% 
  # add crs
  st_drop_geometry() %>% 
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12")


# adding column with distance to closest wolf kill
# wolf kills are available for 2 weeks after dod
hunt_gps$dist2kill <- NA
# loop comparing each GPS point to all wolf kills
for(i in 1:nrow(hunt_gps)){
  tmp_gps <- hunt_gps[i,]
  
  tmp_kills <- wolf_kills %>% 
    # only kills that were made within last 2 weeks
    filter(as.numeric(difftime(tmp_gps$date, dod, 
                               units = "days")) <= 14,
           as.numeric(difftime(tmp_gps$date, dod, 
                               units = "days")) >= 0)
  
  #if there are no kills, then make distance NA
  if(nrow(tmp_kills) > 0){
    # calculating distance, but only for the closest wolf kill
    hunt_gps[i, "dist2kill"] <- as.numeric(min(st_distance(tmp_gps, tmp_kills, units = "meters")))
  } else(hunt_gps[i, "dist2kill"] <- NA)
}


# how many days each raven has GPS points are within 500 meters
hunt_wolf_kills <- hunt_gps %>% 
  st_drop_geometry() %>% 
  # only GPS points within 500 m of kill
  filter(dist2kill < 500) %>% 
  #a single row for each raven/day
  group_by(raven_id, date) %>% 
  slice(1) %>% 
  # remove date grouping (only ID)
  ungroup(date) %>% 
  summarize(days_visit = n()) %>% 
  #adding the total number of days the raven left the territory by didn't visit hunting
  left_join(hunt_gps %>% 
              st_drop_geometry %>% 
              group_by(raven_id, date) %>% 
              slice(1) %>% 
              ungroup(date) %>% 
              summarize(total_days = n())) %>% 
  #calculating proportion of days visiting wolf kill outside territory without visiting the hunting area
  mutate(prop_visit = days_visit/total_days)


# other stuff -------------------------------------------------------------

mean(leave_no_hunt_wolf_kills$prop_visit)
leave_no_hunt_wolf_kills %>% 
  summarize(visit = sum(days_visit),
            total = sum(total_days))

mean(hunt_wolf_kills$prop_visit)
hunt_wolf_kills %>% 
  summarize(visit = sum(days_visit),
            total = sum(total_days))
