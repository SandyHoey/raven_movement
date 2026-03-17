# looking to see how often ravens visit wolf kills during different movement decisions

library(dplyr)
library(here)
library(sf)
library(lubridate)
`%like%` <- data.table::`%like%`

# removing wolf kills that are in the hunting regions
# reading in Gardiner hunting region shapefile
mtfwp_hunt_poly <- st_read("data/clean/gardiner_hunt_poly_roads/gardiner_mtfwp_region.shp") %>% 
  # transforming lat/long to UTM to match the GPS points
  st_transform(crs = "+proj=utm +zone=12")

bison_hunt_poly <- st_read("data/clean/gardiner_hunt_poly_roads/gardiner_bison_region.shp") %>% 
  # transforming lat/long to UTM to match the GPS points
  st_transform(crs = "+proj=utm +zone=12")

hunt_dates <- readxl::read_xlsx("data/raw/hunting_seasons.xlsx") %>%
  dplyr::select(year, end) %>% 
  rename(hunt_end = end)

# cleaning rf kill data
source(here("scripts/clean_rf_data.R"))


wolf_kills <- kill_data_rf %>% 
  # simplify column names
  rename(dod = kill_start_date) %>% 
  # only relevant columns
  dplyr::select(dod, kill_end_date, easting, northing) %>% 
  # only complete cases
  filter(complete.cases(.)) %>% 
  # adding sf geometry so distance can be calculated
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12") %>% 
  # creating a column for if the kill was found by a raven
  mutate(used_nohunt = FALSE,
         used_hunt = FALSE) %>% 
  # add MTFWP hunting end dates
  mutate(winter_year = if_else(month(dod) %in% c(1:3), year(dod)-1, year(dod))) %>% 
  left_join(hunt_dates, by = join_by(winter_year == year)) %>% 
  dplyr::select(-winter_year) %>% 
  # calculating distance to both polygons
  mutate(., dist2fwp  = as.numeric(st_distance(., mtfwp_hunt_poly)),
         dist2bison = as.numeric(st_distance(., bison_hunt_poly))) %>% 
  # removing kills in the season appropriate hunting area
  mutate(active_dist = if_else(dod <= hunt_end, "fwp", "bison")) %>% 
  filter(if_else(active_dist == "fwp", dist2fwp != 0, dist2bison != 0)) %>% 
  dplyr::select(-active_dist)


# creating a single combined kill dataframe with the start date and coordinates
# wolf_kills <- wp_kills %>% 
#   # only useful columns
#   dplyr::select(dod, easting, northing) %>% 
#   # adding RF predictive kills
#   bind_rows(kill_data_rf %>% 
#               rename(dod = kill_start_date) %>% 
#               dplyr::select(dod, easting, northing)) %>% 
#   filter(complete.cases(.)) %>% 
#   st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12") %>% 
#   # creating a column for if the kill was found by a raven
#   mutate(used_nohunt = FALSE,
#          used_hunt = FALSE)


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
  # restricting time frame to winter study periods
  filter((month(date) > 11 | (month(date) == 11 & day(date) >= 15)) &
           (month(date) < 12 | (month(date) == 12 & day(date) <= 15)) |
           (month(date) > 3 | (month(date) == 3 & day(date) >= 1)) &
           (month(date) < 3 | (month(date) == 3 & day(date) <= 30))) %>% 
  # only complete rows
  filter(complete.cases(.)) %>% 
  # add crs
  st_drop_geometry() %>% 
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12") %>% 
  # adding column with distance to closest wolf kill
  mutate(dist2kill = NA)
  

# wolf kills are available for 1 week after dod
# loop comparing each GPS point to all wolf kills
# looping through each GPS point to see if there is an active kill that day
for(i in 1:nrow(leave_no_hunt_gps)){
  tmp_gps <- leave_no_hunt_gps[i,]
  
  # calculating time difference in days to start of carcass (0 = kill on that day) for all kills in territory
  time_diff_start <- difftime(as.Date(tmp_gps$date),
                              wolf_kills %>% 
                                pull(dod), 
                              units = "days") %>% as.numeric()
  
  # calculating time difference in days to end of carcass for all kills in territory
  # for RF predictive, this is the cluster end date
  time_diff_end <- difftime(as.Date(tmp_gps$date),
                            wolf_kills %>% 
                              pull(kill_end_date), 
                            units = "days") %>% as.numeric()
  
  tmp_kills <- wolf_kills %>% 
    # only kills that were wolves are present or were at least 1 day ago
    filter(time_diff_start >= 0 & time_diff_end <= 1)

  
  #if there are no kills, then make distance NA
  if(nrow(tmp_kills) > 0){
    # calculating distance to temporary wolf kills
    tmp_kills$kill_dist <- as.numeric(st_distance(tmp_gps, tmp_kills, units = "meters"))
    
    # pulling the distance for the closest wolf kill
    leave_no_hunt_gps[i, "dist2kill"] <- min(tmp_kills$kill_dist)
    
    # telling the wolf kill df if which kills were utilized 
    tmp_used <- tmp_kills %>% 
      filter(kill_dist < 500) %>% 
      pull(geometry)
    if(length(tmp_used) > 0){
      wolf_kills[wolf_kills$geometry %in% tmp_used,]$used_nohunt <- TRUE
    }
  } else(leave_no_hunt_gps[i, "dist2kill"] <- NA)
}

# how many days each raven has GPS points are within 500 meters
leave_no_hunt_wolf_kills <- leave_no_hunt_gps %>% 
  st_drop_geometry() %>% 
  # only GPS points within 500 m of kill
  filter(dist2kill < 500) %>% 
  # a single row for each raven/day
  group_by(raven_id, date) %>% 
  slice(1) %>% 
  # remove date grouping (only ID)
  ungroup(date) %>% 
  summarize(days_visit = n()) %>% 
  # adding the total number of days the raven left the territory by didn't visit hunting
  left_join(leave_no_hunt_gps %>% 
              st_drop_geometry %>% 
              group_by(raven_id, date) %>% 
              slice(1) %>% 
              ungroup(date) %>% 
              summarize(total_days = n())) %>% 
  # calculating proportion of days visiting wolf kill outside territory without visiting the hunting area
  mutate(prop_visit = days_visit/total_days)


# visiting the hunting area -------------------------------------------------------------------------

hunt_gps <- readr::read_csv(here("data/clean/raven_gps_covariates.csv")) %>% 
  # only days with visit to hunting area
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
  # restricting time frame to winter study periods
  filter((month(date) > 11 | (month(date) == 11 & day(date) >= 15)) &
           (month(date) < 12 | (month(date) == 12 & day(date) <= 15)) |
           (month(date) > 3 | (month(date) == 3 & day(date) >= 1)) &
           (month(date) < 3 | (month(date) == 3 & day(date) <= 30))) %>% 
  # only complete rows
  filter(complete.cases(.)) %>% 
  # add crs
  st_drop_geometry() %>% 
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12")


# adding column with distance to closest wolf kill
# wolf kills are available for 1 week after dod
hunt_gps$dist2kill <- NA
# loop comparing each GPS point to all wolf kills
for(i in 1:nrow(hunt_gps)){
  tmp_gps <- hunt_gps[i,]
  
  # calculating time difference in days to start of carcass (0 = kill on that day) for all kills in territory
  time_diff_start <- difftime(as.Date(tmp_gps$date),
                              wolf_kills %>% 
                                pull(dod), 
                              units = "days") %>% as.numeric()
  
  # calculating time difference in days to end of carcass for all kills in territory
  # for RF predictive, this is the cluster end date
  time_diff_end <- difftime(as.Date(tmp_gps$date),
                            wolf_kills %>% 
                              pull(kill_end_date), 
                            units = "days") %>% as.numeric()
  
  tmp_kills <- wolf_kills %>% 
    # only kills that were wolves are present or were at least 1 day ago
    filter(time_diff_start >= 0 & time_diff_end <= 1)
  
  #if there are no kills, then make distance NA
  if(nrow(tmp_kills) > 0){
    # calculating distance to temporary wolf kills
    tmp_kills$kill_dist <- as.numeric(st_distance(tmp_gps, tmp_kills, units = "meters"))
    
    # pulling the distance for the closest wolf kill
    hunt_gps[i, "dist2kill"] <- min(tmp_kills$kill_dist)
    
    # telling the wolf kill df if which kills were utilized 
    tmp_used <- tmp_kills %>% 
      filter(kill_dist < 500) %>% 
      pull(geometry)
    if(length(tmp_used) > 0){
      wolf_kills[wolf_kills$geometry %in% tmp_used,]$used_hunt <- TRUE
    }
  } else(hunt_gps[i, "dist2kill"] <- NA)
}


# how many days each raven has GPS points within 500 meters of a wolf kill
hunt_wolf_kills <- hunt_gps %>% 
  st_drop_geometry() %>% 
  # only GPS points within 500 m of kill
  filter(dist2kill < 500) %>% 
  # a single row for each raven/day
  group_by(raven_id, date) %>% 
  slice(1) %>% 
  # remove date grouping (only ID)
  ungroup(date) %>% 
  summarize(days_visit = n()) %>% 
  # adding the total number of days the raven left the territory by didn't visit hunting
  left_join(hunt_gps %>% 
              st_drop_geometry %>% 
              group_by(raven_id, date) %>% 
              slice(1) %>% 
              ungroup(date) %>% 
              summarize(total_days = n())) %>% 
  # calculating proportion of days visiting wolf kill outside territory without visiting the hunting area
  mutate(prop_visit = days_visit/total_days)


# combining for all days leaving territory --------------------------------

leave_terr_wolf_kills <- hunt_wolf_kills %>% 
  # combining tables
  bind_rows(leave_no_hunt_wolf_kills) %>% 
  # removing prop_visit column
  dplyr::select(-prop_visit) %>% 
  # adding days together
  group_by(raven_id) %>% 
  summarize(days_visit = sum(days_visit), total_days = sum(total_days)) %>% 
  # calculating proportion 
  mutate(prop_visit = days_visit/total_days) %>% 
  # adding days visiting carcass + hunting area
  left_join(hunt_wolf_kills %>% 
              # only relevant columns
              dplyr::select(raven_id, days_visit) %>% 
              # renaming to avoid conflict
              rename(hunt_days_visit = days_visit)) %>%
  # calculating percent of wolf kill visits also had hunt visits
  mutate(prop_hunt_visit = hunt_days_visit/days_visit)
  

# average days with visit to wolf kills when leaving territory
mean(leave_terr_wolf_kills$prop_visit, na.rm = T)
range(leave_terr_wolf_kills$prop_visit, na.rm = T)
sd(leave_terr_wolf_kills$prop_visit, na.rm = T)


# average days visiting wolf kills that also had hunt visits
mean(leave_terr_wolf_kills$prop_hunt_visit, na.rm = T)
range(leave_terr_wolf_kills$prop_hunt_visit, na.rm = T)
sd(leave_terr_wolf_kills$prop_hunt_visit, na.rm = T)

