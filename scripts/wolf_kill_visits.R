# finding wolf kills visited outside of territory for each raven
# clean version that doesn't require separate output from the mapping datasets

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

# main table that holds by-kill information
wolf_kills <- kill_data_rf %>% 
  # simplify column names
  rename(dod = kill_start_date) %>% 
  # only relevant columns
  dplyr::select(dod, kill_end_date, easting, northing) %>% 
  # only complete cases
  filter(complete.cases(.)) %>% 
  # adding sf geometry so distance can be calculated
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12") %>% 
  # creating a column for if the kill was found by a raven (preset to FALSE)
  mutate(used = FALSE) %>% 
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


# reducing all raven GPS points to only extra-territorial (terr_bin = 1) --------

raven_gps_oot <- read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
  janitor::clean_names() %>% 
  # selecting useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, study_local_timestamp) %>%
  # only complete rows
  na.omit %>% 
  # calculating distance to territory
  gps_in_mcp() %>% 
  # only GPS outside of territory (> 1000 meters)
  filter(dist2terr > 1000) %>% 
  # extracting date
  mutate(date = as.Date(study_local_timestamp)) %>% 
  # filter time frame to winter study periods
  filter((month(date) > 11 | (month(date) == 11 & day(date) >= 15)) &
           (month(date) < 12 | (month(date) == 12 & day(date) <= 15)) |
           (month(date) > 3 | (month(date) == 3 & day(date) >= 1)) &
           (month(date) < 3 | (month(date) == 3 & day(date) <= 30))) %>% 
  # filter to study years
  filter(date <= "2024-3-31") %>% 
  # adding commute decisions to each GPS point
  left_join(commute_df %>% 
              dplyr::select(raven_id, date, terr_bin), 
            by = join_by(individual_local_identifier == raven_id, 
                         date)) %>% 
  # only daytime points
  # calculating sunlight times 
  left_join(readr::read_csv("data/clean/all_raven_gps_clean29.csv") %>%
              janitor::clean_names() %>% 
              mutate(date = as.Date(study_local_timestamp)) %>% 
              rename(lat = location_lat,
                     lon = location_long) %>% 
              suncalc::getSunlightTimes(data = .,
                                        keep = c("sunrise", "sunset"),
                                        tz = "MST") %>% 
              # getting rid of duplicates
              group_by(date) %>% 
              slice(1) %>% 
              ungroup) %>%
  mutate(study_local_timestamp = as.POSIXct(study_local_timestamp, tz = "MST")) %>% 
  filter(study_local_timestamp > sunrise,
         study_local_timestamp < sunset) %>% 
  # turning into sf object to calculate distance from kills
  st_as_sf(coords = c("utm_easting", "utm_northing"), 
           crs = "+proj=utm +zone=12", 
           remove = FALSE) %>% 
  # only complete rows (removes GPS points from days that aren't included)
  na.omit %>% 
  # removing extra columns
  dplyr::select(-c(dist2terr, terr_bin, study_local_timestamp, sunrise, sunset, lat, lon)) %>% 
  #creating column for distance to the nearest wolf kills
  mutate(dist2kill = NA)


# calculating distance to wolf kills --------------------------------------
# adding column with distance to closest wolf kill
# wolf kills are available for 1 day after wolves leave

# loop comparing each GPS point to all wolf kills
for(i in 1:nrow(raven_gps_oot)){
  tmp_gps <- raven_gps_oot[i,]
  
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
    raven_gps_oot[i, "dist2kill"] <- min(tmp_kills$kill_dist)
    
    # telling the wolf kill df if which kills were utilized 
    tmp_used <- tmp_kills %>% 
      filter(kill_dist < 500) %>% 
      pull(geometry)
    if(length(tmp_used) > 0){
      wolf_kills[wolf_kills$geometry %in% tmp_used,]$used <- TRUE
    }
  } else(raven_gps_oot[i, "dist2kill"] <- NA)
}
