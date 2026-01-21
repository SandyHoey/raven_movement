#creating data sets to make maps (probably in ArcGIS) about raven movements

library(dplyr)
library(readr)
library(janitor)
library(sf)
library(lubridate)
library(suncalc)

#calculating sunlight times for all GPS points because I'm to lazy to do it for ecah data set seperately
#especially because it reuqires lat/long instead of utm
sunlight <- read_csv("data/clean/all_raven_gps_clean29.csv") %>%
  clean_names() %>% 
  mutate(date = as.Date(study_local_timestamp)) %>% 
  rename(lat = location_lat,
         lon = location_long) %>% 
  getSunlightTimes(data = .,
                 keep = c("sunrise", "sunset"),
                 tz = "MST") %>% 
  #getting rid of duplicates
  group_by(date) %>% 
  slice(1) %>% 
  ungroup


# raven GPS in March and November ----------------------------------------------
# used to identify the hunting area used by ravens 

source("scripts/dist_to_gardiner.R")
rm(list = setdiff(ls(), c("terr_fw_gps", "sunlight")))

#reading in raven GPS data
terr_fw_gps <- terr_fw_gps %>% 
  #transforming to sf object
  st_as_sf(coords=c("utm_easting", "utm_northing"), 
           crs="+proj=utm +zone=12") %>% 
  #extracting date 
  mutate(date = as.Date(study_local_timestamp)) %>% 
  #only daytime points
  left_join(sunlight) %>% 
  mutate(study_local_timestamp = as.POSIXct(study_local_timestamp, tz = "MST")) %>% 
  filter(study_local_timestamp > sunrise,
         study_local_timestamp < sunset) %>% 
  #removing lat/long columns
  dplyr::select(-c(lat, lon))


#reading in Yellowstone polygon
park_poly <- st_read("data/raw/parkpoly.kml") %>% 
  st_transform(crs = st_crs(terr_fw_gps))


#only GPS points outside of YNP during November
raven_gps_outside_ynp <- terr_fw_gps %>% 
  #calculate distance to Yellowstone (0 == inside park)
  st_distance(park_poly) %>% 
  as.vector %>% 
  #adding back to gps data as a column
  bind_cols(terr_fw_gps %>% 
              #adding utm back
              mutate(utm_easting = st_coordinates(.)[,1],
                     utm_northing = st_coordinates(.)[,2]) %>% 
              #removing geometry
              st_drop_geometry) %>% 
  rename(distance_ynp = ...1) %>%
  #filtering to only distances > 0 (not inside the park)
  filter(distance_ynp > 0) %>% 
  #remove date column since it ArcGIS is the worst
  dplyr::select(-date)


#only November
raven_gps_outside_ynp %>% 
  filter(month(study_local_timestamp) == 11) %>% 
  write.csv("data/clean/nov_gps_outside_ynp.csv")


#only March
raven_gps_outside_ynp %>% 
  filter(month(study_local_timestamp) == 3) %>% 
  write.csv("data/clean/mar_gps_outside_ynp.csv")


# raven GPS to map based on model covariates -----------------

#reading in commute data
commute_df_covariates <- read_csv("data/clean/commute_data.csv") %>% 
  #selecting useful columns
  dplyr::select(raven_id, date, hunt_bin, dist2nentrance, temp_max)


#raven GPS data with covariates to map by
read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
  clean_names() %>% 
  #only useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, study_local_timestamp) %>%
  #extracting date
  mutate(date = as.Date(study_local_timestamp)) %>% 
  #adding commute data to GPS points
  left_join(commute_df_covariates, by = join_by(individual_local_identifier == raven_id, 
                                                date)) %>%
  #only daytime points
  left_join(sunlight) %>% 
  mutate(study_local_timestamp = as.POSIXct(study_local_timestamp, tz = "MST")) %>% 
  filter(study_local_timestamp > sunrise,
         study_local_timestamp < sunset) %>% 
  #only complete rows
  na.omit %>%
  #only winter points
  filter(month(date) %in% c(10:12, 1:3)) %>% 
  #remove date column since it ArcGIS is terrible
  dplyr::select(-date) %>% 
  #write out dataset
  write.csv("data/clean/raven_gps_covariates.csv")



# raven GPS to map days ravens left territory, but didn't visit hunting --------

#reading function that calculates distance of GPS points to territory
source("scripts/commute_decision.R")
rm(list = setdiff(ls(), c("mcp90", "gps_in_mcp", "sunlight")))


#reading in commute data
commute_df_intermediate <- read_csv("data/clean/commute_data.csv") %>% 
  #selecting useful columns
  dplyr::select(raven_id, date, terr_bin, hunt_bin) %>% 
  #filter movement decision for left territory, but didn't visit hunting
  filter(terr_bin == TRUE, hunt_bin == FALSE)


#raven movement data outside of territory
read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
  clean_names() %>% 
  #selecting useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, study_local_timestamp) %>%
  #only complete rows
  na.omit %>% 
  #calculating distance to territory
  gps_in_mcp() %>% 
  #only GPS outside of territory (> 1000 meters)
  filter(dist2terr > 1000) %>% 
  dplyr::select(-dist2terr) %>% 
  #extracting date
  mutate(date = as.Date(study_local_timestamp)) %>% 
  #adding commute decisions to each GPS point
  left_join(commute_df_intermediate %>% 
              dplyr::select(raven_id, date, terr_bin, hunt_bin), 
            by = join_by(individual_local_identifier == raven_id, 
                         date)) %>% 
  #only daytime points
  left_join(sunlight) %>%
  mutate(study_local_timestamp = as.POSIXct(study_local_timestamp, tz = "MST")) %>% 
  filter(study_local_timestamp > sunrise,
           study_local_timestamp < sunset) %>% 
  #only complete rows
  na.omit %>% 
  #remove date column since it ArcGIS is terrible
  dplyr::select(-date) %>% 
  #write out datatset
  write.csv("data/clean/raven_gps_outside_terr_no_hunt.csv")
  

# location of wolf kills visited by ravens when leaving their territory
source("scripts/wolf_kill_visits.R")

# kills on days ravens left territory, but didn't visit hunting
write.csv(wolf_kills %>%
            # removing sf geometry
            bind_cols(., st_coordinates(.)) %>% 
            rename(easting = X, northing = Y) %>% 
            st_drop_geometry() %>% 
            filter(used_nohunt == TRUE), 
          "data/clean/rf_used_kills_nohunt.csv",
          row.names = F)

# kills on days ravens left territory, and visited hunting
write.csv(wolf_kills %>% 
            # removing sf geometry
            bind_cols(., st_coordinates(.)) %>% 
            rename(easting = X, northing = Y) %>% 
            st_drop_geometry() %>% 
            filter(used_hunt == TRUE), 
          "data/clean/rf_used_kills_hunt.csv",
          row.names = F)
  
