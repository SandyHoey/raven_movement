#creating data sets to make maps (probably in ArcGIS) about raven movements

library(dplyr)
library(readr)
library(janitor)
library(sf)
library(lubridate)

# raven movement in November ----------------------------------------------
# used to identify the hunting area used by ravens 

#reading in raven GPS data
raven_gps_sf <- read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
  clean_names() %>% 
  #selecting useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, study_local_timestamp) %>%
  #only complete rows
  na.omit %>% 
  #transforming to sf object
  st_as_sf(coords=c("utm_easting", "utm_northing"), 
           crs="+proj=utm +zone=12")

#reading in Yellowstone polygon
park_poly <- st_read("data/raw/parkpoly.kml") %>% 
  st_transform(crs = st_crs(raven_gps_sf))

#only GPS points outside of YNP during November
raven_gps_outside_ynp <- raven_gps_sf %>% 
  #calculate distance to Yellowstone (0 == inside park)
  st_distance(park_poly) %>% 
  as.vector %>% 
  #adding back to gps data as a column
  bind_cols(raven_gps_sf %>% st_drop_geometry) %>% 
  rename(distance_ynp = ...1) %>% 
  #filtering to only distances > 0 (not inside the park)
  filter(distance_ynp > 0)

#only November
raven_gps_outside_ynp %>% 
  filter(month(study_local_timestamp) == 11) %>% 
  write.csv("data/clean/nov_gps_outside_ynp.csv")


#only March
raven_gps_outside_ynp %>% 
  filter(month(study_local_timestamp) == 3) %>% 
  write.csv("data/clean/mar_gps_outside_ynp.csv")


# data sets to map based on model covariates -----------------

#reading in commute data
commute_df <- read_csv("data/clean/commute_data.csv") %>% 
  #selecting useful columns
  dplyr::select(raven_id, date, terr_bin, hunt_bin, dist2nentrance, temp_max)


#raven GPS data with covariates to map by
read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
  clean_names() %>% 
  #only useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, study_local_timestamp) %>%
  #extracting date
  mutate(date = as.Date(study_local_timestamp)) %>% 
  dplyr::select(-study_local_timestamp) %>% 
  #adding commute data to GPS points
  left_join(commute_df, by = join_by(individual_local_identifier == raven_id, date)) %>% 
  #only complete rows
  na.omit %>%
  #only winter points
  filter(month(date) %in% c(10:12, 1:3)) %>% 
  write.csv("data/clean/raven_gps_covariates.csv")

  
