#creating data sets to make maps (probably in ArcGIS) about raven movements

library(dplyr)
library(readr)
library(janitor)
library(sf)
library(lubridate)

# raven movement in November ----------------------------------------------
# used to identify the hunting area used by ravens 

#reading in raven GPS data
raven_gps <- read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
  clean_names() %>% 
  #selecting useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, study_local_timestamp) %>%
  #only complete columns
  na.omit %>% 
  #transforming to sf object
  st_as_sf(coords=c("utm_easting", "utm_northing"), 
           crs="+proj=utm +zone=12")

#reading in Yellowstone polygon
park_poly <- st_read("data/raw/parkpoly.kml") %>% 
  st_transform(crs = st_crs(raven_gps))

#only GPS points outside of YNP during November
raven_gps_outside_ynp <- raven_gps %>% 
  #calculate distance to Yellowstone (0 == inside park)
  st_distance(park_poly) %>% 
  as.vector %>% 
  #adding back to gps data as a column
  bind_cols(raven_gps %>% st_drop_geometry) %>% 
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



