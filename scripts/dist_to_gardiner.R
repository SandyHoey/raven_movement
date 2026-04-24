# calculating the distance of GPS points to the Gardiner hunting region polygon
# calculating percentage of raven GPS points that occur in the Gardiner hunting district
  # area during the fall/winter season (Nov-Mar)

library(sf)
library(tidyverse)
library(data.table)


# reading in all raven points
five_hour_gps <- readr::read_csv("data/clean/terr_raven_gps_5h.csv", 
                                 locale = locale(tz = "America/Denver"))


# transforming raven points to sf dataframe
# allows a distance metric to the polygon to be calculated
sf_ravens_all <- st_as_sf(five_hour_gps, coords=c("utm_easting", "utm_northing"), 
                          crs="+proj=utm +zone=12")


# reading in Gardiner hunting region shapefile
mtfwp_hunt_poly <- st_read("data/clean/gardiner_hunt_poly_roads/gardiner_mtfwp_region.shp") %>% 
  # transforming latlong to UTM to match the GPS points
  st_transform(crs = st_crs(sf_ravens_all))

bison_hunt_poly <- st_read("data/clean/gardiner_hunt_poly_roads/gardiner_bison_region.shp") %>% 
  # transforming latlong to UTM to match the GPS points
  st_transform(crs = st_crs(sf_ravens_all))


# reading at Gardiner dump/sewage pond kml
dump_kml <- st_read("data/raw/Gardiner_dump.kml") %>% 
  # transforming latlong to UTM to match the GPS points
  st_transform(dump_kml, crs = st_crs(sf_ravens_all))


# calculating distance to MTFWP hunting region, bison hunting region, Gardiner dump
# 0 == inside the polygon
five_hour_gps <- five_hour_gps %>% 
  mutate(dist2fwp  = as.numeric(st_distance(sf_ravens_all, mtfwp_hunt_poly)),
         dist2bison = as.numeric(st_distance(sf_ravens_all, bison_hunt_poly)),
         dist2dump = as.numeric(st_distance(sf_ravens_all, dump_kml)))
