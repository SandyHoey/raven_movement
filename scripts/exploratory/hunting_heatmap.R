#creating a heatmap of the GPS points for all ravens during the winter
#this is to better define the hunting area used by ravens

library(dplyr)
library(lubridate)
library(sf)

#reading in GPS data
raven_all_gps <- readr::read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
  janitor::clean_names() %>% 
  
  #removing useless columns
  dplyr::select(-c(x1, visible, bar_barometric_pressure, data_decoding_software,
                   eobs_activity, eobs_activity_samples, eobs_key_bin_checksum,
                   eobs_speed_accuracy_estimate, eobs_start_timestamp, eobs_status,
                   eobs_temperature, eobs_type_of_fix, eobs_used_time_to_get_fix,
                   ground_speed, heading, height_above_ellipsoid, height_above_msl,
                   height_raw, import_marked_outlier, mag_magnetic_field_raw_x,
                   mag_magnetic_field_raw_y, mag_magnetic_field_raw_z,
                   manually_marked_outlier, orientation_quaternion_raw_w,
                   orientation_quaternion_raw_x, orientation_quaternion_raw_y,
                   orientation_quaternion_raw_z, sensor_type, individual_taxon_canonical_name,
                   tag_local_identifier, study_timezone, study_name, utm_zone, timestamp,
                   eobs_battery_voltage, eobs_fix_battery_voltage, eobs_horizontal_accuracy_estimate,
                   gps_dop, gps_satellite_count)) %>% 
  
  #make sure all rows have coordinates
  filter(!is.na(utm_easting)) %>% 
  
  #transforming to sf object
  st_as_sf(coords=c("utm_easting", "utm_northing"), 
           crs="+proj=utm +zone=12")


# getting only GPS points outside YNP -------------------------------------

#reading in park polygon
park_poly <- st_read("data/raw/parkpoly.kml") %>% 
  st_transform(crs = st_crs(raven_all_gps))


#dataframe with only GPS points outside YNP
raven_outside_ynp <- raven_all_gps %>% 
  
  #calculating distance to YNP
  st_distance(park_poly, raven_all_gps) %>% 
  as.vector %>% 
  
  #adding back to elk data as a column
  bind_cols(raven_all_gps %>% st_drop_geometry) %>% 
  rename(distance_ynp = ...1) %>% 
  
  #filtering to only distances > 0 (not inside the park)
  filter(distance_ynp > 0)


#filtering to "hunting months"
#november gps
nov_gps <- raven_outside_ynp %>% 
  filter(month(study_local_timestamp) == 11)
# write.csv(nov_gps %>% dplyr::select(location_long, location_lat),
#           "data/clean/nov_gps_outside_ynp.csv", row.names = F)

#march gps
mar_gps <- raven_outside_ynp %>% 
  filter(month(study_local_timestamp) == 3)
# write.csv(mar_gps %>% dplyr::select(location_long, location_lat),
#           "data/clean/mar_gps_outside_ynp.csv", row.names = F)


# creating heatmap --------------------------------------------------------
library(ggplot2)
library(RColorBrewer)

nov_gps %>% 
  
  #defining data and axis
  ggplot(aes(x = location_long, y = location_lat)) +
  
  #creating heatmap
  stat_density2d(aes(fill = after_stat(level)), alpha = 0.5, geom = "polygon") +
  
  #heatmap colors
  scale_fill_gradientn(colours = rev(brewer.pal(7,"Spectral"))) +
  coord_fixed()
