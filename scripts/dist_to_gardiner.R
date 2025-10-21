#percentage of raven GPS points that occur in the Gardiner/Jardine
#area during the fall/winter season (Nov-Mar)

library(sf)
library(tidyverse)
library(data.table)


#reading in all raven points
#removing columns with NA coords
all_gps <- readr::read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
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
  filter(!is.na(utm_easting))



#transforming raven points to sf dataframe
#allows a distance metric to the polygon to be calculated
sf_ravens_all <- st_as_sf(all_gps, coords=c("utm_easting", "utm_northing"), 
                          crs="+proj=utm +zone=12")


#reading in Gardiner/Jardine kml
gardiner_kml <- st_read("data/raw/gardiner_polygon.kml") %>% 
  #transforming latlong to UTM to match the GPS points
  st_transform(gardiner_kml, crs = st_crs(sf_ravens_all))

#reading at gardiner dump/sewage pond kml
dump_kml <- st_read("data/raw/gardiner_dump.kml") %>% 
  #transforming latlong to UTM to match the GPS points
  st_transform(dump_kml, crs = st_crs(sf_ravens_all))


#calculating distance to (Jardine and dump)
#0 == inside the polygon
all_gps <- all_gps %>% 
  mutate(dist2gardiner  = as.numeric(st_distance(sf_ravens_all, gardiner_kml)),
         dist2dump = as.numeric(st_distance(sf_ravens_all, dump_kml)))

#importing raven demographic information
#terr: territorial birds inside the park
#trans: birds that transitioned between breeder and nonbreeder
#7485 (Old Faithful) currently not included because she transitioned more than once
raven_id <- readxl::read_excel("data/raw/ravens_banding_tagging.xlsx",sheet=1)
terr <- subset(raven_id, `status (reviewed 8/1/24)` == "territorial" & 
                 raven_id$`inside NationalPark` == "yes")$`tag-id`
trans <- subset(raven_id, `status (reviewed 8/1/24)` %like% "Trans")


#subsetting territorials from entire GPS df
terr_gps <- subset(all_gps, individual_local_identifier %in% terr)


#subsetting trans territorials into their active territorial periods from entire GPS df
trans_gps <- all_gps[all_gps$individual_local_identifier %in% trans$`tag-id`,]

trans_gps <- do.call("rbind", tapply(trans_gps, INDEX = trans_gps$individual_local_identifier, 
                        FUN = function(x){
                          ind <- trans[trans$`tag-id` == x[1,]$individual_local_identifier,]
                          
                          #has only an end date
                          if(is.na(ind$`start date`) & !is.na(ind$`leave date`)){
                            tmp <- x[as.Date(x$study_local_timestamp) < ym(ind$`leave date`),]
                            return(tmp)
                          }
                          
                          #has only a start date
                          if(!is.na(ind$`start date`) & is.na(ind$`leave date`)){
                            tmp <- x[as.Date(x$study_local_timestamp) > ym(ind$`start date`),]
                            return(tmp)
                          }
                          
                          
                          #should be excluded
                          if(is.na(ind$`start date`) & is.na(ind$`leave date`)){
                          return()
                          }
                        }))

        
#combining trans and territorial datasets
#!!!SKIP this step if you want to exclude trans 
terr_gps <- rbind(terr_gps, trans_gps)



#subsetting the GPS points to only fall/winter (Nov-Dec)
terr_fw_gps <- terr_gps %>% 
  filter(month(terr_gps$study_local_timestamp) %in% c(10, 11, 12, 1, 2, 3))
        
        

# basic summary of movement decisions -------------------------------------

# calculating the percent of GPS points that are inside the Gardiner/Jardine polygon
# for territorial birds

# terr_fw_gardiner <- tapply(terr_fw_gps,
#                          terr_fw_gps$individual_local_identifier,
#                          function(x){
#                            nrow(x[x$dist2gardiner == 0,])/nrow(x)
# })
# terr_fw_gardiner
# range(terr_fw_gardiner)
# hist(terr_fw_gardiner)