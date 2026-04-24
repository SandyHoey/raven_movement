# cleaning the gps so that only days with >5 hours represented are kept

library(dplyr)
library(lubridate)
library(data.table)

# reading in uncleaned raven GPS data
all_gps <- readr::read_csv("data/raw/ravenGPS_movebank.csv") %>% 
  janitor::clean_names() %>% 
  # removing useless columns
  dplyr::select(-c(visible, bar_barometric_pressure, data_decoding_software,
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
                   gps_dop, gps_satellite_count, algorithm_marked_outlier)) %>% 
  # adding date column
  mutate(
    # fixing date time reading in as UTC
    study_local_timestamp = force_tz(study_local_timestamp, tz = "America/Denver"),
    #adding date column
    date = as.Date(study_local_timestamp, tz = "America/Denver")) %>% 
  # rmeoving missing GPS coordinates
  filter(!is.na(utm_easting))



# selecting GPS from only territorial ravens ------------------------------
# importing raven demographic information
# terr: territorial birds inside the park
# trans: birds that transitioned between breeder and nonbreeder during the study period
raven_id <- readxl::read_excel("data/raw/ravens_banding_tagging.xlsx", sheet = 1)
terr <- subset(raven_id, `status (reviewed 8/1/24)` == "territorial" & 
                 raven_id$`inside NationalPark` == "yes")$`tag-id`
trans <- subset(raven_id, `status (reviewed 8/1/24)` %like% "Trans")


# subsetting territorials from entire GPS df
terr_gps <- subset(all_gps, individual_local_identifier %in% terr)


# subsetting trans territorials into their active territorial periods from entire GPS df
trans_gps <- all_gps[all_gps$individual_local_identifier %in% trans$`tag-id`,]

trans_gps <- do.call("rbind", tapply(trans_gps, INDEX = trans_gps$individual_local_identifier, 
                                     FUN = function(x){
                                       ind <- trans[trans$`tag-id` == x[1,]$individual_local_identifier,]
                                       
                                       # has only an end date
                                       if(is.na(ind$`start date`) & !is.na(ind$`leave date`)){
                                         tmp <- x[as.Date(x$study_local_timestamp, tz = "America/Denver") < ym(ind$`leave date`),]
                                         return(tmp)
                                       }
                                       
                                       # has only a start date
                                       if(!is.na(ind$`start date`) & is.na(ind$`leave date`)){
                                         tmp <- x[as.Date(x$study_local_timestamp, tz = "America/Denver") > ym(ind$`start date`),]
                                         return(tmp)
                                       }
                                       
                                       
                                       # should be excluded
                                       if(is.na(ind$`start date`) & is.na(ind$`leave date`)){
                                         return()
                                       }
                                     }))


# combining trans and territorial datasets
terr_gps <- rbind(terr_gps, trans_gps)


# only relevant winter time frame
terr_fw_gps <- terr_gps %>% 
  filter(month(study_local_timestamp) %in% c(9, 10, 11, 12, 1, 2, 3)) %>% 
  
  
  # selecting only daylight hours -------------------------------------------

# assigning a sunrise/sunset time for each day
mutate(., sunrise = suncalc::getSunlightTimes(data = data.frame(date = as.Date(.$study_local_timestamp, tz = "America/Denver"),
                                                                lat = .$location_lat,
                                                                lon = .$location_long),
                                              keep = c("dawn"),
                                              tz = "America/Denver")[,c("dawn")],
       sunset = suncalc::getSunlightTimes(data = data.frame(date = as.Date(.$study_local_timestamp, tz = "America/Denver"),
                                                            lat = .$location_lat,
                                                            lon = .$location_long),
                                          keep = c("dusk"),
                                          tz = "America/Denver")[,c("dusk")]) %>% 
  filter(study_local_timestamp > sunrise, study_local_timestamp < sunset)


# remove days with at least 5 hours represented ----------------------------------
five_hour_gps <- terr_fw_gps %>% 
  # finding the number of hours represented each day
  group_by(individual_local_identifier, date) %>% 
  summarize(hours_represented = length(unique(hour(study_local_timestamp))),
            .groups = "keep") %>% 
  right_join(terr_fw_gps) %>% 
  # only keep GPS points from days with at least 5 hours represented
  filter(hours_represented >= 5)

days <- five_hour_gps %>% 
  group_by(individual_local_identifier, date) %>% 
  summarize(n_point = n())

ind <- days %>% 
  group_by(individual_local_identifier) %>% 
  summarize(average = mean(n_point),
            median = median(n_point),
            min = min(n_point),
            max = max(n_point))

write.csv(five_hour_gps, "data/clean/terr_raven_gps_5h.csv", row.names = F)
