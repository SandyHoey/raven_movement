# janky post hoc investigation of how often raven had wolf kills on their territories that they visited
# visit = GPS point within 500 m

library(dplyr)

source("scripts/home_range_mcp.R")

kill_in_terr <- function(data, dist_from_terr, coords, crs, start){
  ID <- mcp90$id
  
  # creating a list to put the kill information for kills inside each territory
  in_terr_kill_list <- vector("list", length(ID))
  names(in_terr_kill_list) <- ID
  
  # changing kill data into a format that can be used for the distance measurement
  tmp_sf <- st_as_sf(data, coords = coords,
                     crs = crs)
  
  for(i in 1:length(ID)){
    
    # calculating distance
    tmp_dist <- as.numeric(st_distance(tmp_sf, st_as_sf(mcp90[i,])))
    
    # putting all rows with distance == 0 into the list
    # and ordering by date
    in_terr_kill_list[[i]] <- subset(data, tmp_dist <= dist_from_terr) %>%
      arrange(start)
    
  }
  return(in_terr_kill_list)
}

# reading in RF data
source("scripts/clean_rf_data.R")
in_terr_kill_df <- kill_in_terr(kill_data_rf, dist_from_terr = 1000,
                                coords = c("easting", "northing"), crs = "+proj=utm +zone=12",
                                start = "kill_start_date") %>% 
  # adding column with list name to each df
  purrr::imap(~ mutate(.x, raven_id = .y)) %>% 
  # list to df
  do.call("rbind", .) %>% 
  # making the kill end dates match the availability
  mutate(kill_end_date = kill_end_date + 1)


commute_df_covariates <- readr::read_csv("data/clean/commute_data.csv") %>% 
  # restricting to only winter study months
  filter((month > 11 | (month == 11 & day >= 15)) &
           (month < 12 | (month == 12 & day <= 15)) |
           (month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # removing days when there is less than 10 GPS point
  filter(n_point >= 10) %>% 
  # only columns used in model
  dplyr::select(date, terr_bin, raven_id, rf_active_kill, rf_active_kill_3, final_take_bms, final_take_bms1, final_take, 
                hunt_season, rf_avg_terr_kill_density, dist2nentrance, 
                study_period, temp_max, snow_depth, prop_group_left_terr) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) %>% 
  # only columns useful for this
  dplyr::select(raven_id, date, rf_active_kill, terr_bin)


# adding information about available kills to raven GPS points
gps_raven <- readr::read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
  janitor::clean_names() %>% 
  # only useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, study_local_timestamp) %>%
  # extracting date
  mutate(date = as.Date(study_local_timestamp)) %>% 
  # adding commute data to GPS points
  left_join(commute_df_covariates, by = join_by(individual_local_identifier == raven_id, 
                                                date)) %>% 
  # adding raven territory size
  left_join(mcp90@data, by = join_by(individual_local_identifier == id)) %>% 
  # only days when wolf kill was available on terr
  filter(rf_active_kill == TRUE) %>% 
  # making column for date
  mutate(date = as.Date(study_local_timestamp))


# IGNORE WARNING SINCE THERE ARE MULTIPLE GPS POINTS PER DAY
# joining kill location with raven GPS points
kill_rows <- in_terr_kill_df %>%
  inner_join(gps_raven, by = c("raven_id" = "individual_local_identifier")) %>%
  # only keeping GPS points with active kill information
  filter(date >= kill_start_date, date <= kill_end_date) %>%
  # calculating distance to active kills
  mutate(dist_m = sqrt((utm_easting  - easting)^2 +
                         (utm_northing - northing)^2)) %>%
  # only keeping the closest GPS point per day
  group_by(kill_id, date) %>% 
  filter(dist_m == min(dist_m)) %>% 
  ungroup %>% 
  # creating column with day since kill
  mutate(delta_dod = as.numeric(date - kill_start_date)) %>% 
  # creating column to define a visited kill
  mutate(visit_500 = if_else(dist_m < 500, TRUE, FALSE),
         visit_1000 = if_else(dist_m < 1000, TRUE, FALSE)) %>% 
  # keeping only relevant columns
  dplyr::select(kill_id, raven_id, visit_500, visit_1000, terr_bin, date, delta_dod, area, easting, northing) %>% 
  arrange(raven_id, kill_id, date)

# kill visits during days kills were available
kill_rows %>% 
  summarize(prop_visit_available_days = sum(visit_500)/n())

# kill visits on at least 1 day the kill was available
kill_rows %>% 
  group_by(kill_id) %>% 
  mutate(visit_available_500 = if_else(any(visit_500 == TRUE), TRUE, FALSE)) %>% 
  slice(1) %>% 
  ungroup %>% 
  summarize(prop_visit_kill_duration = sum(visit_available_500)/n())
  

# doing some testing on probabilities of visiting a kill ------------------
# library(lme4)
# 
# # based on size of territory
# terr_size_500 <- glm(visit_500 ~ area, 
#                      data = kill_rows, 
#                      family = "binomial")
# summary(terr_size_500)
# 
# terr_size_1000 <- glm(visit_1000 ~ area, 
#                       data = kill_rows, 
#                       family = "binomial")
# summary(terr_size_1000)



# # based on days since the kill
# delta_dod_500 <- glm(visit_500 ~ delta_dod, 
#                      data = kill_rows, 
#                      family = "binomial")
# summary(delta_dod_500)
# 
# delta_dod_1000 <- glm(visit_1000 ~ delta_dod, 
#                       data = kill_rows, 
#                       family = "binomial")
# summary(delta_dod_1000)
