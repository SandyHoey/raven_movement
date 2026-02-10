# janky post hoc investigation of how often raven had wolf kills on their territories that they visited
# visit = GPS point within 500 m

# NOT COMPLETE
# got to complicated trying to add kill coordinates to raven GPS points
# since there could be more than 1 carcass on a day

in_terr_kill_df <- rf_in_terr_kill_list %>% 
  # adding column with list name to each df
  purrr::imap(~ mutate(.x, raven_id = .y)) %>% 
  # list to df
  do.call("rbind", .)
  # only useful columns
  dplyr::select(raven_id, kill_number, easting, northing)


commute_df_covariates <- readr::read_csv("data/clean/commute_data.csv") %>% 
  # restricting to only winter study months
  filter((month > 11 | (month == 11 & day >= 15)) &
           (month < 12 | (month == 12 & day <= 15)) |
           (month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # removing days when there is less than 10 GPS point
  # unless the result is Jardine
  filter(!(n_point < 10 & terr_bin == F)) %>% 
  # only columns used in model
  dplyr::select(date, terr_bin, raven_id, rf_active_kill, rf_active_kill_3, final_take_bms, final_take_bms1, final_take, 
                hunt_season, rf_avg_terr_kill_density, dist2nentrance, 
                study_period, temp_max, snow_depth, prop_group_left_terr) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) %>% 
  # only columns useful for this
  dplyr::select(raven_id, date, rf_active_kill)

gps_raven <- readr::read_csv("data/clean/all_raven_gps_clean29.csv") %>% 
  janitor::clean_names() %>% 
  # only useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, study_local_timestamp) %>%
  # extracting date
  mutate(date = as.Date(study_local_timestamp)) %>% 
  # adding commute data to GPS points
  left_join(commute_df_covariates, by = join_by(individual_local_identifier == raven_id, 
                                                date)) %>% 
  # only days when wolf kill was available on terr
  filter(rf_active_kill == TRUE) %>% 
  # adding kill location
  left_join(in_terr_kill_df, by = join_by(individual_local_identifier = raven_id,
                                          date))