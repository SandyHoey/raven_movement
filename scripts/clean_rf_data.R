# takes the random forest predicted wolf kills for the winter (Nov, Dec, Mar) and minimizes it to a usable format for calculating covariates
# the end result has a row for each predicted kill by individual, so kills themselves are duplicated when multiple individuals from a pack have GPS collars

library(dplyr)
library(sf)

# reading in RF input cluster data
# using this to find the end time for each kill
cluster_data_rf <- readr::read_rds("data/raw/cluster_summaries_wolves_winter.rds") %>% 
  janitor::clean_names() %>% 
  # only relevant columns
  dplyr::select(wolf_id, clus_start, clus_end, 
                centroid_easting, centroid_northing)


# reading in RF predicted wolf kills (only has winter study periods)
kill_data_rf <- readr::read_rds("data/raw/mergedkills_wolf_winter_RF_spec95.rds") %>% 
  janitor::clean_names() %>% 
  # removing t2 column
  dplyr::select(-t2) %>% 
  # renaming coordinate columns
  rename(easting = x,
         northing = y) %>% 
  left_join(cluster_data_rf, 
            by = join_by(easting == centroid_easting,
                         northing == centroid_northing,
                         wolf_id,
                         kill_start_date == clus_start)) %>%
  # rename datetime
  rename(kill_start_dt = kill_start_date,
         kill_end_dt = clus_end) %>% 
  # adding dedicated date column
  mutate(kill_start_date = as.Date(kill_start_dt),
         kill_end_date = as.Date(kill_end_dt)) %>% 
  # placing date columns together
  relocate(kill_start_dt, .before = kill_end_dt) %>% 
  # pulling a single row for each kill
  group_by(kill_id) %>% 
  slice(1) %>% 
  ungroup %>% 
  # creating sf geometry
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12") %>% 
  # creating an empty column for new combined kill ID numbers (see section below)
  mutate(comb_kill_id = NA)


# combining kills when they are close together (space/time)
for(i in 1:nrow(kill_data_rf)){
  # only perform these calculations on points that have not been assigned a new kill ID yet
  if(is.na(kill_data_rf[i,]$comb_kill_id)){
    # calculating distance between all points
    kill_data_rf$tmp_dist <- as.numeric(st_distance(kill_data_rf[i,], kill_data_rf, unit = "meters"))
    
    # calculating time between all points (based on start date)
    kill_data_rf$tmp_time <- abs(kill_data_rf[i,]$kill_start_date - kill_data_rf$kill_start_date)
    
    # assign a unique kill number to clusters within 200 m and within +- 1 day
    kill_data_rf[kill_data_rf$tmp_dist <= 200 & kill_data_rf$tmp_time <= 1,]$comb_kill_id <- i
    }
}


# a bit of cleanup
kill_data_rf <- kill_data_rf %>% 
  # removing columns made for this
  dplyr::select(-c(kill_id, tmp_dist, tmp_time)) %>% 
  # only getting 1 row for each kill
  group_by(comb_kill_id) %>% 
  slice(1) %>% 
  ungroup %>% 
  # renaming kill_id column
  rename(kill_id = comb_kill_id) %>% 
  # removing sf geometry
  bind_cols(., st_coordinates(.)) %>% 
  rename(easting = X, northing = Y) %>% 
  st_drop_geometry()

