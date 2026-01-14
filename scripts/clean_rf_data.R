# takes the random forest predicted wolf kills for the winter (Nov, Dec, Mar) and minimizes it to a usable format for calculating covariates

library(dplyr)

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
  ungroup




# for loop that pulls the end date for each cluster
  # I need to do this as a for loop because the RF output only matches with a single row in the cluster data
  # but there can potentially be multiple rows in the cluster data associated with that cluster (because of multiple GPS collars)
  #' step 1: find which GPS cluster the kill is associated with
  #' step 2: find all of the wolves associated with the GPS cluster
  #' step 3: find the latest datetime for that cluster
  #' step 4: add the end datetime to the RF data

