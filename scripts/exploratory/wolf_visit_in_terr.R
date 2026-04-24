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
gps_raven <- readr::read_csv("data/clean/terr_raven_gps_5h.csv", 
                             locale = locale(tz = "America/Denver")) %>% 
  # only useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, date) %>%
  # adding commute data to GPS points
  left_join(commute_df_covariates, by = join_by(individual_local_identifier == raven_id, 
                                                date)) %>% 
  # adding raven territory size
  left_join(mcp90@data, by = join_by(individual_local_identifier == id)) %>% 
  # only days when wolf kill was available on terr
  filter(rf_active_kill == TRUE)


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
  # adding column for kills visited at any time
  group_by(kill_id) %>% 
  mutate(visit = if_else(any(visit_500), TRUE, FALSE)) %>% 
  # keeping only relevant columns
  dplyr::select(kill_id, raven_id, visit_500, visit_1000, visit, terr_bin, date, delta_dod, area, easting, northing, kill_start_dt) %>% 
  arrange(raven_id, kill_id, date) %>% 
  # removing duplicate columns
  group_by(kill_id, delta_dod) %>% 
  slice(1) %>% ungroup

# kill visits during days kills were available
kill_rows %>% 
  summarize(prop_visit_available_days = sum(visit_500)/n())


# kill visits on at least 1 day the kill was available
kill_rows %>% 
  group_by(kill_id) %>% 
  slice(1) %>% 
  ungroup %>% 
  summarize(prop_visit_kill_duration = sum(visit)/n())
  


# kill attributes ---------------------------------------------------------

kill_db <- readr::read_csv("data/raw/wolf_project_carcass_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = mdy(dod)) %>%
  mutate(easting = case_when(!is.na(ground_east) ~ ground_east,
                             !is.na(aerial_east) ~ aerial_east,
                             !is.na(est_ground_east) ~ est_ground_east),
         northing = case_when(!is.na(ground_north) ~ ground_north,
                              !is.na(aerial_north) ~ aerial_north,
                              !is.na(est_ground_north) ~ est_ground_north)) %>%
  filter(!is.na(easting))


for(i in 1:length(unique(kill_rows$kill_id))){
  # kill date 
  tmp_kill <- kill_rows %>% 
    group_by(kill_id) %>% 
    slice(1) %>% 
    ungroup %>% 
    slice(i)
  
  # getting all kills within 2 days
  kill_possible <- kill_db %>% 
    mutate(time_diff = difftime(date, tmp_kill$date)) %>% 
    filter(time_diff >= 0, time_diff <= 2) %>% 
  # getting kills within 1 km
    mutate(dist_m = sqrt((easting  - tmp_kill$easting)^2 +
                           (northing - tmp_kill$northing)^2)) %>% 
    filter(dist_m <= 2500)
  
  kill_rows[kill_rows$kill_id == tmp_kill$kill_id, "potential"] <- nrow(kill_possible)
  kill_rows[kill_rows$kill_id == tmp_kill$kill_id, "prey_species"] <- kill_possible[1, "species"]
  kill_rows[kill_rows$kill_id == tmp_kill$kill_id, "prey_sex"] <- kill_possible[1, "sex"]
  kill_rows[kill_rows$kill_id == tmp_kill$kill_id, "prey_age"] <- kill_possible[1, "age_class"]
}


# number of kills visited
kill_rows %>% 
  filter(visit == TRUE) %>% 
  pull(kill_id) %>% 
  unique() %>% 
  length


# duration of availability
kill_rows %>% 
  group_by(kill_id) %>% 
  filter(delta_dod == max(delta_dod)) %>% 
  ungroup %>% 
  summarize(mean = mean(delta_dod + 1),
            min = min(delta_dod + 1),
            max = max(delta_dod + 1),
            sd = sd(delta_dod + 1))


# prey species
# visited
kill_rows %>% 
  filter(!is.na(prey_species),
         visit == TRUE) %>% 
  mutate(prey = paste(prey_species, prey_sex, prey_age)) %>% 
  group_by(kill_id) %>% 
  slice(1) %>% 
  pull(prey) %>% 
  table
# not visited
kill_rows %>% 
  filter(!is.na(prey_species),
         visit == FALSE) %>% 
  mutate(prey = paste(prey_species, prey_sex, prey_age)) %>% 
  group_by(kill_id) %>% 
  slice(1) %>% 
  pull(prey) %>% 
  table

  
# timing of kills visited on first day
# visited
kill_rows %>% 
  group_by(kill_id) %>% 
  slice(1) %>% 
  filter(delta_dod == 0,
         visit_500 == TRUE) %>% 
  pull(kill_start_dt) %>%
  hour() %>% 
  sort()
# not visited
kill_rows %>% 
  group_by(kill_id) %>% 
  slice(1) %>% 
  filter(delta_dod == 0,
         visit_500 == FALSE) %>% 
  pull(kill_start_dt) %>%
  hour() %>% 
  sort


# probability of visits on all days
kill_rows %>% 
  group_by(delta_dod) %>% 
  summarize(prob = sum(visit_500)/n(),
            days_visited = sum(visit_500),
            n_days = n())


# day of first visit
kill_rows %>% 
  filter(visit_500 == TRUE) %>% 
  group_by(kill_id) %>% 
  filter(delta_dod == min(delta_dod)) %>% 
  pull(delta_dod) %>% 
  table


# doing some testing on probabilities of visiting a kill ------------------
# library(lme4)
# 
# # what is the day of discovery across territory size
# terr_size_discovery <- kill_rows %>% 
#   group_by(kill_id) %>%
#   filter(visit_500 == TRUE) %>% 
#   slice(1) %>% 
#   ungroup %>% 
#   glm(delta_dod ~ scale(area),
#       data = .,
#       family = "poisson")
# summary(terr_size_discovery)
#   
# 
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
# 
# 
# 
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
