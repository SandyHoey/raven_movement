# creating data sets to make maps (probably in ArcGIS) about raven movements

library(dplyr)
library(readr)
library(janitor)
library(sf)
library(lubridate)
library(suncalc)


# reading function that calculates distance of GPS points to territory
source("scripts/commute_decision.R")


# reading in commute data
commute_df_intermediate <- read_csv("data/clean/commute_data.csv") %>% 
  # only days with > 10 GPS points
  filter(n_point >= 10) %>% 
  # selecting useful columns
  dplyr::select(raven_id, date, terr_bin, hunt_bin, hunt_season) %>% 
  # filter movement decision for left territory, but didn't visit hunting
  filter(terr_bin == TRUE, hunt_bin == FALSE)


# raven GPS to map days ravens left territory, but didn't visit hunting --------

# raven movement data outside of territory
read_csv("data/clean/terr_raven_gps_5h.csv", 
         locale = locale(tz = "America/Denver")) %>% 
  # selecting useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, date) %>%
  # only complete rows
  na.omit %>% 
  # calculating distance to territory
  gps_in_mcp() %>% 
  # only GPS outside of territory (> 1000 meters)
  filter(dist2terr > 1000) %>% 
  dplyr::select(-dist2terr) %>% 
  # adding commute decisions to each GPS point
  left_join(commute_df_intermediate %>% 
              dplyr::select(raven_id, date, terr_bin, hunt_bin), 
            by = join_by(individual_local_identifier == raven_id, 
                         date)) %>% 
  # only complete rows
  na.omit %>% 
  # remove date column since it ArcGIS is terrible
  dplyr::select(-date) %>%  
  # write out datatset
  write.csv("data/clean/raven_gps_outside_terr_no_hunt.csv", row.names = F)


# location of wolf kills visited by ravens when leaving their territory
source("scripts/wolf_kill_visits.R")

# kills on days ravens left territory, but didn't visit hunting
write.csv(wolf_kills %>%
            # removing sf geometry
            bind_cols(., st_coordinates(.)) %>% 
            rename(easting = X, northing = Y) %>% 
            st_drop_geometry() %>% 
            filter(used_nohunt == TRUE), 
          "data/clean/rf_used_kills_nohunt.csv",
          row.names = F)

# kills on days ravens left territory, and visited hunting
write.csv(wolf_kills %>% 
            # removing sf geometry
            bind_cols(., st_coordinates(.)) %>% 
            rename(easting = X, northing = Y) %>% 
            st_drop_geometry() %>% 
            filter(used_hunt == TRUE), 
          "data/clean/rf_used_kills_hunt.csv",
          row.names = F)


# splitting winter by hunting season --------------------------------------
# reading in hunting season dates
hunting_dates <- readxl::read_xlsx("data/raw/hunting_seasons.xlsx")

# raven movement data outside of territory
read_csv("data/clean/terr_raven_gps_5h.csv", 
         locale = locale(tz = "America/Denver")) %>% 
  janitor::clean_names() %>% 
  # selecting useful columns
  dplyr::select(individual_local_identifier, utm_easting, utm_northing, study_local_timestamp) %>%
  # adding winter year
  mutate(winter_year = if_else(month(study_local_timestamp) %in% c(9:12), year(study_local_timestamp), year(study_local_timestamp) - 1),
         date = as.Date(study_local_timestamp, tz = "MST")) %>% 
  # filter to only Sep - March
  filter(month(date) %in% c(9:12, 1:3))%>% 
  # only complete rows
  na.omit %>% 
  # adding hunting dates
  left_join(hunting_dates, by = join_by(winter_year == year)) %>% 
  # creating bins of time based on hunting periods
  mutate(hseason_blocks = factor(if_else(date < start, "Pre-hunt", # before MTFWP season
                                         if_else(date <= end, "MTFWP hunt", # MTFWP hunting season
                                                 if_else(date < bison, "Mid-winter", # between MTFWP and bison season
                                                         "Bison hunt"))), # bison season
                                 levels = c("Pre-hunt", "MTFWP hunt", "Mid-winter", "Bison hunt"))) %>% 
  filter(
    # only complete rows
    complete.cases(.),
    # only winter years used
    winter_year <= 2023) %>% 
  # adding commute decisions so data is restricted to birds/days that I used
  left_join(commute_df %>% 
              filter(n_point >= 10), by = join_by(individual_local_identifier, date)) %>% 
  filter(complete.cases(.),
         # removing paired females
         !(individual_local_identifier %in% c("7654", "7489_2"))) %>% 
  # remove date column since it ArcGIS is terrible
  dplyr::select(-date) %>% 
  # write out datatset
  write.csv("data/clean/raven_gps_hseason_divide.csv", row.names = F)


# writing out wolf kill locations
# wolf kill locations (rf) ------------------------------------------------
source("scripts/clean_rf_data.R")

test <- kill_data_rf %>% 
  # filtering to study period
  mutate(year = lubridate::year(kill_start_date),
         month = lubridate::month(kill_start_date),
         day = lubridate::day(kill_start_date)) %>% 
  filter(
    # only winter study
    (month > 11 | (month == 11 & day >= 15)) &
      (month < 12 | (month == 12 & day <= 15)) |
      (month > 3 | (month == 3 & day >= 1)) &
      (month < 3 | (month == 3 & day <= 30)),
    # setting broad study period
    lubridate::floor_date(kill_start_date, "month") >= as.Date("2019-10-01"),
    lubridate::ceiling_date(kill_start_date, "month") <= as.Date("2024-03-30")) %>% 
  dplyr::select(easting, northing) %>% 
write.csv("data/clean/rf_kills_coords.csv")
