# modeling the impacts of food availability on raven winter movements decisions

library(dplyr)
library(glmmTMB)

# model optimizer
cntrlTMB = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS"))


# model data --------------------------------------------------------------
## dataset for part 1 of conditional model
ws_model_data <- readr::read_csv("data/clean/commute_data.csv")  %>% 
  # limit to study years
  filter(date < "2024-3-31") %>% 
  # restricting to only winter study months
  filter((month > 11 | (month == 11 & day >= 15)) &
           (month < 12 | (month == 12 & day <= 15)) |
           (month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # only columns used in model
  dplyr::select(terr_bin, raven_id, rf_active_kill, visit_500, final_take_bms, final_take_bms1, 
                hunt_season, rf_avg_terr_kill_density, dist2nentrance, 
                study_period, temp_max, snow_depth, prop_group_left_terr) %>% 
  # scaling continuous variables
  mutate(final_take_bms1 = scale(final_take_bms1),
         rf_avg_terr_kill_density = scale(rf_avg_terr_kill_density),
         dist2nentrance = scale(dist2nentrance), temp_max = scale(temp_max),
         snow_depth = scale(snow_depth), 
         prop_group_left_terr = scale(prop_group_left_terr)) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 


## dataset for part 2 of conditional model
hunt_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  # limit to study years
  filter(date < "2024-3-31") %>%
  # restricting to only winter study months
  filter((month > 11 | (month == 11 & day >= 15)) &
           (month < 12 | (month == 12 & day <= 15)) |
           (month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # only have days ravens decided to leave territory
  filter(terr_bin == 1) %>% 
  # only columns used in model
  dplyr::select(hunt_bin, raven_id, visit_kill, final_take_bms, final_take_bms1, final_take, hunt_season,
                dist2nentrance, study_period, temp_max, snow_depth, prop_group_visit_hunt) %>% 
  # scaling continuous variables
  mutate(final_take_bms1 = scale(final_take_bms1), dist2nentrance = scale(dist2nentrance), 
         temp_max = scale(temp_max), snow_depth = scale(snow_depth), 
         prop_group_visit_hunt = scale(prop_group_visit_hunt)) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 


# PART 1 of decision (stay/leave territory) ---------------------------------------------
# did the raven choose to leaves its territory

## DEPENDENT VARIABLE ##
# terr_bin
# 1 = left territory
# 0 = stayed on territory


# model with wolf kill visits in terr
mod_terr <- glmmTMB(terr_bin ~ (1|raven_id) + visit_500 + rf_active_kill + final_take_bms1 + hunt_season + rf_avg_terr_kill_density + 
                      dist2nentrance + study_period + temp_max + snow_depth + prop_group_left_terr,
                    data = ws_model_data,
                    family = "binomial",
                    control = cntrlTMB)
summary(mod_terr)


# PART 2 of decision (visit recreational hunting or other) ----------------------
# model is conditional on ravens leaving their territory
# if the raven chose to leave its territory, did it visit the hunting area or not

## DEPENDENT VARIABLE ##
# hunt_bin
# 1 = visited hunting region
# 0 = visited other place


# model with all hunting covariates
mod_hunt <- glmmTMB(hunt_bin ~ (1|raven_id) + visit_kill + final_take_bms1 + hunt_season + 
                      dist2nentrance + temp_max + snow_depth + prop_group_visit_hunt,
                    data = hunt_model_data,
                    family = "binomial",
                    control = cntrlTMB)
summary(mod_hunt)
