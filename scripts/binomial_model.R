# modeling the impacts of food availability on raven winter movements decisions

library(dplyr)

## dataset for part 1 of conditional model
ws_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  # restricting to only winter study months
  filter((paste(month, day, sep = "-") >= "11-15" &
            paste(month, day, sep = "-") <= "12-15") |
          (paste(month, day, sep = "-") >= "3-1" &
            paste(month, day, sep = "-") <= "3-30")) %>% 
  # removing days when there is less than 5 GPS point
  # unless the result is Jardine
  filter(!(n_point < 5 & terr_bin == F)) %>% 
  # only columns used in model
  dplyr::select(terr_bin, raven_id, rf_active_kill, rf_active_kill_3, final_take_bms, final_take_bms1, final_take, 
                hunt_season, rf_avg_terr_kill_density, dist2nentrance, 
                study_period, temp_max, snow_depth, prop_group_left_terr) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 
  

## dataset for part 2 of conditional model
hunt_model_data <- readr::read_csv("data/clean/commute_data.csv") %>%
  # restricting to only winter study months
  filter((paste(month, day, sep = "-") >= "11-15" &
            paste(month, day, sep = "-") <= "12-15") |
           (paste(month, day, sep = "-") >= "3-1" &
              paste(month, day, sep = "-") <= "3-30")) %>% 
  # only have days ravens decided to leave territory
  filter(terr_bin == 1) %>% 
  # removing days when there is less than 5 GPS point
  # unless the result is Jardine
  filter(!(n_point < 5 & hunt_bin == F)) %>% 
  # only columns used in model
  dplyr::select(hunt_bin, raven_id, visit_kill, final_take_bms, final_take, hunt_season,
                dist2nentrance, study_period, temp_max, snow_depth, prop_group_visit_hunt) %>% 
  
  # making sure rows are complete
  filter(complete.cases(.)) 


# checking correlation between biomass covariates --------------------
# cor.test(hunt_model_data$final_take_bms, hunt_model_data$bms_window_3)
# cor.test(hunt_model_data$final_take_bms, hunt_model_data$bms_window_5)
# cor.test(hunt_model_data$bms_window_5, hunt_model_data$bms_window_3)
# #moving averages are basically the same
# 
# #non-average options are still very similar
# cor.test(hunt_model_data$final_take_bms, hunt_model_data$final_take_bms)

# model setup -------------------------------------------------------------
library(lme4)
library(DHARMa)
library(ggplot2)
library(myFunctions) #custom bootstrap function

# optimizer for glmer
cntrl <- glmerControl(optimizer = "bobyqa", tol = 1e-4, optCtrl=list(maxfun=100000))
 

# part 1 of conditional model (stay/leave territory) ---------------------------------------------
# modeling first part of conditional binomial model
# did the raven choose to leaves its territory

## DEPENDENT VARIABLE ##
# terr_bin
# 1 = left territory
# 0 = stayed on territory

# model with biomass number
mod_terr_bms <- glmer(terr_bin ~ (1|raven_id) + rf_active_kill * scale(final_take_bms1) + scale(rf_avg_terr_kill_density) + 
                         scale(dist2nentrance) + study_period * scale(temp_max) + scale(snow_depth) + scale(prop_group_left_terr),
                       data = ws_model_data,
                       family = "binomial",
                       nAGQ = 40,
                       control = cntrl)
summary(mod_terr_bms)


# model with hunting season (changes result for active_kill)
# including the interaction effect messes with active_kill because of high error with interaction term
# with the new polygon and updated bison hunting season, this is even more out of control
mod_terr_hseason <- glmer(terr_bin ~ (1|raven_id) + rf_active_kill + hunt_season + scale(rf_avg_terr_kill_density) + 
                            scale(dist2nentrance) + study_period * scale(temp_max) + scale(snow_depth) + scale(prop_group_left_terr),
                         data = ws_model_data,
                         family = "binomial",
                         nAGQ = 40,
                         control = cntrl)
summary(mod_terr_hseason)


# model with raw hunting take (no adjustment for varying weights)
# hunting covariates still not significant
mod_terr_take <- glmer(terr_bin ~ (1|raven_id) + rf_active_kill * final_take + scale(rf_avg_terr_kill_density) + 
                            scale(dist2nentrance) + study_period + scale(temp_max) + scale(snow_depth) + scale(prop_group_left_terr),
                     data = ws_model_data,
                     family = "binomial",
                     nAGQ = 40,
                     control = cntrl)
summary(mod_terr_take)

AIC(mod_terr_bms)
AIC(mod_terr_hseason) #best
AIC(mod_terr_take)


# bootstrapping parameter confidence intervals -------------------------------

# bootstrapping parameter values from model simulations
boot_terr_bms <- boot_param_CI(nsim = 50, model = mod_terr_bms, data = ws_model_data)

# view effect plot
boot_terr_bms[[3]]


# bootstrapping parameter values from model simulations
boot_terr_season <- boot_param_CI(nsim = 50, model = mod_terr_hseason, data = ws_model_data)

# view effect plot
boot_terr_season[[3]]


# PART 2 of conditional model (visit gardiner/other) ----------------------
# modeling second part of conditional binomial model
# if the raven chose to leave its territory, did it visit the hunting area or not

## DEPENDENT VARIABLE ##
# hunt_bin
# 1 = visited hunting
# 0 = visited other place


# model with biomass number
mod_hunt_bms <- glmer(hunt_bin ~ (1|raven_id) + visit_kill + scale(final_take_bms) + scale(dist2nentrance) + 
                         scale(prop_group_visit_hunt) + scale(temp_max) + scale(snow_depth),
                       data = hunt_model_data,
                       family = "binomial",
                       nAGQ = 40,
                       control = cntrl)
summary(mod_hunt_bms)


# model with hunting season (changes study period, p value and effect direction)
mod_hunt_hseason <- glmer(hunt_bin ~ (1|raven_id) + visit_kill + hunt_season + scale(dist2nentrance) + 
                            scale(prop_group_visit_hunt) + scale(temp_max) + scale(snow_depth),
                          data = hunt_model_data,
                          family = "binomial",
                          nAGQ = 40,
                          control = cntrl)
summary(mod_hunt_hseason)


# model with raw hunting take (no adjustment for varying weights)
mod_hunt_take <- glmer(hunt_bin ~ (1|raven_id) + final_take + visit_kill + scale(dist2nentrance) + 
                            scale(prop_group_visit_hunt) + scale(temp_max) + scale(snow_depth),
                     data = hunt_model_data,
                     family = "binomial",                       
                     nAGQ = 40,
                     control = cntrl)
summary(mod_hunt_take)

AIC(mod_hunt_bms)
AIC(mod_hunt_hseason) #best
AIC(mod_hunt_take)


# bootstrapping parameter confidence intervals -------------------------------

# bootstrapping parameter values from model simulations
boot_hunt_bms <- boot_param_CI(nsim = 50, model = mod_hunt_bms, data = hunt_model_data)

# view effect plot
boot_hunt_bms[[3]]

# bootstrapping parameter values from model simulations
boot_hunt_season <- boot_param_CI(nsim = 50, model = mod_hunt_hseason, data = hunt_model_data)

# view effect plot
boot_hunt_season[[3]]