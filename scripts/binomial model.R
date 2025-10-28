#modeling the impacts of food availability on raven winter movements decisions

library(dplyr)

## dataset for part 1 of conditional model
ws_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  
  #restricting to only winter study months
  filter((paste(month, day, sep = "-") >= "11-15" &
            paste(month, day, sep = "-") <= "12-15") |
          (paste(month, day, sep = "-") >= "3-1" &
            paste(month, day, sep = "-") <= "3-30")) %>% 
  
  #removing rows that don't have a previous_day 
  filter(!is.na(previous_decision_terr))


## dataset for part 2 of conditional model
hunt_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  
  #removing rows that don't have a previous_day 
  filter(!is.na(previous_decision_terr)) %>%
  
  #only have days ravens decided to leave territory
  filter(terr_bin == 1)


# checking correlation between biomass covariates --------------------
# cor.test(hunt_model_data$bms_window_1, hunt_model_data$bms_window_3)
# cor.test(hunt_model_data$bms_window_1, hunt_model_data$bms_window_5)
# cor.test(hunt_model_data$bms_window_5, hunt_model_data$bms_window_3)
# #moving averages are basically the same
# 
# #non-average options are still very similar
# cor.test(hunt_model_data$bms_window_1, hunt_model_data$final_take_bms)

# model setup -------------------------------------------------------------
library(lme4)
library(DHARMa)

#optimizer for glmer
cntrl <- glmerControl(optimizer = "bobyqa", tol = 1e-4, optCtrl=list(maxfun=100000))


# part 1 of conditional model (stay/leave territory) ---------------------------------------------
# modeling first part of conditional binomial model
# did the raven choose to leaves its territory

## DEPENDENT VARIABLE ##
# terr_bin
# 1 = left territory
# 0 = stayed on territory

#model with biomass number
#I changed active_kill to only within 1 day of wolves leaving and that made a big difference
mod_terr_bms1 <- glmer(terr_bin ~ (1|raven_id) + active_kill * scale(bms_window_1) + scale(yearly_terr_kill_density) + 
                         scale(dist2nentrance) + study_period + scale(prop_group_left_terr),
                       data = ws_model_data,
                       family = "binomial",
                       control = cntrl)
summary(mod_terr_bms1)


#model with hunting season (changes result for active_kill)
mod_terr_hseason <- glmer(terr_bin ~ (1|raven_id) + active_kill * hunt_season + scale(yearly_terr_kill_density) + 
                         scale(dist2nentrance) + study_period + scale(prop_group_left_terr),
                       data = ws_model_data,
                       family = "binomial",
                       control = cntrl)
summary(mod_terr_hseason)


#model with categorical high/low hunt (no changes)
mod_terr_hl <- glmer(terr_bin ~ (1|raven_id) + active_kill * take_high_low + scale(yearly_terr_kill_density) + 
                            scale(dist2nentrance) + study_period + scale(prop_group_left_terr),
                          data = ws_model_data,
                          family = "binomial",
                          control = cntrl)
summary(mod_terr_hl)

AIC(mod_terr_bms1) #equally good
AIC(mod_terr_hseason) #equally good
AIC(mod_terr_hl)


# PART 2 of conditional model (visit gardiner/other) ----------------------
#modeling second part of conditional binomial model
# if the raven chose to leave its territory, did it visit the hunting area or not

## DEPENDENT VARIABLE ##
# hunt_bin
# 1 = visited hunting
# 0 = visited other place

#!!! add a covariate for overall yearly wolf kill rate

#model with biomass number
mod_hunt_bms1 <- glmer(hunt_bin ~ (1|raven_id) + scale(bms_window_1) + scale(dist2nentrance) + 
                         study_period + scale(prop_group_visit_hunt),
                       data = hunt_model_data,
                       family = "binomial",
                       control = cntrl)
summary(mod_hunt_bms1)


#model with hunting season (changes study period, p value and effect direction)
mod_hunt_hseason <- glmer(hunt_bin ~ (1|raven_id) + hunt_season + scale(dist2nentrance) + 
                         study_period + scale(prop_group_visit_hunt),
                       data = hunt_model_data,
                       family = "binomial",
                       control = cntrl)
summary(mod_hunt_hseason)


#model with categorical high/low (changes study period, p value and effect direction)
mod_hunt_hl <- glmer(hunt_bin ~ (1|raven_id) + take_high_low + scale(dist2nentrance) + 
                            study_period + scale(prop_group_visit_hunt),
                          data = hunt_model_data,
                          family = "binomial",
                          control = cntrl)
summary(mod_hunt_hl)

AIC(mod_hunt_bms1)
AIC(mod_hunt_hseason)
AIC(mod_hunt_hl) #best by far
