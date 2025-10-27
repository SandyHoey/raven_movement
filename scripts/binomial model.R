#modeling the impacts of food availability on raven winter movements decisions

library(dplyr)
library(lme4)
library(DHARMa)


#restricting to only winter study periods
full_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  
  #restricting to only winter study months
  filter(month %in% c(11, 12, 3),
         !(month %in% c(11, 12) & is.na(bms_window_1)))


# checking correlation between biomass covariates --------------------
# cor.test(full_model_data$bms_window_1, full_model_data$bms_window_3)
# cor.test(full_model_data$bms_window_1, full_model_data$bms_window_5)
# cor.test(full_model_data$bms_window_5, full_model_data$bms_window_3)
# #moving averages are basically the same
# 
# #non-average options are still very similar
# cor.test(full_model_data$bms_window_1, full_model_data$bison_daily_bms)
# cor.test(full_model_data$bms_window_1, full_model_data$bison_daily_take)


# model setup -------------------------------------------------------------

#optimizer for glmer
cntrl <- glmerControl(optimizer = "bobyqa", tol = 1e-4, optCtrl=list(maxfun=100000))


# part 1 of conditional model (stay/leave territory) ---------------------------------------------
# modeling first part of conditional binomial model
# did the raven choose to leaves its territory

## DEPENDENT VARIABLE ##
# terr_bin
# 1 = left territory
# 0 = stayed on territory

#comparing models with different biomass window sizes
mod_terr_bms3 <- glmer(terr_bin ~ (1|raven_id) + active_kill * scale(bms_window_3) + scale(yearly_terr_kill_density) + 
                         scale(dist2nentrance) + study_period + weekend,
                       data = full_model_data,
                       family = "binomial",
                       control = cntrl)

summary(mod_terr_bms3)


# PART 2 of conditional model (visit gardiner/other) ----------------------
#modeling second part of conditional binomial model
# if the raven chose to leave its territory, did it visit the hunting area or not

## DEPENDENT VARIABLE ##
# hunt_bin
# 1 = visited hunting
# 0 = visited other place

#!!! add a covariate for overall yearly wolf kill rate

#creating new data frame to only have days ravens decided to leave territory
leave_model_data <- full_model_data %>% 
  filter(terr_bin == 1)

mod_hunt_bms3 <- glmer(hunt_bin ~ (1|raven_id) + scale(bms_window_3) + scale(dist2nentrance) + study_period + weekend,
                       data = leave_model_data,
                       family = "binomial",
                       control = cntrl)

summary(mod_hunt_bms3)
