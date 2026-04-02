# modeling impacts of hunting season and biomass on raven decisions to leave territory

library(dplyr)

## dataset for part 1 of conditional model
winter_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  # removing days when there is less than 10 GPS point
  # unless the result is Jardine
  filter(!(n_point < 10 & terr_bin == F)) %>% 
  # only columns used in model
  dplyr::select(terr_bin, raven_id, final_take_bms, final_take_bms1, final_take, 
                hunt_season, dist2nentrance, 
                study_period, temp_max, snow_depth, prop_group_left_terr) %>% 
  # scaling continuous variables
  mutate(final_take_bms = scale(final_take_bms), final_take_bms1 = scale(final_take_bms1),
         final_take = scale(final_take),
         dist2nentrance = scale(dist2nentrance), temp_max = scale(temp_max),
         snow_depth = scale(snow_depth), prop_group_left_terr = scale(prop_group_left_terr)) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 


# model setup -------------------------------------------------------------
library(lme4)
library(ggplot2)
library(myFunctions) # custom bootstrap function

# optimizer for glmer
cntrl <- glmerControl(optimizer = "bobyqa", tol = 1e-4, optCtrl=list(maxfun=100000))


# modeling ----------------------------------------------------------------
mod <- glmer(terr_bin ~ (1|raven_id) + final_take_bms1 + hunt_season + dist2nentrance + 
              temp_max + snow_depth + prop_group_left_terr,
             data = winter_data,
             family = "binomial",
             nAGQ = 40,
             control = cntrl)
summary(mod)

