# modeling the impacts of food availability on raven winter movements decisions

library(dplyr)
library(glmmTMB)
library(modelsummary)
library(flextable)

# model optimizer
cntrlTMB = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS"))

# modeling in march only, keeping wolf kill covariates --------------------------------------------------------------
## dataset for part 1 of conditional model
ws_model_data <- readr::read_csv("data/clean/commute_data.csv")  %>% 
  # limit to study years
  filter(date < "2024-3-31") %>% 
  # restricting to only winter study months
  filter((month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # only columns used in model
  dplyr::select(terr_bin, raven_id, rf_active_kill, visit_500, final_take_bms, final_take_bms1, 
                hunt_season, rf_avg_terr_kill_density, dist2nentrance, 
                temp_max, snow_depth, prop_group_left_terr) %>% 
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
  filter((month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # only have days ravens decided to leave territory
  filter(terr_bin == 1) %>% 
  # only columns used in model
  dplyr::select(hunt_bin, raven_id, visit_kill, final_take_bms, final_take_bms1, final_take, hunt_season,
                dist2nentrance, temp_max, snow_depth, prop_group_visit_hunt) %>% 
  # scaling continuous variables
  mutate(final_take_bms1 = scale(final_take_bms1), dist2nentrance = scale(dist2nentrance), 
         temp_max = scale(temp_max), snow_depth = scale(snow_depth), 
         prop_group_visit_hunt = scale(prop_group_visit_hunt)) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 


# model leaving territory (decision 1)
mod_terr <- glmmTMB(terr_bin ~ (1|raven_id) + visit_500 + rf_active_kill + final_take_bms1 + hunt_season + rf_avg_terr_kill_density + 
                      dist2nentrance + temp_max + snow_depth + prop_group_left_terr,
                    data = ws_model_data,
                    family = "binomial",
                    control = cntrlTMB)
summary(mod_terr)

# writing table for SI
modelsummary(list(" " = mod_terr),
             # set included values
             statistic = c("Std. Error" = "std.error", "p-value" = "p.value"),
             shape = term ~ model + statistic,
             # removing stats
             gof_map = NA,
             # set coef names
             coef_rename = c("(Intercept)" = "Intercept",
                             "visit_500TRUE" = "Visit",
                             "rf_active_killTRUE" = "Available",
                             "final_take_bms1" = "Biomass",
                             "hunt_seasonTRUE" = "Hunting",
                             "rf_avg_terr_kill_density" = "Density",
                             "dist2nentrance" = "Distance",
                             "snow_depth" = "Snow",
                             "temp_max" = "Temp",
                             "prop_group_left_terr" = "Social"),
             # remove random effects rows
             effects = "fixed",
             # output type
             output = "flextable")  %>% 
  # automatically set table formatting
  autofit() %>% 
  save_as_image(path = "figures/sens_analy_terr_table.png")


# model visiting hunting (decision 2)
mod_hunt <- glmmTMB(hunt_bin ~ (1|raven_id) + visit_kill + final_take_bms1 + hunt_season + 
                      dist2nentrance + temp_max + snow_depth + prop_group_visit_hunt,
                    data = hunt_model_data,
                    family = "binomial",
                    control = cntrlTMB)
summary(mod_hunt)

# writing table for SI
modelsummary(list(" " = mod_hunt),
             # set included values
             statistic = c("Std. Error" = "std.error", "p-value" = "p.value"),
             shape = term ~ model + statistic,
             # removing stats
             gof_map = NA,
             # set coef names
             coef_rename = c("(Intercept)" = "Intercept",
                             "visit_killTRUE" = "Kill",
                             "final_take_bms1" = "Biomass",
                             "hunt_seasonTRUE" = "Hunting",
                             "dist2nentrance" = "Distance",
                             "snow_depth" = "Snow",
                             "temp_max" = "Temp",
                             "prop_group_visit_hunt" = "Social"),
             # remove random effects rows
             effects = "fixed",
             # output type
             output = "flextable") %>% 
  # automatically set table formatting
  autofit() %>% 
  save_as_image(path = "figures/sens_analy_hunt_table.png")

# all winter (Oct-Mar) removing wolf covariates  --------------------------------------------------------------

full_wint <- readr::read_csv("data/clean/commute_data.csv")  %>% 
  # limit to study years
  filter(date < "2024-3-31") %>% 
  # outside of FWP hunting
  # since that had the messy biomass estimates
  # !!!!! THIS IS SLIGHTLY JANKY, DATES ARENT EXACT !!!!!! 
  # !!!!! There are days in NOv/Dec that can still be added !!!!!! 
  filter((month > 12 | (month == 12 & day > 1)) &
           (month < 12 | (month == 12 & day <= 31)) |
           month %in% 1:3) %>% 
  # scaling continuous variables
  mutate(final_take_bms1 = scale(final_take_bms1),
         dist2nentrance = scale(dist2nentrance), temp_max = scale(temp_max),
         snow_depth = scale(snow_depth), 
         prop_group_left_terr = scale(prop_group_left_terr)) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 


# model leaving territory (decision 1)
mod_terr_full_wint <- glmmTMB(terr_bin ~ (1|raven_id) + final_take_bms1 + hunt_season + 
                      dist2nentrance + temp_max + snow_depth + prop_group_left_terr,
                    data = full_wint,
                    family = "binomial",
                    control = cntrlTMB)
summary(mod_terr_full_wint)


# model visiting hunting (decision 2)
mod_hunt_full_wint <- glmmTMB(hunt_bin ~ (1|raven_id) + final_take_bms1 + hunt_season + 
                      dist2nentrance + temp_max + snow_depth + prop_group_visit_hunt,
                    data = full_wint,
                    family = "binomial",
                    control = cntrlTMB)
summary(mod_hunt_full_wint)
