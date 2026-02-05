# modeling the impacts of food availability on raven winter movements decisions

library(dplyr)

## dataset for part 1 of conditional model
ws_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  # restricting to only winter study months
  filter((month > 11 | (month == 11 & day >= 15)) &
           (month < 12 | (month == 12 & day <= 15)) |
           (month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # removing days when there is less than 5 GPS point
  # unless the result is Jardine
  filter(!(n_point < 10 & terr_bin == F)) %>% 
  # only columns used in model
  dplyr::select(terr_bin, raven_id, rf_active_kill, rf_active_kill_3, final_take_bms, final_take_bms1, final_take, 
                hunt_season, rf_avg_terr_kill_density, dist2nentrance, 
                study_period, temp_max, snow_depth, prop_group_left_terr) %>% 
  # scaling continuous variables
  mutate(final_take_bms = scale(final_take_bms), final_take_bms1 = scale(final_take_bms1),
         final_take = scale(final_take), rf_avg_terr_kill_density = scale(rf_avg_terr_kill_density),
         dist2nentrance = scale(dist2nentrance), temp_max = scale(temp_max),
         snow_depth = scale(snow_depth), prop_group_left_terr = scale(prop_group_left_terr)) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 


## dataset for part 2 of conditional model
hunt_model_data <- readr::read_csv("data/clean/commute_data.csv") %>%
  # restricting to only winter study months
  filter((month > 11 | (month == 11 & day >= 15)) &
           (month < 12 | (month == 12 & day <= 15)) |
           (month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # only have days ravens decided to leave territory
  filter(terr_bin == 1) %>% 
  # removing days when there is less than 5 GPS point
  # unless the result is Jardine
  filter(!(n_point < 10 & hunt_bin == F)) %>% 
  # only columns used in model
  dplyr::select(hunt_bin, raven_id, visit_kill, final_take_bms, final_take_bms1, final_take, hunt_season,
                dist2nentrance, study_period, temp_max, snow_depth, prop_group_visit_hunt) %>% 
  # scaling continuous variables
  mutate(final_take_bms = scale(final_take_bms), final_take_bms1 = scale(final_take_bms1),
         final_take = scale(final_take), dist2nentrance = scale(dist2nentrance), 
         temp_max = scale(temp_max), snow_depth = scale(snow_depth), 
         prop_group_visit_hunt = scale(prop_group_visit_hunt)) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 


# model setup -------------------------------------------------------------
library(lme4)
library(DHARMa)
library(ggplot2)
library(myFunctions) # custom bootstrap function

# optimizer for glmer
cntrl <- glmerControl(optimizer = "bobyqa", tol = 1e-4, optCtrl=list(maxfun=100000))


# PART 1 of conditional model (stay/leave territory) ---------------------------------------------
# modeling first part of conditional binomial model
# did the raven choose to leaves its territory

## DEPENDENT VARIABLE ##
# terr_bin
# 1 = left territory
# 0 = stayed on territory

# model with biomass number
mod_terr_bms <- glmer(terr_bin ~ (1|raven_id) + rf_active_kill * final_take_bms1 + rf_avg_terr_kill_density + 
                        dist2nentrance + study_period * temp_max + snow_depth + prop_group_left_terr,
                      data = ws_model_data,
                      family = "binomial",
                      nAGQ = 40,
                      control = cntrl)
summary(mod_terr_bms)


# model with hunting season (changes result for active_kill)
# including the interaction effect messes with active_kill because of high error with interaction term
mod_terr_hseason <- glmer(terr_bin ~ (1|raven_id) + rf_active_kill + hunt_season + rf_avg_terr_kill_density + 
                            dist2nentrance + study_period * temp_max + snow_depth + prop_group_left_terr,
                          data = ws_model_data,
                          family = "binomial",
                          nAGQ = 40,
                          control = cntrl)
summary(mod_terr_hseason)


# model with all the hunting covariates
mod_terr <- glmer(terr_bin ~ (1|raven_id) + rf_active_kill * final_take_bms1 + hunt_season + rf_avg_terr_kill_density + 
                    dist2nentrance + study_period * temp_max + snow_depth + prop_group_left_terr,
                  data = ws_model_data,
                  family = "binomial",
                  nAGQ = 40,
                  control = cntrl)
summary(mod_terr)


# modeling without weather covariates
mod_terr_noweather <- glmer(terr_bin ~ (1|raven_id) + rf_active_kill * final_take_bms1 + hunt_season + rf_avg_terr_kill_density + 
                    dist2nentrance + study_period + prop_group_left_terr,
                  data = ws_model_data,
                  family = "binomial",
                  nAGQ = 40,
                  control = cntrl)
summary(mod_terr_noweather)

AIC(mod_terr_bms)
AIC(mod_terr_hseason)
AIC(mod_terr) #best


# bootstrap -------------------------------

# plot coefficient CI
terr_coef <- confint(mod_terr)
terr_coef %>%
  as.data.frame %>% 
  # removing unused rows
  filter(!rownames(.) %in% c(".sig01", "(Intercept)")) %>% 
  # adding model coefficient
  mutate(coeff = fixef(mod_terr)[names(fixef(mod_terr)) != "(Intercept)"],
         FE = rownames(.)) %>% 
  ggplot + 
  geom_point(aes(x = coeff, y = FE), colour = "black") +
  # confidence intervals
  geom_segment(aes(x = `2.5 %`, xend = `97.5 %`, y = FE, yend = FE), colour = "black") +
  # creating dashed line around 0
  geom_vline(xintercept = 0, lty = "dashed") +
  # adding model coefficient value to plot
  geom_text(aes(x = coeff, y = FE, label = round(coeff, 2),
                vjust = -.6, hjust = ifelse(coeff > 0, 0.2, 1)), 
            size = 3) +
  theme(legend.position = "none") +
  labs(y = "",
       x = "\u03b2")+ 
  # changing name and order of y axis
  scale_y_discrete(limits = c("study_periodlate:temp_max", "rf_active_killTRUE:final_take_bms1", 
                              "prop_group_left_terr", "snow_depth", "temp_max", "study_periodlate", 
                              "dist2nentrance", "rf_avg_terr_kill_density", "hunt_seasonTRUE", 
                              "final_take_bms1", "rf_active_killTRUE"),
                   labels = c("study_periodlate:temp_max" = "Study period * Max temp",
                              "rf_active_killTRUE:final_take_bms1" = "Active kill * Hunting biomass", 
                              "prop_group_left_terr" = "Proportion traveling",
                              "snow_depth" = "Snow depth", 
                              "temp_max" = "Max temperature", 
                              "study_periodlate" = "Study period", 
                              "dist2nentrance" = "Distance", 
                              "rf_avg_terr_kill_density" = "Kill density",
                              "hunt_seasonTRUE" = "Hunting season", 
                              "final_take_bms1" = "Hunting biomass", 
                              "rf_active_killTRUE" = "Active kill"))
ggsave("coef_terr.svg", device = "svg", path = "reports")
  

# bootstrapping predictions values from model simulations
boot_terr <- boot_param_CI(nsim = 500, model = mod_terr, data = ws_model_data, pred_CI = TRUE,
                           newData = expand.grid(rf_active_kill = c(TRUE, FALSE),
                                                 hunt_season = c(TRUE, FALSE),
                                                 final_take_bms1 = 0,
                                                 rf_avg_terr_kill_density = 0,
                                                 dist2nentrance = 0,
                                                 study_period = "early",
                                                 temp_max = 0,
                                                 snow_depth = 0,
                                                 prop_group_left_terr = 0))


  
# plotting predictions for wolf kills and hunting season
(terr_plot <- boot_terr[[4]] %>% 
    # plotting
    ggplot(aes(x = rf_active_kill, y = mean, col = rf_active_kill,
               ymin = lower, ymax = upper)) +
    geom_point() +
    facet_wrap(~hunt_season, 
               labeller = labeller(hunt_season = c("FALSE" = "No Hunting", "TRUE" = "Hunting"))) +
    geom_errorbar(width = .1) +
    labs(title = "Leaving territory",
         x = "Active wolf kill",
         y = "Predicted Probability")) +
  # custom color/texture scheme
  scale_color_manual(values = c("TRUE" = "#006CD1", "FALSE" = "#DC3220")) +
  # removing legend
  guides(color = "none") +
  theme_classic() +
  # removing title
  ggtitle("", subtitle = "")
ggsave("pred_terr_hseason.svg", device = "svg", path = "reports")


# PART 2 of conditional model (visit Gardiner/other) ----------------------
# modeling second part of conditional binomial model
# if the raven chose to leave its territory, did it visit the hunting area or not

## DEPENDENT VARIABLE ##
# hunt_bin
# 1 = visited hunting region
# 0 = visited other place

# model with biomass number
mod_hunt_bms <- glmer(hunt_bin ~ (1|raven_id) + visit_kill * final_take_bms1 + dist2nentrance + 
                        prop_group_visit_hunt + temp_max + snow_depth,
                      data = hunt_model_data,
                      family = "binomial",
                      nAGQ = 40,
                      control = cntrl)
summary(mod_hunt_bms)


# model with hunting season (changes study period, p value and effect direction)
mod_hunt_hseason <- glmer(hunt_bin ~ (1|raven_id) + visit_kill + hunt_season + dist2nentrance + 
                            prop_group_visit_hunt + temp_max + snow_depth,
                          data = hunt_model_data,
                          family = "binomial",
                          nAGQ = 40,
                          control = cntrl)
summary(mod_hunt_hseason)


# model with all hunting covariates
mod_hunt <- glmer(hunt_bin ~ (1|raven_id) + visit_kill * final_take_bms1 + hunt_season + dist2nentrance + 
                    temp_max + snow_depth + prop_group_visit_hunt,
                  data = hunt_model_data,
                  family = "binomial",
                  nAGQ = 40,
                  control = cntrl)
summary(mod_hunt)

AIC(mod_hunt_bms)
AIC(mod_hunt_hseason) 
AIC(mod_hunt) #best


# bootstrap -------------------------------

# plot coefficient CI
hunt_coef <- confint(mod_hunt)
hunt_coef %>%
  as.data.frame %>% 
  # removing unused rows
  filter(!rownames(.) %in% c(".sig01", "(Intercept)")) %>% 
  # adding model coefficient
  mutate(coeff = fixef(mod_hunt)[names(fixef(mod_hunt)) != "(Intercept)"],
         FE = rownames(.)) %>% 
  ggplot + 
  geom_point(aes(x = coeff, y = FE), colour = "black") +
  # confidence intervals
  geom_segment(aes(x = `2.5 %`, xend = `97.5 %`, y = FE, yend = FE), colour = "black") +
  # creating dashed line around 0
  geom_vline(xintercept = 0, lty = "dashed") +
  # adding model coefficient value to plot
  geom_text(aes(x = coeff, y = FE, label = round(coeff, 2),
                vjust = -.6, hjust = ifelse(coeff > 0, 0.2, 1)), 
            size = 3) +
  theme(legend.position = "none") +
  labs(y = "",
       x = "\u03b2") + 
  # changing name and order of y axis
  scale_y_discrete(limits = c("visit_killTRUE:final_take_bms1", "prop_group_visit_hunt", 
                              "snow_depth", "temp_max", "dist2nentrance", 
                              "hunt_seasonTRUE", "final_take_bms1", "visit_killTRUE"),
                   labels = c("visit_killTRUE:final_take_bms1" = "Visit kill * Hunting biomass", 
                              "prop_group_visit_hunt" = "Proportion traveling",
                              "snow_depth" = "Snow depth", 
                              "temp_max" = "Max temperature", 
                              "dist2nentrance" = "Distance", 
                              "hunt_seasonTRUE" = "Hunting season", 
                              "final_take_bms1" = "Hunting biomass", 
                              "visit_killTRUE" = "Visit kill"))
ggsave("coef_hunt.svg", device = "svg", path = "reports")


# prediction for kill visit and hunting season
boot_hunt <- boot_param_CI(nsim = 500, model = mod_hunt, data = hunt_model_data, pred_CI = TRUE, 
                           newData = expand.grid(visit_kill = c(TRUE, FALSE),
                                                 hunt_season = c(TRUE, FALSE),
                                                 final_take_bms1 = 0,
                                                 dist2nentrance = 0,
                                                 temp_max = 0,
                                                 snow_depth = 0,
                                                 prop_group_visit_hunt = 0))


# plotting predictions for wolf kills and hunting season
(hunt_plot <- boot_hunt[[4]] %>% 
    # plotting
    ggplot(aes(x = visit_kill, y = mean, col = visit_kill,
               ymin = lower, ymax = upper)) +
    geom_point() +
    facet_wrap(~hunt_season, 
               labeller = labeller(hunt_season = c("FALSE" = "No Hunting", "TRUE" = "Hunting"))) +
    geom_errorbar(width = .1) +
    labs(title = "Visiting hunting region",
         x = "Visit Kill",
         y = "Predicted Probability")) +
  # custom color/texture scheme
  scale_color_manual(values = c("TRUE" = "#006CD1", "FALSE" = "#DC3220")) +
  theme_classic() +
  # removing legend
  guides(color = "none") +
  # removing title
  ggtitle("", subtitle = "")
ggsave("pred_hunt_hseason.svg", device = "svg", path = "reports")

