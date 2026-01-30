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
  filter(!(n_point < 5 & terr_bin == F)) %>% 
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
  filter(!(n_point < 5 & hunt_bin == F)) %>% 
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
# with the new polygon and updated bison hunting season, this is even more out of control
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

AIC(mod_terr_bms)
AIC(mod_terr_hseason)
AIC(mod_terr) #best


# bootstrap -------------------------------

# bootstrapping parameter values from model simulations
boot_terr <- boot_param_CI(nsim = 500, model = mod_terr, data = ws_model_data,
                           newData = expand.grid(rf_active_kill = c(TRUE, FALSE),
                                                 hunt_season = c(TRUE, FALSE),
                                                 final_take_bms1 = 0,
                                                 rf_avg_terr_kill_density = 0,
                                                 dist2nentrance = 0,
                                                 study_period = "early",
                                                 temp_max = 0,
                                                 snow_depth = 0,
                                                 prop_group_left_terr = 0))

# view effect plot
boot_terr[[3]]


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
         y = "Predicted Probability"))


# prediction bootstrap -----------------------------------------------

# model with biomass number
nsim = 3
terr_bms_newData <- expand.grid(rf_active_kill = c(TRUE, FALSE),
                                hunt_season = c(TRUE, FALSE),
                                final_take_bms1 = 0,
                                rf_avg_terr_kill_density = 0,
                                dist2nentrance = 0,
                                study_period = "early",
                                temp_max = 0,
                                snow_depth = 0,
                                prop_group_left_terr = 0)
# creating a data frame to put predicted values form each iteration
terr_bms_pv <- matrix(NA, nrow = nsim,
                      ncol = nrow(terr_bms_newData))
# getting the sd for the random effect
terr_bms_RE <- as.data.frame(VarCorr(mod_terr))$sdcor


for(j in 1:nsim){
  sim_data <- ws_model_data %>% mutate(y = unlist(simulate(mod_terr)))
  sim_model <- update(mod_terr, y ~ ., data = sim_data)
  
  # then predict from the updated model if the bootstrap sample converged 
  if(is.null(summary(sim_model)$optinfo$conv$lme4$messages) == TRUE){
    # add random variation for the random effect by adding a pull from a normal distribution at 0 with a sd from the model
    # back transform predictions since this is negative binomial on log scale
    terr_bms_pv[j,] <- plogis(predict(sim_model, terr_bms_newData, re.form = NA) + rnorm(1, 0, sd = terr_bms_RE))
  }
}
terr_bms_newData$mean <- apply(terr_bms_pv, 2, function(x)mean(x,na.rm = TRUE))
terr_bms_newData$lower <- apply(terr_bms_pv, 2, function(x)quantile(x,p = 0.025, na.rm = TRUE))
terr_bms_newData$upper <- apply(terr_bms_pv, 2, function(x)quantile(x,p = 0.975, na.rm = TRUE))

# plotting predictions for the wolf kills and hunter biomass
(terr_bms_plot <- terr_bms_newData %>% 
    # plotting
    ggplot(aes(x = rf_active_kill, y = mean, col = rf_active_kill,
               ymin = lower, ymax = upper)) +
    geom_point() +
    facet_wrap(~hunt_season) +
    geom_errorbar(width = .1) +
    labs(title = "Leaving territory",
         x = "Scaled available hunter biomass",
         y = "Predicted Probability")) +
  scale_x_continuous(labels = -2:2, breaks = -2:2)


# model with hunting season
nsim = 10
terr_hseason_newData <- expand.grid(rf_active_kill = c(TRUE, FALSE),
                                    hunt_season = c(TRUE, FALSE),
                                    rf_avg_terr_kill_density = 0,
                                    dist2nentrance = 0,
                                    study_period = "late",
                                    temp_max = 0,
                                    snow_depth = 0,
                                    prop_group_left_terr = 0)
# creating a data frame to put predicted values form each iteration
terr_hseason_pv <- matrix(NA, nrow = nsim,
                          ncol = nrow(terr_hseason_newData))
# getting the sd for the random effect
terr_hseason_RE <- as.data.frame(VarCorr(mod_terr_hseason))$sdcor


for(j in 1:nsim){
  sim_data <- ws_model_data %>% mutate(y = unlist(simulate(mod_terr_hseason)))
  sim_model <- update(mod_terr_hseason, y ~ ., data = sim_data)
  
  # then predict from the updated model if the bootstrap sample converged 
  if(is.null(summary(sim_model)$optinfo$conv$lme4$messages) == TRUE){
    # add random variation for the random effect by adding a pull from a normal distribution at 0 with a sd from the model
    # back transform predictions since this is negative binomial on log scale
    terr_hseason_pv[j,] <- plogis(predict(sim_model, terr_hseason_newData, re.form = NA) + rnorm(1, 0, sd = terr_hseason_RE))
  }
}
terr_hseason_newData$mean <- apply(terr_hseason_pv, 2, function(x)mean(x,na.rm = TRUE))
terr_hseason_newData$lower <- apply(terr_hseason_pv, 2, function(x)quantile(x,p = 0.025, na.rm = TRUE))
terr_hseason_newData$upper <- apply(terr_hseason_pv, 2, function(x)quantile(x,p = 0.975, na.rm = TRUE))

# plotting predictions for wolf kills and hunting season
(terr_hseason_plot <- terr_hseason_newData %>% 
    # plotting
    ggplot(aes(x = rf_active_kill, y = mean, col = rf_active_kill,
               ymin = lower, ymax = upper)) +
    geom_point() +
    facet_wrap(~hunt_season) +
    geom_errorbar(width = .1) +
    labs(title = "Leaving territory",
         x = "Hunting season",
         y = "Predicted Probability"))


# PART 2 of conditional model (visit gardiner/other) ----------------------
# modeling second part of conditional binomial model
# if the raven chose to leave its territory, did it visit the hunting area or not

## DEPENDENT VARIABLE ##
# hunt_bin
# 1 = visited hunting
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
                    prop_group_visit_hunt + temp_max + snow_depth,
                  data = hunt_model_data,
                  family = "binomial",
                  nAGQ = 40,
                  control = cntrl)
summary(mod_hunt)

AIC(mod_hunt_bms)
AIC(mod_hunt_hseason) 
AIC(mod_hunt) #best


# bootstrap -------------------------------

# bootstrapping parameter values from model simulations
boot_hunt <- boot_param_CI(nsim = 500, model = mod_hunt, data = hunt_model_data, 
                           newData = expand.grid(visit_kill = c(TRUE, FALSE),
                                                 hunt_season = c(TRUE, FALSE),
                                                 final_take_bms1 = 0,
                                                 dist2nentrance = 0,
                                                 temp_max = 0,
                                                 snow_depth = 0,
                                                 prop_group_visit_hunt = 0))

# parameter estimates
boot_hunt[[3]]


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
       x = "Found Kill",
       y = "Predicted Probability"))


# prediction bootstrap -----------------------------------------------

# model with biomass number
nsim = 10
hunt_bms_newData <- expand.grid(visit_kill = c(TRUE, FALSE),
                                final_take_bms1 = seq(-2, 2, 0.1),
                                dist2nentrance = 0,
                                temp_max = 0,
                                snow_depth = 0,
                                prop_group_visit_hunt = 0)
# creating a data frame to put predicted values form each iteration
hunt_bms_pv <- matrix(NA, nrow = nsim,
                      ncol = nrow(hunt_bms_newData))
# getting the sd for the random effect
hunt_bms_RE <- as.data.frame(VarCorr(mod_hunt_bms))$sdcor


for(j in 1:nsim){
  sim_data <- hunt_model_data %>% mutate(y = unlist(simulate(mod_hunt_bms)))
  sim_model <- update(mod_hunt_bms, y ~ ., data = sim_data)
  
  # then predict from the updated model if the bootstrap sample converged 
  if(is.null(summary(sim_model)$optinfo$conv$lme4$messages) == TRUE){
    # add random variation for the random effect by adding a pull from a normal distribution at 0 with a sd from the model
    # back transform predictions since this is negative binomial on log scale
    hunt_bms_pv[j,] <- plogis(predict(sim_model, hunt_bms_newData, re.form = NA) + rnorm(1, 0, sd = hunt_bms_RE))
  }
}
hunt_bms_newData$mean <- apply(hunt_bms_pv, 2, function(x)mean(x,na.rm = TRUE))
hunt_bms_newData$lower <- apply(hunt_bms_pv, 2, function(x)quantile(x,p = 0.025, na.rm = TRUE))
hunt_bms_newData$upper <- apply(hunt_bms_pv, 2, function(x)quantile(x,p = 0.975, na.rm = TRUE))

# plotting predictions for the wolf kills and hunter biomass
(hunt_bms_plot <- hunt_bms_newData %>% 
    # plotting
    ggplot(aes(x = final_take_bms1, y = mean, col = visit_kill,
               ymin = lower, ymax = upper)) +
    geom_line() +
    # geom_errorbar(width = .1) +
    labs(title = "Visiting hunting region",
         x = "Scaled available hunter biomass",
         y = "Predicted Probability")) +
  scale_x_continuous(labels = -2:2, breaks = -2:2)


# model with hunting season
nsim = 500
hunt_hseason_newData <- expand.grid(visit_kill = c(TRUE, FALSE),
                                    hunt_season = c(TRUE, FALSE),
                                    dist2nentrance = 0,
                                    temp_max = 0,
                                    snow_depth = 0,
                                    prop_group_visit_hunt = 0)
# creating a data frame to put predicted values form each iteration
hunt_hseason_pv <- matrix(NA, nrow = nsim,
                          ncol = nrow(hunt_hseason_newData))
# getting the sd for the random effect
hunt_hseason_RE <- as.data.frame(VarCorr(mod_hunt_hseason))$sdcor


for(j in 1:nsim){
  sim_data <- hunt_model_data %>% mutate(y = unlist(simulate(mod_hunt_hseason)))
  sim_model <- update(mod_hunt_hseason, y ~ ., data = sim_data)
  
  # then predict from the updated model if the bootstrap sample converged 
  if(is.null(summary(sim_model)$optinfo$conv$lme4$messages) == TRUE){
    # add random variation for the random effect by adding a pull from a normal distribution at 0 with a sd from the model
    # back transform predictions since this is negative binomial on log scale
    hunt_hseason_pv[j,] <- plogis(predict(sim_model, hunt_hseason_newData, re.form = NA) + rnorm(1, 0, sd = hunt_hseason_RE))
  }
}
hunt_hseason_newData$mean <- apply(hunt_hseason_pv, 2, function(x)mean(x,na.rm = TRUE))
hunt_hseason_newData$lower <- apply(hunt_hseason_pv, 2, function(x)quantile(x,p = 0.025, na.rm = TRUE))
hunt_hseason_newData$upper <- apply(hunt_hseason_pv, 2, function(x)quantile(x,p = 0.975, na.rm = TRUE))

# plotting predictions for wolf kills and hunting season
(hunt_hseason_plot <- hunt_hseason_newData %>% 
    # plotting
    ggplot(aes(x = visit_kill, y = mean, col = visit_kill,
               ymin = lower, ymax = upper)) +
    geom_point() +
    facet_wrap(~hunt_season, 
               labeller = labeller(hunt_season = c("FALSE" = "No Hunting", "TRUE" = "Hunting"))) +
    geom_errorbar(width = .1) +
    labs(title = "Visiting hunting region",
         x = "Found Kill",
         y = "Predicted Probability"))

