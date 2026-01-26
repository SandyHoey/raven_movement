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


# part 1 of conditional model (stay/leave territory) ---------------------------------------------
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


# model with raw hunting take (no adjustment for varying weights)
# hunting covariates still not significant
mod_terr_take <- glmer(terr_bin ~ (1|raven_id) + rf_active_kill * final_take + rf_avg_terr_kill_density + 
                         dist2nentrance + study_period + temp_max + snow_depth + prop_group_left_terr,
                       data = ws_model_data,
                       family = "binomial",
                       nAGQ = 40,
                       control = cntrl)
summary(mod_terr_take)

AIC(mod_terr_bms)
AIC(mod_terr_hseason) #best
AIC(mod_terr_take)


# parameter confidence intervals bootstrap -------------------------------

# bootstrapping parameter values from model simulations
boot_terr_bms <- boot_param_CI(nsim = 50, model = mod_terr_bms, data = ws_model_data)

# view effect plot
boot_terr_bms[[3]]


# bootstrapping parameter values from model simulations
boot_terr_season <- boot_param_CI(nsim = 50, model = mod_terr_hseason, data = ws_model_data)

# view effect plot
boot_terr_season[[3]]


# prediction bootstrap -----------------------------------------------

# model with biomass number
nsim = 10
terr_bms_newData <- expand.grid(rf_active_kill = c(TRUE, FALSE),
                                final_take_bms1 = seq(-2, 2, 0.1),
                                rf_avg_terr_kill_density = 0,
                                dist2nentrance = 0,
                                study_period = "late",
                                temp_max = 0,
                                snow_depth = 0,
                                prop_group_left_terr = 0)
# creating a data frame to put predicted values form each iteration
terr_bms_pv <- matrix(NA, nrow = nsim,
                  ncol = nrow(terr_bms_newData))
# getting the sd for the random effect
terr_bms_RE <- as.data.frame(VarCorr(mod_terr_bms))$sdcor


for(j in 1:nsim){
  sim_data <- ws_model_data %>% mutate(y = unlist(simulate(mod_terr_bms)))
  sim_model <- update(mod_terr_bms, y ~ ., data = sim_data)
  
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
    ggplot(aes(x = final_take_bms1, y = mean, col = rf_active_kill,
               ymin = lower, ymax = upper)) +
    geom_line() +
    # geom_errorbar(width = .1) +
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
mod_hunt_bms <- glmer(hunt_bin ~ (1|raven_id) + visit_kill + final_take_bms1 + dist2nentrance + 
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


# model with raw hunting take (no adjustment for varying weights)
mod_hunt_take <- glmer(hunt_bin ~ (1|raven_id) + final_take + visit_kill + dist2nentrance + 
                         prop_group_visit_hunt + temp_max + snow_depth,
                       data = hunt_model_data,
                       family = "binomial",                       
                       nAGQ = 40,
                       control = cntrl)
summary(mod_hunt_take)

AIC(mod_hunt_bms)
AIC(mod_hunt_hseason) #best
AIC(mod_hunt_take)


# parameter confidence intervals bootstrap -------------------------------

# bootstrapping parameter values from model simulations
boot_hunt_bms <- boot_param_CI(nsim = 50, model = mod_hunt_bms, data = hunt_model_data)

# view effect plot
boot_hunt_bms[[3]]

# bootstrapping parameter values from model simulations
boot_hunt_season <- boot_param_CI(nsim = 50, model = mod_hunt_hseason, data = hunt_model_data)

# view effect plot
boot_hunt_season[[3]]

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
nsim = 10
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
    ggplot(aes(x = rf_active_kill, y = mean, col = rf_active_kill,
               ymin = lower, ymax = upper)) +
    geom_point() +
    facet_wrap(~hunt_season) +
    geom_errorbar(width = .1) +
    labs(title = "Visiting hunting region",
         x = "Hunting season",
         y = "Predicted Probability"))