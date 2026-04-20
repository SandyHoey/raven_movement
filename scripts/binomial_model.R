# modeling the impacts of food availability on raven winter movements decisions

library(dplyr)
library(glmmTMB)
library(ggplot2)
library(myFunctions) # custom bootstrap function

# model optimizer
cntrlTMB = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS"))

## dataset for part 1 of conditional model
ws_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  # restricting to only winter study months
  filter((month > 11 | (month == 11 & day >= 15)) &
           (month < 12 | (month == 12 & day <= 15)) |
           (month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # removing days when there is less than 10 GPS point
  # unless the result is Jardine
  filter(n_point >= 10) %>% 
  # only columns used in model
  dplyr::select(terr_bin, raven_id, visit_500, final_take_bms, final_take_bms1, 
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
  # restricting to only winter study months
  filter((month > 11 | (month == 11 & day >= 15)) &
           (month < 12 | (month == 12 & day <= 15)) |
           (month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # only have days ravens decided to leave territory
  filter(terr_bin == 1) %>% 
  # removing days when there is less than 10 GPS point
  # unless the result is Jardine
  filter(n_point >= 10) %>% 
  # only columns used in model
  dplyr::select(hunt_bin, raven_id, visit_kill, final_take_bms, final_take_bms1, final_take, hunt_season,
                dist2nentrance, study_period, temp_max, snow_depth, prop_group_visit_hunt) %>% 
  # scaling continuous variables
  mutate(final_take_bms1 = scale(final_take_bms1), dist2nentrance = scale(dist2nentrance), 
         temp_max = scale(temp_max), snow_depth = scale(snow_depth), 
         prop_group_visit_hunt = scale(prop_group_visit_hunt)) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 


# PART 1 of conditional model (stay/leave territory) ---------------------------------------------
# modeling first part of conditional binomial model
# did the raven choose to leaves its territory

## DEPENDENT VARIABLE ##
# terr_bin
# 1 = left territory
# 0 = stayed on territory


# model with wolf kill visits in terr
mod_terr <- glmmTMB(terr_bin ~ (1|raven_id) + visit_500 * final_take_bms1 + hunt_season + rf_avg_terr_kill_density + 
                    dist2nentrance + study_period + snow_depth + temp_max + prop_group_left_terr,
                  data = ws_model_data,
                  family = "binomial",
                  control = cntrlTMB)
summary(mod_terr)


# modeling without weather covariates
mod_terr_noweather <- glmmTMB(terr_bin ~ (1|raven_id) + visit_500 * final_take_bms1 + hunt_season + 
                              rf_avg_terr_kill_density + dist2nentrance + study_period + prop_group_left_terr,
                            data = ws_model_data,
                            family = "binomial",
                            control = cntrlTMB)
summary(mod_terr_noweather)


# bootstrap -------------------------------

expand.grid(visit_500 = c(TRUE, FALSE),
                    hunt_season = c(TRUE),
                    final_take_bms1 = 0,
                    rf_avg_terr_kill_density = 0,
                    dist2nentrance = 0,
                    study_period = "early",
                    temp_max = 0,
                    snow_depth = 0,
                    prop_group_left_terr = 0,
                    sample_duration = 1) %>% 
  bind_cols(predict(mod_terr, expand.grid(visit_500 = c(TRUE, FALSE),
                                hunt_season = c(TRUE),
                                final_take_bms1 = 0,
                                rf_avg_terr_kill_density = 0,
                                dist2nentrance = 0,
                                study_period = "early",
                                temp_max = 0,
                                snow_depth = 0,
                                prop_group_left_terr = 0),
          re.form = NA, type = "link", se.fit = T)) %>% 
  mutate(mean = plogis(fit),
         upper = plogis(fit + 1.96*se.fit),
         lower = plogis(fit - 1.96*se.fit)) %>% 
  ggplot(aes(x = visit_500, y = mean, col = visit_500,
             ymin = lower, ymax = upper)) +
  geom_point() +
  geom_errorbar(width = .1) +
  labs(x = "Visit wolf kill in territory",
       y = "Predicted probability") +
  # custom color/texture scheme
  scale_color_manual(values = c("TRUE" = "#006CD1", "FALSE" = "#DC3220")) +
  # removing legend
  guides(color = "none") +
  theme_classic() +
  # increasing y axis 
  scale_y_continuous(limits = c(0, 1)) +
  # removing title
  ggtitle("", subtitle = "") +
  # increase size of axis label
  theme(axis.title = element_text(size = 13, face = "bold"))
ggsave("pred_terr_hseason.svg", units = "in", width = 9, height = 6.5, device = "svg", path = "figures")


# plot coefficient CI
terr_coef <- confint(mod_terr, parm = "beta_", method = "profile")
terr_coef %>%
  as.data.frame %>% 
  # removing unused rows
  filter(!rownames(.) %in% c(".sig01", "(Intercept)")) %>% 
  # adding model coefficient
  mutate(coeff = fixef(mod_terr)$cond[names(fixef(mod_terr)$cond) != "(Intercept)"],
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
  # changing labels
  labs(y = "",
       x = "\u03b2") +
  # changing x axis limits
  scale_x_continuous(limits = c(-7.5, 2), breaks = seq(-8, 2, 1), labels = seq(-8, 2, 1),
                     expand = expansion(add = c(0.1, 0.1))) +
  # changing name and order of y axis
  scale_y_discrete(limits = c("visit_500TRUE:final_take_bms1", "prop_group_left_terr", 
                              "snow_depth", "temp_max", "study_periodlate", 
                              "dist2nentrance", "rf_avg_terr_kill_density", "hunt_seasonTRUE", 
                              "final_take_bms1", "visit_500TRUE"),
                   labels = c("visit_500TRUE:final_take_bms1" = "Visit kill * Hunting biomass", 
                              "prop_group_left_terr" = "Proportion traveling",
                              "snow_depth" = "Snow depth", 
                              "temp_max" = "Max temperature", 
                              "study_periodlate" = "Study period (Late)", 
                              "dist2nentrance" = "Distance", 
                              "rf_avg_terr_kill_density" = "Kill density",
                              "hunt_seasonTRUE" = "Hunting season (TRUE)", 
                              "final_take_bms1" = "Hunting biomass", 
                              "visit_500TRUE" = "Visit kill (TRUE)")) +
  theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 10))
ggsave("coef_terr.svg", units = "in", width = 9, height = 6.5, device = "svg", path = "figures")


# bootstrapping predictions values from model simulations
boot_terr <- boot_param_CI(nsim = 5000, model = mod_terr, data = ws_model_data, pred_CI = FALSE,
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
               labeller = labeller(hunt_season = c("TRUE" = "Hunting", "FALSE" = "No Hunting"))) +
    geom_errorbar(width = .1) +
    labs(x = "Active wolf kill",
         y = "Predicted Probability") +
  # custom color/texture scheme
  scale_color_manual(values = c("TRUE" = "#006CD1", "FALSE" = "#DC3220")) +
  # removing legend
  guides(color = "none") +
  theme_classic() +
  # removing title
  ggtitle("", subtitle = "") +
  # increase size of axis label
  theme(axis.title = element_text(size = 13, face = "bold")))
ggsave("pred_terr_hseason.svg", units = "in", width = 9, height = 6.5, device = "svg", path = "figures")


# PART 2 of conditional model (visit Gardiner/other) ----------------------
# modeling second part of conditional binomial model
# if the raven chose to leave its territory, did it visit the hunting area or not

## DEPENDENT VARIABLE ##
# hunt_bin
# 1 = visited hunting region
# 0 = visited other place


# model with all hunting covariates
mod_hunt <- glmmTMB(hunt_bin ~ (1|raven_id) + visit_kill * final_take_bms1 + hunt_season + dist2nentrance + 
                    temp_max + snow_depth + prop_group_visit_hunt,
                  data = hunt_model_data,
                  family = "binomial",
                  control = cntrlTMB)
summary(mod_hunt)


# bootstrap -------------------------------

expand.grid(visit_kill = c(TRUE, FALSE),
            hunt_season = c(TRUE, FALSE),
            final_take_bms1 = 0,
            dist2nentrance = 0,
            temp_max = 0,
            snow_depth = 0,
            prop_group_visit_hunt = 0) %>% 
  bind_cols(predict(mod_hunt, expand.grid(visit_kill = c(TRUE, FALSE),
                                          hunt_season = c(TRUE, FALSE),
                                          final_take_bms1 = 0,
                                          dist2nentrance = 0,
                                          temp_max = 0,
                                          snow_depth = 0,
                                          prop_group_visit_hunt = 0),
                    re.form = NA, type = "link", se.fit = T)) %>% 
  mutate(mean = plogis(fit),
         upper = plogis(fit + 1.96*se.fit),
         lower = plogis(fit - 1.96*se.fit)) %>% 
  # plotting
  ggplot(aes(x = visit_kill, y = mean, col = visit_kill,
             ymin = lower, ymax = upper)) +
  geom_point() +
  facet_wrap(~hunt_season, 
             labeller = labeller(hunt_season = c("FALSE" = "No Hunting", "TRUE" = "Hunting"))) +
  geom_errorbar(width = .1) +
  labs(x = "Visit wolf kill outside territory",
       y = "Predicted probability") +
  # custom color/texture scheme
  scale_color_manual(values = c("TRUE" = "#006CD1", "FALSE" = "#DC3220")) +
  theme_classic() +
  # removing legend
  guides(color = "none") +
  # removing title
  ggtitle("", subtitle = "") +
  # make y axis full limits
  scale_y_continuous(limits = c(0, 1)) +
  # increase size of axis label
  theme(axis.title = element_text(size = 13, face = "bold"))
ggsave("pred_hunt_hseason.svg", units = "in", width = 9, height = 6.5, device = "svg", path = "figures")

  
# plot coefficient CI
hunt_coef <- confint(mod_hunt, parm = "beta_", method = "profile")
hunt_coef %>%
  as.data.frame %>% 
  # removing unused rows
  filter(!rownames(.) %in% c(".sig01", "(Intercept)")) %>% 
  # adding model coefficient
  mutate(coeff = fixef(mod_hunt)$cond[names(fixef(mod_hunt)$cond) != "(Intercept)"],
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
  # changing labels
  labs(y = "",
       x = "\u03b2") + 
  # changing x axis limits
  scale_x_continuous(limits = c(-3, 2), breaks = seq(-3, 2, 1), labels = seq(-3, 2, 1),
                     expand = expansion(add = c(0.1, 0.1))) +
  # changing name and order of y axis
  scale_y_discrete(limits = c("visit_killTRUE:final_take_bms1", "prop_group_visit_hunt", 
                              "snow_depth", "temp_max", "dist2nentrance", 
                              "hunt_seasonTRUE", "final_take_bms1", "visit_killTRUE"),
                   labels = c("visit_killTRUE:final_take_bms1" = "Visit kill * Hunting biomass", 
                              "prop_group_visit_hunt" = "Proportion traveling",
                              "snow_depth" = "Snow depth", 
                              "temp_max" = "Max temperature", 
                              "dist2nentrance" = "Distance", 
                              "hunt_seasonTRUE" = "Hunting season (TRUE)", 
                              "final_take_bms1" = "Hunting biomass", 
                              "visit_killTRUE" = "Visit kill (TRUE)")) +
  theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 10))
ggsave("coef_hunt.svg", units = "in", width = 9, height = 6.5, device = "svg", path = "figures")


# prediction for kill visit and hunting season
boot_hunt <- boot_param_CI(nsim = 5000, model = mod_hunt, data = hunt_model_data, pred_CI = TRUE, 
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
    labs(x = "Visit Kill",
         y = "Predicted Probability")) +
  # custom color/texture scheme
  scale_color_manual(values = c("TRUE" = "#006CD1", "FALSE" = "#DC3220")) +
  theme_classic() +
  # removing legend
  guides(color = "none") +
  # removing title
  ggtitle("", subtitle = "") +
  # make y axis full limits
  scale_y_continuous(limits = c(0, 1)) +
  # increase size of axis label
  theme(axis.title = element_text(size = 13, face = "bold"))
ggsave("pred_hunt_hseason.svg", units = "in", width = 9, height = 6.5, device = "svg", path = "figures")
