# modeling the impacts of food availability on raven winter movements decisions

library(dplyr)
library(glmmTMB)
library(ggplot2)
library(myFunctions) # custom bootstrap function


# model optimizer
cntrlTMB = glmmTMBControl(optimizer = optim, optArgs = list(method="BFGS"))

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


# PART 1 of conditional model (stay/leave territory) ---------------------------------------------
# modeling first part of conditional binomial model
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


# odds ratio
data.frame(term = rownames(summary(mod_terr)$coefficients$cond),
           estimate = summary(mod_terr)$coefficients$cond[, "Estimate"],
           se = summary(mod_terr)$coefficients$cond[, "Std. Error"],
           row.names = NULL) %>% 
  mutate(OR = round(exp(estimate), 2),
         low = round(exp(estimate - 1.96 * se), 2),
         up = round(exp(estimate + 1.96 * se), 2)) %>% 
  dplyr::select(term, OR, low, up)
  
# model summary table -----------------------------------------------------
modelsummary::modelsummary(list(" " = mod_terr),
             # set included values
             statistic = c("Std. Error" = "std.error", "p-value" = "p.value"),
             shape = term ~ model + statistic,
             # removing stats
             gof_map = NA,
             # set coef names
             coef_rename = c("(Intercept)" = "Intercept",
                             "visit_500TRUE" = "Visit kill",
                             "rf_active_killTRUE" = "Available kill",
                             "final_take_bms1" = "Biomass",
                             "hunt_seasonTRUE" = "Hunt",
                             "rf_avg_terr_kill_density" = "Density",
                             "dist2nentrance" = "Distance",
                             "study_periodlate" = "Season",
                             "snow_depth" = "Snow",
                             "temp_max" = "Temp",
                             "prop_group_left_terr" = "Social"),
             # remove random effects rows
             effects = "fixed",
             # output
             output = "flextable")  %>% 
  flextable::autofit() %>% 
  flextable::save_as_image(path = "figures/terr_table.png")


# prediction and coef plots -------------------------------

# main model (visit wolf kill)
visit_kill_table <- expand.grid(visit_500 = c(TRUE, FALSE),
                                rf_active_kill = FALSE,
                                hunt_season = FALSE,
                                final_take_bms1 = 0,
                                rf_avg_terr_kill_density = 0,
                                dist2nentrance = 0,
                                study_period = "early",
                                temp_max = 0,
                                snow_depth = 0,
                                prop_group_left_terr = 0,
                                sample_duration = 1,
                                model = "Kill visit") %>% 
  bind_cols(predict(mod_terr, expand.grid(visit_500 = c(TRUE, FALSE),
                                          rf_active_kill = FALSE,
                                          hunt_season = FALSE,
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
         lower = plogis(fit - 1.96*se.fit),
         kill = visit_500)
visit_kill_table %>% 
  ggplot(aes(x = visit_500, y = mean, col = visit_500,
             ymin = lower, ymax = upper)) +
  geom_point() +
  geom_errorbar(width = .1) +
  labs(x = "Wolf kill visit",
       y = "P(leave territory)") +
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
  theme(axis.title = element_text(size = 13),
        axis.text.y = element_text(size = 10))
ggsave("pred_terr.tif", units = "in", width = 6, height = 6, device = "tiff", path = "figures")


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
                vjust = -.6, hjust = ifelse(coeff > 0, 0.2, 1.1)), 
            size = 3) +
  # changing labels
  labs(y = "",
       x = "\u03b1") +
  # changing x axis limits
  scale_x_continuous(limits = c(-3.3, 1.2), breaks = seq(-4, 2, 1), labels = seq(-4, 2, 1),
                     expand = expansion(add = c(0.1, 0.1))) +
  # changing name and order of y axis
  scale_y_discrete(limits = c("prop_group_left_terr", "snow_depth", "temp_max", 
                              "study_periodlate", "dist2nentrance", 
                              "rf_avg_terr_kill_density", "hunt_seasonTRUE", 
                              "final_take_bms1", "rf_active_killTRUE", "visit_500TRUE"),
                   labels = c("visit_500TRUE" = "Visit kill",
                              "rf_active_killTRUE" = "Available kill",
                              "final_take_bms1" = "Biomass",
                              "hunt_seasonTRUE" = "Hunt",
                              "rf_avg_terr_kill_density" = "Density",
                              "dist2nentrance" = "Distance",
                              "study_periodlate" = "Season",
                              "snow_depth" = "Snow",
                              "temp_max" = "Temp",
                              "prop_group_left_terr" = "Social")) +
  theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 10))
ggsave("coef_terr.tif", units = "in", width = 8.5, height = 6.5, device = "tiff", path = "figures")


# PART 2 of conditional model (visit Gardiner/other) ----------------------
# modeling second part of conditional binomial model
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


# odds ratio
data.frame(term = rownames(summary(mod_hunt)$coefficients$cond),
           estimate = summary(mod_hunt)$coefficients$cond[, "Estimate"],
           se = summary(mod_hunt)$coefficients$cond[, "Std. Error"],
           row.names = NULL) %>% 
  mutate(OR = round(exp(estimate), 2),
         low = round(exp(estimate - 1.96 * se), 2),
         up = round(exp(estimate + 1.96 * se), 2)) %>% 
  dplyr::select(term, OR, low, up)
# model summary table -----------------------------------------------------
modelsummary::modelsummary(list(" " = mod_hunt),
             # set included values
             statistic = c("Std. Error" = "std.error", "p-value" = "p.value"),
             shape = term ~ model + statistic,
             # removing stats
             gof_map = NA,
             # set coef names
             coef_rename = c("(Intercept)" = "Intercept",
                             "visit_killTRUE" = "Visit",
                             "final_take_bms1" = "Biomass",
                             "hunt_seasonTRUE" = "Hunt",
                             "dist2nentrance" = "Distance",
                             "study_periodlate" = "Season",
                             "snow_depth" = "Snow",
                             "temp_max" = "Temp",
                             "prop_group_visit_hunt" = "Social"),
             # remove random effects rows
             effects = "fixed",
             # output
             output = "flextable") %>% 
  flextable::autofit() %>% 
  flextable::save_as_image(path = "figures/hunt_table.png")

# prediction and coef plots -------------------------------

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
       y = "P(visit hunting | left territory)") +
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
  theme(axis.title = element_text(size = 13))
ggsave("pred_hunt.tif", units = "in", width = 9, height = 6.5, device = "tiff", path = "figures")


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
  scale_y_discrete(limits = c("prop_group_visit_hunt", "snow_depth", "temp_max", 
                              "dist2nentrance", "hunt_seasonTRUE", "final_take_bms1", 
                              "visit_killTRUE"),
                   labels = c("prop_group_visit_hunt" = "Social",
                              "snow_depth" = "Snow", 
                              "temp_max" = "Temp", 
                              "dist2nentrance" = "Distance", 
                              "hunt_seasonTRUE" = "Hunt", 
                              "final_take_bms1" = "Biomass", 
                              "visit_killTRUE" = "Visit")) +
  theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 10))
ggsave("coef_hunt.tif", units = "in", width = 8.5, height = 6.5, device = "tiff", path = "figures")

