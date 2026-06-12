# Plotting the coefficients from the binomial models

library(ggplot2)

# running models
source("scripts/model_workflow/01_models.R")


# PART 1 of decision (stay/leave territory) ---------------------------------------------

# calculating coefficients
terr_coef <- confint(mod_terr, parm = "beta_", method = "profile")

terr_coef %>%
  as.data.frame %>% 
  # removing unused rows
  filter(!rownames(.) %in% c(".sig01", "(Intercept)")) %>% 
  # adding model coefficient
  mutate(coeff = fixef(mod_terr)$cond[names(fixef(mod_terr)$cond) != "(Intercept)"],
         FE = rownames(.)) %>% 
  # plotting
  ggplot + 
  geom_point(aes(x = coeff, y = FE), colour = "black") +
  # confidence intervals
  geom_segment(aes(x = `2.5 %`, xend = `97.5 %`, y = FE, yend = FE), colour = "black") +
  # creating dashed line at 0
  geom_vline(xintercept = 0, lty = "dashed") +
  # # adding model coefficient value to plot
  # geom_text(aes(x = coeff, y = FE, label = round(coeff, 2),
  #               vjust = -.6, hjust = ifelse(coeff > 0, 0.2, 1.1)), 
  #           size = 3) +
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
                   labels = c("visit_500TRUE" = "Visit",
                              "rf_active_killTRUE" = "Available",
                              "final_take_bms1" = "Biomass",
                              "hunt_seasonTRUE" = "Hunting",
                              "rf_avg_terr_kill_density" = "Density",
                              "dist2nentrance" = "Distance",
                              "study_periodlate" = "Late Winter",
                              "snow_depth" = "Snow",
                              "temp_max" = "Temp",
                              "prop_group_left_terr" = "Social")) +
  theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 12))
ggsave("coef_terr.tif", units = "in", width = 8.5, height = 6.5, device = "tiff", path = "figures")


# PART 2 of decision (visit recreational hunting or other) ----------------------
# calculating coefficients
hunt_coef <- confint(mod_hunt, parm = "beta_", method = "profile")

hunt_coef %>%
  as.data.frame %>% 
  # removing unused rows
  filter(!rownames(.) %in% c(".sig01", "(Intercept)")) %>% 
  # adding model coefficient
  mutate(coeff = fixef(mod_hunt)$cond[names(fixef(mod_hunt)$cond) != "(Intercept)"],
         FE = rownames(.)) %>% 
  # plotting
  ggplot + 
  geom_point(aes(x = coeff, y = FE), colour = "black") +
  # confidence intervals
  geom_segment(aes(x = `2.5 %`, xend = `97.5 %`, y = FE, yend = FE), colour = "black") +
  # creating dashed line at 0
  geom_vline(xintercept = 0, lty = "dashed") +
  # # adding model coefficient value to plot
  # geom_text(aes(x = coeff, y = FE, label = round(coeff, 2),
  #               vjust = -.6, hjust = ifelse(coeff > 0, 0.2, 1)), 
  #           size = 3) +
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
                              "hunt_seasonTRUE" = "Hunting", 
                              "final_take_bms1" = "Biomass", 
                              "visit_killTRUE" = "Kill")) +
  theme_classic() +
  theme(axis.title = element_text(size = 13, face = "bold"),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 12))
ggsave("coef_hunt.tif", units = "in", width = 8.5, height = 6.5, device = "tiff", path = "figures")


