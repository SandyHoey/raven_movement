# plotting the predictions from binomial models

library(ggplot2)

# running models
source("scripts/model_workflow/01_models.R")


# PART 1 of decision (stay/leave territory) ---------------------------------------------

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



# PART 2 of decision (visit recreational hunting or other) ----------------------

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

