scale_seq <- seq(-2, 2, .005)

# PART 1
# impact of wolf kill visit on p(leaving)
expand.grid(visit_500 = c(TRUE, FALSE),
            rf_active_kill = c(TRUE, FALSE),
            hunt_season = c(TRUE, FALSE),
            final_take_bms1 = 0,
            rf_avg_terr_kill_density = 0,
            dist2nentrance = 0,
            study_period = c("early", "late"),
            temp_max = 0,
            snow_depth = 0,
            prop_group_left_terr = 0) %>% 
  mutate(pred = predict(mod_terr, expand.grid(visit_500 = c(TRUE, FALSE),
                              rf_active_kill = c(TRUE, FALSE),
                              hunt_season = c(TRUE, FALSE),
                              final_take_bms1 = 0,
                              rf_avg_terr_kill_density = 0,
                              dist2nentrance = 0,
                              study_period = c("early", "late"),
                              temp_max = 0,
                              snow_depth = 0,
                              prop_group_left_terr = 0),
        re.form = NA, type = "response")) %>% 
  group_by(rf_active_kill, hunt_season, study_period)%>%
  pivot_wider(names_from   = visit_500,
              values_from  = pred,
              names_prefix = "visit_") %>%
  mutate(difference = visit_FALSE - visit_TRUE) %>% 
  ungroup %>% 
  summarize(mean = mean(difference),
            min = min(difference),
            max = max(difference))



# PART 2
# impact of wolf kill visit on p(hunt)
expand.grid(visit_kill = c(TRUE, FALSE),
            hunt_season = c(TRUE, FALSE),
            final_take_bms1 = 0,
            dist2nentrance = 0,
            temp_max = 0,
            snow_depth = 0,
            prop_group_visit_hunt = 0) %>% 
  mutate(pred = predict(mod_hunt, expand.grid(visit_kill = c(TRUE, FALSE),
                                              hunt_season = c(TRUE, FALSE),
                                              final_take_bms1 = 0,
                                              dist2nentrance = 0,
                                              temp_max = 0,
                                              snow_depth = 0,
                                              prop_group_visit_hunt = 0),
                        re.form = NA, type = "response")) %>% 
  group_by(hunt_season)%>%
  pivot_wider(names_from   = visit_kill,
              values_from  = pred,
              names_prefix = "visit_") %>%
  mutate(difference = visit_FALSE - visit_TRUE) %>% 
  ungroup %>% 
  summarize(mean = mean(difference),
            min = min(difference),
            max = max(difference))


# impact of hunting season on p(hunt)
expand.grid(visit_kill = c(TRUE, FALSE),
            hunt_season = c(TRUE, FALSE),
            final_take_bms1 = 0,
            dist2nentrance = 0,
            temp_max = 0,
            snow_depth = 0,
            prop_group_visit_hunt = 0) %>% 
  mutate(pred = predict(mod_hunt, expand.grid(visit_kill = c(TRUE, FALSE),
                                              hunt_season = c(TRUE, FALSE),
                                              final_take_bms1 = 0,
                                              dist2nentrance = 0,
                                              temp_max = 0,
                                              snow_depth = 0,
                                              prop_group_visit_hunt = 0),
                        re.form = NA, type = "response")) %>% 
  group_by(visit_kill)%>%
  pivot_wider(names_from   = hunt_season,
              values_from  = pred,
              names_prefix = "hunt_") %>%
  mutate(difference = hunt_TRUE - hunt_FALSE) %>% 
  ungroup %>% 
  summarize(mean = mean(difference),
            min = min(difference),
            max = max(difference))






















