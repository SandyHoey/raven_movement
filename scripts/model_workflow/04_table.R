# creating model output tables from model results


library(modelsummary)
library(flextable)

# running models
source("scripts/model_workflow/01_models.R")


# PART 1 of decision (stay/leave territory) ---------------------------------------------

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
                             "study_periodlate" = "LateWinter",
                             "snow_depth" = "Snow",
                             "temp_max" = "Temp",
                             "prop_group_left_terr" = "Social"),
             # remove random effects rows
             effects = "fixed",
             # output type
             output = "flextable")  %>% 
  # automatically set table formatting
  autofit() %>% 
  save_as_image(path = "figures/terr_table.png")


# PART 2 of decision (visit recreational hunting or other) ----------------------

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
  save_as_image(path = "figures/hunt_table.png")