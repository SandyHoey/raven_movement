# gets basic statistics about the data for reporting in the beginning of the results

library(dplyr)
library(ggplot2)
library(ggpattern)


# reading in data ---------------------------------------------------------
# reading in full data (Sep-Mar)
data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  # limit to study years
  filter(date < "2024-3-31")


## dataset for part 1 of conditional model
ws_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  # limit to study years
  filter(date < "2024-3-31") %>% 
  # restricting to only winter study months
  filter((month > 11 | (month == 11 & day >= 15)) &
           (month < 12 | (month == 12 & day <= 15)) |
           (month > 3 | (month == 3 & day >= 1)) &
           (month < 3 | (month == 3 & day <= 30))) %>% 
  # only columns used in model
  dplyr::select(date, n_point, terr_bin, hunt_bin, raven_id, dump, rf_active_kill, visit_500, 
                visit_kill, final_take_bms1, hunt_season, rf_avg_terr_kill_density, 
                dist2nentrance, study_period, temp_max, snow_depth, prop_group_left_terr) %>% 
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
  dplyr::select(date, n_point, hunt_bin, terr_bin, raven_id, dump, visit_kill, final_take_bms1, 
                hunt_season, dist2nentrance, study_period, temp_max, snow_depth, prop_group_visit_hunt) %>% 
  # making sure rows are complete
  filter(complete.cases(.))

# adding raven sex to winter study model data
ws_model_data <- readxl::read_excel("data/raw/ravens_banding_tagging.xlsx", sheet = 1) %>% 
  janitor::clean_names() %>% 
  # only relevant colmuns
  dplyr::select(tag_id, sex_based_on_dna) %>% 
  # adding to model data
  right_join(ws_model_data, by = join_by(tag_id == raven_id)) %>% 
  # better column name
  rename(sex = sex_based_on_dna,
         raven_id = tag_id)


# summary data ------------------------------------------------------------

# total number of ravens included in study
length(unique(data$raven_id))


# decision days
nrow(data) # including all winter
mean(table(data$raven_id))
sd(table(data$raven_id))
max(table(data$raven_id))
min(table(data$raven_id))
nrow(ws_model_data) # for the model
mean(table(ws_model_data$raven_id))
sd(table(ws_model_data$raven_id))
max(table(ws_model_data$raven_id))
min(table(ws_model_data$raven_id))


# data date range
range(ws_model_data$date)


# proportion of days leaving territory
data %>% 
  filter(n_point >= 10) %>% 
  group_by(raven_id) %>% 
  summarize(prop_leave = sum(terr_bin)/n()) %>% 
  summarize(mean = mean(prop_leave),
            min = min(prop_leave),
            max = max(prop_leave),
            sd = sd(prop_leave))


# proportion of days off territory visiting hunting
data %>% 
  filter(n_point >= 10,
         terr_bin == TRUE) %>% 
  group_by(raven_id) %>% 
  summarize(prop_hunt = sum(hunt_bin)/n()) %>% 
  summarize(mean = mean(prop_hunt),
            min = min(prop_hunt),
            max = max(prop_hunt),
            sd = sd(prop_hunt))


# average home range size
source("scripts/home_range_mcp.R")
mean(mcp90@data$area[mcp90@data$id %in% data$raven_id])
median(mcp90@data$area[mcp90@data$id %in% data$raven_id])
range(mcp90@data$area[mcp90@data$id %in% data$raven_id])
sd(mcp90@data$area[mcp90@data$id %in% data$raven_id])


# average distance to hunting
ws_model_data %>% 
  # removing Old Faithful birds since they never go to Gardiner
  filter(!(raven_id %in% c(7485, 7494))) %>% 
  group_by(raven_id) %>% 
  slice(1) %>%
  ungroup %>% 
  summarize(mean = mean(dist2nentrance),
            median = median(dist2nentrance),
            max = max(dist2nentrance),
            min = min(dist2nentrance),
            sd = sd(dist2nentrance))


# percent of trips to Gardiner included visit to the dump
data %>% 
  filter(hunt_bin == TRUE) %>% 
  summarise(sum(dump)/n()) 
  

# average percent trip to hunting included a trip to the dump
data %>% 
  filter(hunt_bin == TRUE | dump == TRUE) %>% 
  group_by(raven_id) %>% 
  summarize(prop_visit_dump = sum(dump == TRUE)/n()) %>% 
  summarize(mean = mean(prop_visit_dump),
            min = min(prop_visit_dump),
            max = max(prop_visit_dump),
            sd = sd(prop_visit_dump))


# average percent trip to dump (hunting season)
data %>% 
  filter(
    # only during the hunting season
    hunt_season == TRUE,
    # only on trips to Gardiner
    hunt_bin == TRUE | dump == TRUE) %>% 
  group_by(raven_id) %>% 
  summarise(prop_visit_dump = visit/(visit + no_visit)) %>% 
  summarize(mean = mean(prop_visit_dump),
            min = min(prop_visit_dump),
            max = max(prop_visit_dump),
            sd = sd(prop_visit_dump))


# wolf kills available on territory
ws_model_data %>% 
  group_by(raven_id) %>% 
  summarize(available_kills = sum(rf_active_kill),
            total_days = n()) %>% 
  mutate(prop_available = available_kills/total_days) %>% 
  summarize(mean = mean(prop_available),
            min = min(prop_available),
            max = max(prop_available),
            sd = sd(prop_available))


# wolf kills visited outside of territory
hunt_model_data %>% 
  group_by(raven_id) %>% 
  summarize(visited_kills = sum(visit_kill),
            total_days = n()) %>% 
  mutate(prop_visited = visited_kills/total_days) %>% 
  ungroup %>% 
  summarize(mean = mean(prop_visited),
            min = min(prop_visited),
            max = max(prop_visited),
            sd = sd(prop_visited),
            days_visited = sum(visited_kills))


# how often ravens visited a kill outside territory, then visited the hunting
hunt_model_data %>% 
  filter(visit_kill == TRUE) %>% 
  group_by(raven_id) %>% 
  summarize(visit_hunt = sum(hunt_bin),
            visited_kills = n()) %>% 
  mutate(prop_hunt = visit_hunt/visited_kills) %>% 
  ungroup %>% 
  summarize(mean = mean(prop_hunt),
            median = median(prop_hunt),
            min = min(prop_hunt),
            max = max(prop_hunt),
            sd = sd(prop_hunt))


# daily proportion visiting Gardiner based on daily temperature
#' based on all decision days, not filtered modeling data
ws_model_data %>% 
  #grouping by date
  group_by(date) %>% 
  #calculating the proportion of trips went to the hunting area
  summarize(visit_hunt_prop = sum(hunt_bin)/n()) %>% 
  #adding the relevant plotting columns back into the summarized data
  right_join(ws_model_data %>% 
               dplyr::select(date, temp_max)) %>% 
  #plotting
  ggplot(aes(y = visit_hunt_prop, x = temp_max)) +
  geom_point() + 
  geom_smooth(method = "lm")


# individuals' Gardiner visits based on distance to hunting
#' based on all decision days, not filtered modeling data
ws_model_data %>% 
  #grouping by individual
  group_by(raven_id) %>% 
  #calculating the proportion of trips went to the hunting area
  summarize(visit_hunt_prop = sum(hunt_bin)/n()) %>% 
  #adding the relevant plotting columns back into the summarized data
  right_join(ws_model_data %>% 
               dplyr::select(raven_id, dist2nentrance)) %>% 
  #plotting
  ggplot(aes(y = visit_hunt_prop, x = dist2nentrance)) +
  geom_point() + 
  geom_smooth(method = "lm")


# individuals' Gardiner visits based on distance to hunting 
#' based on filtered modeling data, only if ravens decided to leave their territory
hunt_model_data %>% 
  #grouping by individual
  group_by(raven_id) %>% 
  #calculating the proportion of trips went to the hunting area
  summarize(visit_hunt_prop = sum(hunt_bin)/n()) %>% 
  #adding the relevant plotting columns back into the summarized data
  right_join(ws_model_data %>% 
               dplyr::select(raven_id, dist2nentrance)) %>% 
  #plotting
  ggplot(aes(y = visit_hunt_prop, x = dist2nentrance)) +
  geom_point() + 
  geom_smooth(method = "lm")


# range of group proportion decisions
#' proportion of ravens that left the hunting area
ws_model_data %>% 
  group_by(date) %>% 
  slice(1) %>% 
  ungroup %>% 
  pull(prop_group_left_terr) %>% 
  hist(main = "proportion of territorials that left territoriy")
#' based on part 2 model data, proportion of territorial ravens that visited hunting area
hunt_model_data %>% 
  group_by(date) %>% 
  slice(1) %>% 
  ungroup %>% 
  pull(prop_group_visit_hunt) %>% 
  hist(main = "proportion of territorials that visited hunting")


# stacked barplot (all winter) --------------------------------------------
# table showing raven decisions during winter studies
decision_table <- data %>% 
  group_by(raven_id) %>% 
  summarize(terr = sum(terr_bin == FALSE),
            other = sum(terr_bin == TRUE & hunt_bin == FALSE),
            hunt = sum(hunt_bin == TRUE)) %>% 
  # adding column for total sample size for each raven
  mutate(n = hunt + other + terr)

# stacked barplot showing raven decisions including wolf kill presence
# data from all winter
data %>% 
  group_by(raven_id) %>% 
  summarize(terr = sum(terr_bin == FALSE),
            other = sum(terr_bin == TRUE & hunt_bin == FALSE),
            hunt = sum(hunt_bin == TRUE)) %>% 
  # adding column for total sample size for each raven
  mutate(n = terr + other + hunt) %>% 
  # switching to long format
  tidyr::pivot_longer(cols = c(hunt, other, terr),
                      names_to = "decision") %>%  
  # setting plotting order
  mutate(decision = factor(decision, levels = rev(c("terr", "other", "hunt")))) %>% 
  # setting graphing data
  ggplot(aes(x = value, y = raven_id, fill = decision)) +
  # creating proportion stacked barplot
  geom_bar(position = "fill", stat = "identity",
                   colour = "black", linewidth = 0.2) +
  # changing labels of plot
  labs(title = "  ",
       x = "Proportion of days",
       y = "Raven ID",
       fill = "Decision") +
  # custom color/texture scheme
  scale_fill_manual(values = c(terr = "#E69F00",
                               other = "#A4D8F4",
                               hunt = "#0072B2"),
                    # changing name of legend items
                    labels = c("Hunting", "Other", "Territory")) +
  # removing pattern from fill legend and changing pattern legend background
  guides(fill = guide_legend(override.aes = list(pattern = "none")),
         pattern = guide_legend(override.aes = list(fill = "white"))) +
  # removing space between axis and barplot
  scale_x_continuous(expand = c(0, 0)) +
  # adding sample size to right axis
  geom_text(data = decision_table, aes(x = 1.01, y = raven_id, label = n),
            inherit.aes = FALSE, hjust = 0, size = 3) + 
  # adding label for sample size column
  annotate("text", x = 1, y = Inf, label = "Sample size",
           hjust = 0.3, vjust = -0.3, size = 4) +
  # adjusting plot axis to show the extra text
  coord_cartesian(xlim = c(0, 1.1), clip = "off") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))
ggsave("decision_barplot.tif", units = "in", width = 7, height = 6, device = "tiff", path = "figures")


# stacked barplot (WS) ----------------------------------------------------




# table showing raven decisions during winter studies
ws_decision_table <- ws_model_data %>% 
  group_by(raven_id) %>% 
  summarize(terr = sum(terr_bin == FALSE),
            other = sum(terr_bin == TRUE & hunt_bin == FALSE),
            hunt = sum(hunt_bin == TRUE)) %>% 
  # adding column for total sample size for each raven
  mutate(n = hunt + other + terr)

# stacked barplot showing raven decisions including wolf kill presence
# only during winter study
ws_model_data %>% 
  group_by(raven_id) %>% 
  summarize(terr_avail = sum(terr_bin == FALSE & rf_active_kill == TRUE & visit_500 == FALSE),
            terr_visit = sum(terr_bin == FALSE & visit_500 == TRUE),
            terr_nokill = sum(terr_bin == FALSE & rf_active_kill == FALSE),
            other_terr_avail= sum(terr_bin == TRUE & hunt_bin == FALSE & rf_active_kill == TRUE & visit_500 == FALSE),
            other_terr_visit = sum(terr_bin == TRUE & hunt_bin == FALSE & visit_500 == TRUE),
            other_visit_outside = sum(terr_bin == TRUE & hunt_bin == FALSE & visit_kill == TRUE & visit_500 == FALSE),
            other_nokill = sum(terr_bin == TRUE & hunt_bin == FALSE & visit_kill == FALSE & visit_500 == FALSE),
            hunt_terr_avail = sum(hunt_bin == TRUE & rf_active_kill == TRUE & visit_500 == FALSE) ,
            hunt_terr_visit = sum(hunt_bin == TRUE & visit_500 == TRUE),
            hunt_visit_outside = sum(hunt_bin == TRUE & visit_kill == TRUE & visit_500 == FALSE),
            hunt_nokill = sum(hunt_bin == TRUE & visit_kill == FALSE & visit_500 == FALSE)) %>% 
  # adding column for total sample size for each raven
  mutate(n = hunt_terr_avail + hunt_terr_visit + hunt_visit_outside + hunt_nokill + 
           other_terr_avail + other_terr_visit + other_visit_outside + other_nokill + 
           terr_avail + terr_visit + terr_nokill) %>% 
  # switching to long format
  tidyr::pivot_longer(cols = c(hunt_terr_avail, hunt_terr_visit, hunt_visit_outside, hunt_nokill, 
                                 other_terr_avail, other_terr_visit, other_visit_outside, other_nokill, 
                                 terr_avail, terr_visit, terr_nokill),
                      names_to = "decision") %>%  
  # setting plotting order
  mutate(decision = factor(decision, levels = rev(c("terr_nokill", "terr_avail", "terr_visit",
                                                   "other_nokill", "other_terr_avail", "other_terr_visit", "other_visit_outside",
                                                   "hunt_nokill", "hunt_terr_avail", "hunt_terr_visit", "hunt_visit_outside")))) %>% 
  # adding column for presence of kill
  mutate(kill = rep(c("terr_avail", "terr_visit", "visit_outside", "no_kill",
                      "terr_avail", "terr_visit", "visit_outside", "no_kill",
                      "terr_avail", "terr_visit", "no_kill"), 18)) %>% 
  # setting graphing data
  ggplot(aes(x = value, y = raven_id, fill = decision, pattern = kill)) +
  # creating proportion stacked barplot
  geom_bar_pattern(position = "fill", stat = "identity",
                   colour = "black", linewidth = 0.2,
                   pattern_fill = "black", pattern_color = "transparent",
                   pattern_size = 0.02, pattern_spacing = 0.015, pattern_angle = 50) +
  # changing labels of plot
  labs(title = "  ",
       x = "Proportion of days",
       y = "Raven ID",
       fill = "Decision") +
  scale_pattern_manual(values = c("terr_avail" = "stripe", 
                                  "terr_visit" = "crosshatch",
                                  "visit_outside" = "circle", 
                                  "no_kill" = "none"), 
                       name = "Wolf kill",
                       breaks = c("terr_avail", "terr_visit", "visit_outside"),
                       labels = c("Available in territory", "Visit in territory", "Visit out of territory")) +  
  # custom color/texture scheme
  scale_fill_manual(values = c(terr_avail = "#E69F00", terr_visit = "#E69F00", terr_nokill = "#E69F00",
                               other_terr_avail = "#A4D8F4",other_terr_visit = "#A4D8F4", other_visit_outside = "#A4D8F4", other_nokill = "#A4D8F4",
                               hunt_terr_avail = "#0072B2",hunt_terr_visit = "#0072B2", hunt_visit_outside = "#0072B2", hunt_nokill = "#0072B2"),
                    # removing repeats in legend
                    breaks = c("hunt_nokill", "other_nokill", "terr_nokill"),
                    # changing name of legend items
                    labels = c("Hunting", "Other", "Territory")) +
  # removing pattern from fill legend and changing pattern legend background
  guides(fill = guide_legend(override.aes = list(pattern = "none")),
         pattern = guide_legend(override.aes = list(fill = "white"))) +
  # removing space between axis and barplot
  scale_x_continuous(expand = c(0, 0)) +
  # adding sample size to right axis
  geom_text(data = ws_decision_table, aes(x = 1.01, y = raven_id, label = n),
            inherit.aes = FALSE, hjust = 0, size = 3) + 
  # adding label for sample size column
  annotate("text", x = 1, y = Inf, label = "Sample size",
           hjust = 0.3, vjust = -0.3, size = 4) +
  # adjusting plot axis to show the extra text
  coord_cartesian(xlim = c(0, 1.1), clip = "off") +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        legend.box.spacing = unit(0.1, "cm"),
        legend.margin = margin(0, 0, 0, 0))
ggsave("ws_decision_barplot.tif", units = "in", width = 8, height = 5.5, device = "tiff", path = "figures")


# decisions between months ------------------------------------------------
# getting commute_df
source("scripts/commute_decision.R")

# calculating the percentage of points in each commute category by individual and month
commute_month <- commute_df  %>% 
  # adding month column
  mutate(month = month(date),
         winter_year = if_else(month(date) %in% c(11,12), year(date), year(date) - 1)) %>% 
  # changing name of ID column
  rename(raven_id = individual_local_identifier) %>% 
  # removing days when there are < 10 GPS points
  filter(n_point >= 10) %>% 
  # getting proportion travel decisions by bird
  group_by(raven_id, month, winter_year) %>% 
  summarize(territory = sum(commute == 1)/n(),
            other = sum(commute == 2)/n(),
            hunting = sum(commute == 3)/n()) %>% 
  ungroup

# plotting the average proportion of each commute category by month
commute_month %>%
  filter(month != 8) %>% 
  # long data
  pivot_longer(cols = c(territory, other, hunting), 
               names_to = "commute", values_to = "prop") %>% 
  # creating averages per month with sd bars
  group_by(month, commute) %>% 
  summarize(mean_prop = mean(prop), 
            sd = sd(prop)) %>%
  ungroup %>% 
  # ordering months
  mutate(month = fct_relevel(as.character(month), c("8", "9", "10", "11", "12", "1", "2", "3"))) %>% 
  # plotting
  ggplot(aes(x = month, y = mean_prop, 
             group = commute, col = commute,
             ymin = mean_prop - sd, ymax = mean_prop + sd)) +
  geom_line(lwd = 1) +
  ggborderline::geom_borderline(lwd = 1.1, bordercolor = "black" ) +
  # changing labels
  labs(y = "Porportion of days",
       x = "Month",
       color = "Decision") +
  # setting aesthetic theme
  theme_classic() +
  # setting y limits
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  # getting x axis tick closer to 0
  scale_x_discrete(expand = expansion(add = c(0.2, 0.1))) +
  # custom color/texture scheme
  scale_color_manual(values = c(territory = "#E69F00",
                                other = "#A4D8F4",
                                hunting = "#0072B2"),
                     labels = c("Hunting", "Other", "Territory")) +
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14))
# saving plot so the lines are less pixelated
ggsave("monthly_decision.tif", units = "in", width = 7, height = 6, device = "tiff", path = "figures")



# calculating the proportion individuals making each commute decision by days in October
commute_day <- commute_df  %>% 
  # adding date columns
  mutate(day = day(date),
         month = factor(month(date), levels = c(8, 9, 10, 11, 12, 1, 2, 3)),
         year = year(date)) %>% 
  filter(month %in% c(8:12, 1:3)) %>% 
  # changing name of ID column
  rename(raven_id = individual_local_identifier) %>% 
  # removing days when there are < 10 GPS points
  filter(n_point >= 10) %>% 
  # getting proportion travel decisions by bird
  group_by(year, month, day) %>% 
  summarize(territory = sum(commute == 1)/n(),
            other = sum(commute == 2)/n(),
            hunting = sum(commute == 3)/n()) %>% 
  ungroup

# plotting the proportion of individual making a commute category by days in October
commute_day %>% 
  # only Sep-Nov
  filter(month %in% 9:12) %>% 
  # long data
  pivot_longer(cols = c(territory, other, hunting), 
               names_to = "commute", values_to = "prop") %>% 
  # creating averages per day with sd bars
  group_by(month, day, commute) %>% 
  summarize(mean_prop = mean(prop), 
            sd = sd(prop)) %>%
  ungroup %>% 
  # plotting
  ggplot(aes(x = day, y = mean_prop, 
             group = commute, col = commute,
             ymin = mean_prop - sd, ymax = mean_prop + sd)) +
  geom_line(lwd = 1) +
  # adding black border to line
  ggborderline::geom_borderline(lwd = 1.1, bordercolor = "black" ) +
  # separate plots by month
  facet_wrap(~month,
             # changing numbers ot names of months
             labeller = labeller(month = c("8" = "August",
                                           "9" = "September",
                                           "10" = "October",
                                           "11" = "November",
                                           "12" = "December",
                                           "1" = "January",
                                           "2" = "February",
                                           "3" = "March"))) + 
  # changing labels
  labs(y = "Porportion of ravens",
       x = "Day",
       color = "Decision") +
  # setting aesthetic theme
  theme_classic() +
  # setting y limits
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  # custom color/texture scheme
  scale_color_manual(values = c(territory = "#E69F00",
                                other = "#A4D8F4",
                                hunting = "#0072B2"),
                     name = "Decision",
                     labels = c("Hunting", "Other", "Territory")) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 15))
# saving plot so the lines are less pixelated
ggsave("daily_decision.tif", units = "in", width = 8, height = 6, device = "tiff", path = "figures")


# bison area only ---------------------------------------------------------

# calculating the percentage of points in each commute category by individual and month
commute_bison <- commute_df  %>% 
  # adding month column
  mutate(month = month(date),
         winter_year = if_else(month(date) %in% c(11,12), year(date), year(date) - 1)) %>% 
  # changing name of ID column
  rename(raven_id = individual_local_identifier) %>% 
  # removing days when there are < 10 GPS points
  filter(n_point >= 10) %>% 
  # getting proportion travel decisions by bird
  group_by(raven_id, month, winter_year) %>% 
  summarize(territory = sum(commute == 1)/n(),
            hunting = sum(bison_visit == TRUE)/n(),
            other = 1 - territory - hunting) %>% 
  ungroup

# plotting the average proportion of each commute category by month
commute_bison %>% 
  # long data
  pivot_longer(cols = c(territory, other, hunting), 
               names_to = "commute", values_to = "prop") %>% 
  # creating averages per month with sd bars
  group_by(month, commute) %>% 
  summarize(mean_prop = mean(prop), 
            sd = sd(prop)) %>%
  ungroup %>% 
  # ordering months
  mutate(month = fct_relevel(as.character(month), c("8", "9", "10", "11", "12", "1", "2", "3"))) %>% 
  # plotting
  ggplot(aes(x = month, y = mean_prop, 
             group = commute, col = commute,
             ymin = mean_prop - sd, ymax = mean_prop + sd)) +
  geom_line(lwd = 1) +
  ggborderline::geom_borderline(lwd = 1.1, bordercolor = "black" ) +
  # changing labels
  labs(y = "Porportion of days",
       x = "Month",
       color = "Decision") +
  # setting aesthetic theme
  theme_classic() +
  # setting y limits
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  # custom color/texture scheme
  scale_color_manual(values = c(territory = "#E69F00",
                                other = "#A4D8F4",
                                hunting = "#0072B2"),
                     labels = c("Bison area", "Other", "Territory")) 
ggsave("monthly_bison.tif", units = "in", width = 8, height = 6, device = "tiff", path = "figures")



# dump visits -------------------------------------------------------------
# proportion of days visiting the dump
dump_visits <- data %>% 
  # filter to days visiting hunting
  filter(hunt_bin == TRUE,
         month != 8) %>% 
  # creating bins of time based on hunting periods
  mutate(hseason_blocks = factor(if_else(hunt_season == F & month %in% 8:10, "Pre-hunt", # all days before MTFWP season
                                  if_else(hunt_season == F, "Mid-winter", # all other days not during hunting season, which is the mid winter break
                                          if_else(hunt_season == T & month %in% 10:12, "MTFWP hunt", # all days during MTFWP season
                                                  if_else(hunt_season == T, "Bison hunt", NA)))),
                                 levels = c("Pre-hunt", "MTFWP hunt", "Mid-winter", "Bison hunt"))) %>% # all other hunting season days, wihch is the bison hunt
  # getting monthly proportion by raven
  group_by(raven_id, hseason_blocks) %>% 
  summarize(dump_visit = sum(dump),
            n_days = n(),
            prop_dump = sum(dump)/n()) %>% 
  ungroup

dump_visits %>% 
  # plotting
  ggplot(aes(x = hseason_blocks, y = prop_dump)) +
  geom_boxplot() +
  # axis labels
  labs(x = "",
       y = "Proportion of days visiting dump") +
  theme_classic()
ggsave("dump_visits.tif", units = "in", width = 6, height = 4, device = "tiff", path = "figures")


