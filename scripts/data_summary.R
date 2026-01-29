#gets basic statistics about the data for reporting in the beginning of the results

library(dplyr)
library(ggplot2)


# reading in data ---------------------------------------------------------

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
  # scaling continuous variables
  mutate(final_take_bms = scale(final_take_bms), final_take_bms1 = scale(final_take_bms1),
         final_take = scale(final_take), dist2nentrance = scale(dist2nentrance), 
         temp_max = scale(temp_max), snow_depth = scale(snow_depth), 
         prop_group_visit_hunt = scale(prop_group_visit_hunt)) %>% 
  # making sure rows are complete
  filter(complete.cases(.))


# summary data ------------------------------------------------------------

# total number of ravens included in study
length(unique(ws_model_data$raven_id))


# decision days
nrow(all) #total days
mean(table(ws_model_data$raven_id))
sd(table(ws_model_data$raven_id))
max(table(ws_model_data$raven_id))
min(table(ws_model_data$raven_id))


# data date range
range(ws_model_data$date)


# number of GPS points per day
hist(ws_model_data$n_point, 
     breaks = 1:max(ws_model_data$n_point))
mean(ws_model_data$n_point)
sd(ws_model_data$n_point)


# average home range size
source("scripts/home_range_mcp.R")
mean(mcp90@data$area)
range(mcp90@data$area)
sd(mcp90@data$area)


# wolf kills available on territory
kills_available <- ws_model_data %>% 
  group_by(raven_id) %>% 
  summarize(available_kills = sum(rf_active_kill),
            total_days = n()) %>% 
  mutate(prop_available = available_kills/total_days)
mean(kills_available$prop_available)
range(kills_available$prop_available)
sd(kills_available$prop_available)


# percent of trips to Gardiner included visit to the dump
ws_model_data %>% 
  filter(hunt_bin == TRUE) %>% 
  group_by(dump) %>% 
  summarise(n())
161/(161+845)
# 16%


# average percent trip to dump
all_dump <- ws_model_data %>% 
  filter(hunt_bin == TRUE) %>% 
  group_by(raven_id) %>% 
  summarise(no_visit = sum(dump == F),
            visit = sum(dump == T)) %>% 
  mutate(prop_visit_dump = visit/(visit + no_visit))
all_dump %>% 
  summarize(mean = mean(prop_visit_dump),
            min = min(prop_visit_dump),
            max = max(prop_visit_dump),
            sd = sd(prop_visit_dump))


# percent of trips to Gardiner included visit to the dump (hunting seasons)
ws_model_data %>% 
  filter(
    # only during the hunting season
    hunt_season == TRUE,
    # only on trips to Gardiner
    hunt_bin == TRUE) %>% 
  group_by(dump) %>% 
  summarise(n())
264/(264+710)
# 27.1%


# average percent trip to dump (hunting season)
ws_model_data %>% 
  filter(
    # only during the hunting season
    hunt_season == TRUE,
    # only on trips to Gardiner
    hunt_bin == TRUE) %>% 
  group_by(raven_id) %>% 
  summarise(no_visit = sum(dump == F),
            visit = sum(dump == T)) %>% 
  mutate(prop_visit_dump = visit/(visit + no_visit),
         season = "hunt") %>% 
  summarize(mean = mean(prop_visit_dump),
            min = min(prop_visit_dump),
            max = max(prop_visit_dump),
            sd = sd(prop_visit_dump))



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


# table showing raven decisions
(decision_table <- ws_model_data %>% 
    group_by(raven_id) %>% 
    summarize(terr = sum(terr_bin == FALSE),
              other = sum(terr_bin == TRUE & hunt_bin == FALSE),
              hunt = sum(hunt_bin == TRUE)) %>% 
    # adding column for total sample size for each raven
    mutate(n = hunt + other + terr))

# stacked barplot showing raven decisions
decision_table %>% 
  # switching to long format
  pivot_longer(cols = c(hunt, other, terr),
               names_to = "decision") %>% 
  # setting graphing data
  ggplot(aes(x = value, y = raven_id, fill = decision)) +
  # creating proportion stacked barplot
  geom_bar(position = "fill", stat = "identity",
           colour = "black", linewidth = 0.2) +
  # changing labels of plot
  labs(title = "Raven movement decisions",
       x = "Proportion",
       y = "Raven ID",
       fill = "Movement\ndecision") +
  # custom color scheme
  scale_fill_manual(values = c(terr = "#e7e1ef", 
                               other = "#c994c7", 
                               hunt = "#dd1c77"),
                    # changing name of legend items
                    labels = c("hunting", "other", "territory")) +
  # removing space between axis and barplot
  scale_x_continuous(expand = c(0, 0)) +
  # adding sample size to right axis
  geom_text(data = decision_table, aes(x = 1.01, y = raven_id, label = n),
            inherit.aes = FALSE, hjust = 0, size = 3) + 
  # adding label for sample size column
  annotate("text", x = 1, y = Inf, label = "Sample size",
           hjust = 0, vjust = -0.3, size = 3) +
  # adjusting plot axis to show the extra text
  coord_cartesian(xlim = c(0, 1.1), clip = "off") +
  theme_classic()

