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
  # only columns used in model
  dplyr::select(terr_bin, raven_id, dump, date, rf_active_kill, final_take_bms, final_take_bms1, final_take, 
                hunt_season, rf_avg_terr_kill_density, dist2nentrance, 
                study_period, temp_max, snow_depth, prop_group_left_terr) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 


## dataset for part 2 of conditional model
hunt_model_data <- readr::read_csv("data/clean/commute_data.csv") %>%
  # only have days ravens decided to leave territory
  filter(terr_bin == 1) %>% 
  # removing days when there is less than 5 GPS point
  # unless the result is Jardine
  filter(!(n_point < 5 & hunt_bin == F)) %>% 
  # only columns used in model
  dplyr::select(hunt_bin, raven_id, dump, date, final_take_bms, final_take, hunt_season,
                dist2nentrance, study_period, temp_max, snow_depth, prop_group_visit_hunt) %>% 
  # making sure rows are complete
  filter(complete.cases(.)) 

## all data with filters
  # combines both data sets and removes duplicates
all <- ws_model_data %>% 
  bind_rows(hunt_model_data) %>% 
  group_by(raven_id, date) %>% 
  slice(1) %>% 
  ungroup
  

# summary data ------------------------------------------------------------

# total number of ravens included in study
length(unique(all$raven_id))


# decision days
nrow(all) #total days
mean(table(all$raven_id))
sd(table(all$raven_id))
max(table(all$raven_id))
min(table(all$raven_id))


# data date range
range(all$date)


# number of GPS points per day
hist(all$n_point, 
     breaks = 1:max(all$n_point))
mean(all$n_point)
sd(all$n_point)


# average home range size
source("scripts/home_range_mcp.R")
mean(mcp90@data$area)
sd(mcp90@data$area)

# percent of trips to Gardiner included visit to the dump (all winter)
all %>% 
  filter(hunt_bin == TRUE) %>% 
  group_by(dump) %>% 
  summarise(n())
474/(474+913)
# 34.17%

# average percent trip to dump (all winter)
all_dump <- all %>% 
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
all %>% 
  filter(
    #only during the hunitng season
    hunt_season == TRUE,
    #only on trips to Gardiner
    hunt_bin == TRUE) %>% 
  group_by(dump) %>% 
  summarise(n())
264/(264+710)
# 27.1%

# average percent trip to dump (hunting season)
hunt_dump <- all %>% 
  filter(
    #only during the hunitng season
    hunt_season == TRUE,
    #only on trips to Gardiner
    hunt_bin == TRUE) %>% 
  group_by(raven_id) %>% 
  summarise(no_visit = sum(dump == F),
            visit = sum(dump == T)) %>% 
  mutate(prop_visit_dump = visit/(visit + no_visit),
         season = "hunt")
hunt_dump %>% 
  summarize(mean = mean(prop_visit_dump),
            min = min(prop_visit_dump),
            max = max(prop_visit_dump),
            sd = sd(prop_visit_dump))



# daily proportion visiting Gardiner based on daily temperature
#' based on all decision days, not filtered modeling data
all %>% 
  #grouping by date
  group_by(date) %>% 
  #calculating the proportion of trips went to the hunting area
  summarize(visit_hunt_prop = sum(hunt_bin)/n()) %>% 
  #adding the relevant plotting columns back into the summarized data
  right_join(all %>% 
               dplyr::select(date, temp_max)) %>% 
  #plotting
  ggplot(aes(y = visit_hunt_prop, x = temp_max)) +
  geom_point() + 
  geom_smooth(method = "lm")


# individuals' Gardiner visits based on distance to hunting
 #' based on all decision days, not filtered modeling data
all %>% 
  #grouping by individual
  group_by(raven_id) %>% 
  #calculating the proportion of trips went to the hunting area
  summarize(visit_hunt_prop = sum(hunt_bin)/n()) %>% 
  #adding the relevant plotting columns back into the summarized data
  right_join(all %>% 
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
  right_join(all %>% 
               dplyr::select(raven_id, dist2nentrance)) %>% 
  #plotting
  ggplot(aes(y = visit_hunt_prop, x = dist2nentrance)) +
  geom_point() + 
  geom_smooth(method = "lm")


# range of group proportion decisions
  #' proportion of ravens that left the hunting area
  all %>% 
    group_by(date) %>% 
    slice(1) %>% 
    ungroup %>% 
    pull(prop_group_left_terr) %>% 
    hist(main = "proportion of territorials that left territoriy")
  #' based on part 2 model data, proportion of territorial ravens that visited hunting area
  all %>% 
    group_by(date) %>% 
    slice(1) %>% 
    ungroup %>% 
    pull(prop_group_visit_hunt) %>% 
    hist(main = "proportion of territorials that visited hunting")
  
  
# table showing raven decisions
  (decision_table <- all %>% 
    group_by(raven_id) %>% 
    summarize(terr = sum(terr_bin == FALSE),
              other = sum(terr_bin == TRUE & hunt_bin == FALSE),
              hunt = sum(hunt_bin == TRUE)) %>% 
  mutate(n = hunt + other + terr))
  
# stacked barplot showing raven decisions
  decision_table %>% 
    # switching to long format
    pivot_longer(cols = c(hunt, other, terr),
                 names_to = "decision") %>% 
    # plotting proportion stacked barchart
    ggplot(aes(x = value, y = raven_id, fill = decision)) +
    geom_bar(position = "fill", stat = "identity") +
    labs(title = "Raven movement decisions",
         x = "Proportion",
         y = "Raven ID",
         fill = "Movement\ndecision") +
    scale_x_continuous(expand = c(0, 0)) +
    # changing name of legend items
    scale_fill_discrete(labels = c("hunting", "other", "territory")) + 
    geom_text(data = decision_table, aes(x = 1.01, y = raven_id, label = n),
              inherit.aes = FALSE, hjust = 0, size = 3) + 
    # adding label for sample size column
    annotate("text", x = 1, y = Inf, label = "Sample size",
             hjust = 0, vjust = -0.3, size = 3) +
    # adjusting plot axis to show the extra text
    coord_cartesian(xlim = c(0, 1.1), clip = "off") +
    theme_classic()

