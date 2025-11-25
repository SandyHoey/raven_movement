#gets basic statistics about the data for reporting in the beginning of the results

library(dplyr)
library(ggplot2)


# reading in data ---------------------------------------------------------

#all data without filters
all <- readr::read_csv("data/clean/commute_data.csv")

## dataset for part 1 of conditional model
ws_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  
  #restricting to only winter study months
  filter((paste(month, day, sep = "-") >= "11-15" &
            paste(month, day, sep = "-") <= "12-15") |
           (paste(month, day, sep = "-") >= "3-1" &
              paste(month, day, sep = "-") <= "3-30")) %>% 
  
  #making sure rows are complete
  filter(
    #previous_day history
    !is.na(previous_decision_terr)) %>% 
  
  #removing days when there is less than 5 GPS point
  #unless the result is Jardine
  filter(!(n_point < 5 & terr_bin == F))


## dataset for part 2 of conditional model
hunt_model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  
  #making sure rows are complete
  filter(
    #previous_day history
    !is.na(previous_decision_terr),
    #temperature
    !is.na(temp_max)) %>%
  
  #only have days ravens decided to leave territory
  filter(terr_bin == 1) %>% 
  
  #removing days when there is less than 5 GPS point
  #unless the result is Jardine
  filter(!(n_point < 5 & hunt_bin == F))


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

