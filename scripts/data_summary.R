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

#total number of ravens included in study
length(unique(all$raven_id))


#decision days
nrow(all) #total days
mean(table(all$raven_id))
sd(table(all$raven_id))
max(table(all$raven_id))
min(table(all$raven_id))


#data date range
range(all$date)


#number of GPS points per day
hist(all$n_point, 
     breaks = 1:max(all$n_point))
mean(all$n_point)
sd(all$n_point)


#average home range size
source("scripts/home_range_mcp.R")
mean(mcp90@data$area)
sd(mcp90@data$area)


#movement decisions based on distance to hunting
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


#movement decisions based on distance to hunting 
  #' based filtered modeling data, only if ravens decided to leave their territory
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
