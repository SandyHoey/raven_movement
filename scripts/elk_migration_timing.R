#tracking the dates that elk start appearing in the Jardine hunting area

library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)


# prepping data -----------------------------------------------------------

#reading in data
elk_data <- readr::read_csv("data/raw/elk_GPS_2025-09-03_BJS.csv") %>% 
  
  #subsetting to only including study years
  filter(year(dt) >= 2019) %>%
  
  #transforming to sf dataframe
  st_as_sf(coords=c("utm_e", "utm_n"), 
           crs="+proj=utm +zone=12") %>% 
  
  #only hunting months
  filter(month(dt) %in% c(10, 11, 12))


#reading in Gardiner/Jardine and YNP kml
#transforming latlong to UTM to match the GPS points
jardine_poly <- st_read("data/raw/gardiner_hunt.kml") %>% 
  st_transform(crs = st_crs(elk_data))
park_poly <- st_read("data/raw/parkpoly.kml") %>% 
  st_transform(crs = st_crs(elk_data))

#dataframe with only GPS points in Jardine
elk_jardine <- st_intersection(jardine_poly, elk_data) %>% 
  select(-c(1, 2)) %>% 
  
  #chronological order
  arrange(dt) %>% 
  
  #removing sf geometry
  st_drop_geometry()


#dataframe with only GPS points outside YNP
elk_ynp <- elk_data %>% 
  
  #calculating distance to YNP
  st_distance(park_poly, elk_data) %>% 
  as.vector %>% 

  #adding back to elk data as a column
  bind_cols(elk_data) %>% 
  rename(distance_ynp = ...1) %>% 
  
  #filtering to only distances > 0 (not inside the park)
  filter(distance_ynp > 0) %>% 
  
  #chronological order
  arrange(dt) %>% 
  
  #removing sf geometry
  st_drop_geometry()


# number of collared elk each month in Jardine ---------------------------
monthly_count <- elk_ynp %>%

  
  #adding year and month columns
  mutate(year = year(dt),
         month = month(dt)) %>%
  group_by(year, month) %>%
  
  #summarize data
  summarise(outside_park = n_distinct(ID), .groups = "drop")


#adding the total sample size each month
monthly_count <- elk_data %>% 
  
  #removing sf geometry
  st_drop_geometry() %>% 
  
  #adding year and month columns
  mutate(year = year(dt),
         month = month(dt)) %>%
  group_by(year, month) %>%
  
  #summarize data
  summarise(total_ind = n_distinct(ID), .groups = "drop") %>% 
  
  #merging with monthly count
  right_join(monthly_count, by = c("year" = "year", "month" = "month")) %>% 
  
  #adding column with proportion
  mutate(prop_available = outside_park/total_ind) %>% 
  
  #adding column with combined year-month
  mutate(ym = paste(year, month, sep="-"))


#average by month
monthly_count %>% 
  group_by(month) %>% 
  summarise(mean(prop_available))
boxplot(prop_available ~ month, data = monthly_count)

#plotting 
boxplot(prop_available ~ ym, data = monthly_count)


# number of collared elk daily during November in Jardine ---------------------------
nov_count <- elk_ynp %>%
  
  #only november
  filter(month(dt) == 11) %>% 
  
  #adding year and month columns
  mutate(year = year(dt),
         day = day(dt)) %>%
  group_by(year, day) %>%
  
  #summarize data
  summarise(outside_park = n_distinct(ID), .groups = "drop")


#adding the total sample size every day
nov_count <- elk_data %>% 
  
  #removing sf geometry
  st_drop_geometry() %>% 
  
  #only november
  filter(month(dt) == 11) %>% 
  
  #adding year and month columns
  mutate(year = year(dt),
         day = day(dt)) %>%
  group_by(year, day) %>%
  
  #summarize data
  summarise(total_ind = n_distinct(ID), .groups = "drop") %>% 
  
  #merging with monthly count
  right_join(nov_count, by = c("year" = "year", "day" = "day")) %>% 
  
  #adding column with proportion
  mutate(prop_available = outside_park/total_ind) %>% 
  
  #adding column with combined year-month
  mutate(yd = paste(year, day, sep="-"))


#average by day
nov_count %>% 
  group_by(day) %>% 
  summarise(mean(prop_available))
boxplot(prop_available ~ day, data = nov_count)


#plotting splitting years
nov_count %>% 
  ggplot(aes(x = day, y = prop_available, 
             group = year, col = factor(year))) +
  geom_line()

