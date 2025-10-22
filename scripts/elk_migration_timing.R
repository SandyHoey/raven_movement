#tracking the dates that elk start appearing in the Jardine hunting area

library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)


# prepping data -----------------------------------------------------------

#reading in data
elk_all_gps <- readr::read_csv("data/raw/elk_all_gps_2025-09-03_BJS.csv") %>% 
  
  #subsetting to only including study years
  filter(year(dt) >= 2019) %>%
  
  #transforming to sf dataframe
  st_as_sf(coords=c("utm_e", "utm_n"), 
           crs="+proj=utm +zone=12") %>% 
  
  #only hunting months
  filter(month(dt) %in% c(10, 11, 12))


#reading in Gardiner/Jardine and YNP kml
#transforming latlong to UTM to match the GPS points
# jardine_poly <- st_read("data/raw/gardiner_hunt.kml") %>% 
#   st_transform(crs = st_crs(elk_all_gps))
park_poly <- st_read("data/raw/parkpoly.kml") %>% 
  st_transform(crs = st_crs(elk_all_gps))

#dataframe with only GPS points in Jardine
# elk_in_jardine <- st_intersection(jardine_poly, elk_all_gps) %>% 
#   select(-c(1, 2)) %>% 
#   
#   #chronological order
#   arrange(dt) %>% 
#   
#   #removing sf geometry
#   st_drop_geometry()


#dataframe with only GPS points outside YNP
elk_outside_ynp <- elk_all_gps %>% 
  
  #calculating distance to YNP
  st_distance(park_poly, elk_all_gps) %>% 
  as.vector %>% 

  #adding back to elk data as a column
  bind_cols(elk_all_gps) %>% 
  rename(distance_ynp = ...1) %>% 
  
  #filtering to only distances > 0 (not inside the park)
  filter(distance_ynp > 0) %>% 
  
  #chronological order
  arrange(dt) %>% 
  
  #removing sf geometry
  st_drop_geometry()


# number of collared elk each month in Jardine ---------------------------
# monthly_count <- elk_outside_ynp %>%
# 
#   
#   #adding year and month columns
#   mutate(year = year(dt),
#          month = month(dt)) %>%
#   group_by(year, month) %>%
#   
#   #summarize data
#   summarise(outside_park = n_distinct(ID), .groups = "drop")
# 
# 
# #adding the total sample size each month
# monthly_count <- elk_all_gps %>% 
#   
#   #removing sf geometry
#   st_drop_geometry() %>% 
#   
#   #adding year and month columns
#   mutate(year = year(dt),
#          month = month(dt)) %>%
#   group_by(year, month) %>%
#   
#   #summarize data
#   summarise(total_ind = n_distinct(ID), .groups = "drop") %>% 
#   
#   #merging with monthly count
#   right_join(monthly_count, by = c("year" = "year", "month" = "month")) %>% 
#   
#   #adding column with proportion
#   mutate(prop_available = outside_park/total_ind) %>% 
#   
#   #adding column with combined year-month
#   mutate(ym = paste(year, month, sep="-"))
# 
# 
# #average by month
# monthly_count %>% 
#   group_by(month) %>% 
#   summarise(mean(prop_available))
# boxplot(prop_available ~ month, data = monthly_count)
# 
# #plotting 
# boxplot(prop_available ~ ym, data = monthly_count)


# number of collared elk daily during hunting season in Jardine ---------------------------

#reading in hunting season start and end dates
hunting_dates <- readxl::read_xlsx("data/raw/hunting_seasons.xlsx")


daily_count <- elk_outside_ynp %>%
  
  #adding year, month, and day columns
  mutate(year = year(dt),
         month = month(dt),
         day = day(dt),
         date = as.Date(dt)) %>%
  
  #adding hunting season dates
  left_join(hunting_dates %>% 
              dplyr::select(year, start, end),
            by = join_by(year)) %>% 
  
  #filtering to only hunting season
    #creating new boolean column for hunting season
    mutate(hunt = if_else((format(date, "%m-%d") >= 
                            format(start, "%m-%d")) &
                          (format(date, "%m-%d") <= 
                             format(end, "%m-%d")), 
                          1, #days in nov before end date
                          0)) %>%  #otherwise no hunting == 0 
    filter(hunt == 1) %>% 
  
  #summarize data
  group_by(year, month, day) %>%
  summarise(outside_park = n_distinct(ID), .groups = "drop")

  
#fixing an issue with the hunting season of 2024 not having any data some days
#since no elk were out of the park
daily_count <- data.frame(year = 2024, #missing days in October
           month = 10,
           day = c(26:29),
           outside_park = 0) %>% 
  bind_rows(data.frame(year = 2024, #missing days in November
                       month = 11,
                       day = 3,
                       outside_park = 0)) %>% 
  bind_rows(daily_count) %>% 
  arrange(year, month, day)


#adding the total sample size every day
daily_count <- elk_all_gps %>% 
  
  #removing sf geometry
  st_drop_geometry() %>% 
  
  #only hunting months
  filter(month(dt) %in% c(10, 11, 12)) %>% 
  
  #adding year and month columns
  mutate(year = year(dt),
         month = month(dt),
         day = day(dt)) %>%
  group_by(year, month, day) %>%
  
  #summarize data
  summarise(total_ind = n_distinct(ID), .groups = "drop") %>% 
  
  #merging with monthly count
  right_join(daily_count, by = join_by(year, month, day)) %>% 
  
  #adding columns 
  mutate(
    #with proportion
    prop_available = outside_park/total_ind,
    
    #adding month-day
    md = paste(month, day, sep = "-")) 



#average by day
# daily_count %>% 
#   group_by(md) %>% 
#   summarise(mean(prop_available))
# boxplot(prop_available ~ md, data = daily_count)


#plotting splitting years
daily_count %>% 
  ggplot(aes(x = as.Date(paste(2000, md, sep = "-")), y = prop_available, 
             group = year, col = factor(year))) +
  geom_line()

