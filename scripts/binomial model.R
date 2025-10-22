#modeling the impacts of food availability on raven winter movements decisions

library(lme4)
library(DHARMa)


#3 = has a least 1 point that day in Gardiner poly
#2 = has at least 1 point farther than 1 km from terr poly , but none in Gardiner poly
#1 = only has points in terr poly


#restricting to only winter study periods
model_data <- readr::read_csv("data/clean/commute_data.csv") %>% 
  
  #restricting to only winter study months
  filter(month %in% c(11, 12, 3),
         !(month %in% c(11, 12) & is.na(bms_window_1)))


