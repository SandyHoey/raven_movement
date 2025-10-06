#Combining the elk migration timing with the MTFWP hunting estimates
 #so the estimated carcasses per day isn't uniform across the hunting season
 #since hunter success is dependent on availability

### !!!!!!!!!!!!!!!!! ###
# Current issue with not knowing any information about the breakdown of bow hunting by sex
# can't tell how many of the female/male mule deer were harvested by bow vs rifle
# so don't know how to sum with white tail to account for shorter buck season
# !! currently ignoring bow hunting since its relatively uncommon !!
### !!!!!!!!!!!!!!!! ###

library(dplyr)

#reading in elk estimates
elk_harvest_estimate <- readr::read_csv("data/raw/fwp_elk_harvest_estimate.csv") %>% 
  
  #cleaning column names
  janitor::clean_names() %>% 
  
  #only keeping sums instead of residential status rows
  filter(residency == "SUM") %>% 
  
  #removing unnecessary columns
  dplyr::select(license_year, total_harvest, bulls, cows, calves, rifle)



#reading in deer estimates
deer_harvest_estimate <- readr::read_csv("data/raw/fwp_deer_harvest_estimate.csv") %>% 
  
  #cleaning column names
  janitor::clean_names() %>% 
  
  #only keeping sums instead of residential status rows
  filter(residency == "SUM") %>% 
  
  #removing all_deer species rows
  filter(deer_species != "all_deer") %>% 
  
  #removing unnecessary columns
  dplyr::select(license_year, deer_species, total_harvest, bucks, does, fawns, rifle)


#pulling out mule deer
mule <- deer_harvest_estimate %>% 
  filter(deer_species == "md")


#calculating the daily take number of all ungulates
#taking the total number of elk and female deer and dividing by days of hunting season
#buck deer are protected in
deer_harvest_estimate <- deer_harvest_estimate %>% 
  
  #pulling out wt
  filter(deer_species == "wt") %>% 
  
  #updating columns to add the mule does and fawns
  mutate(does = does + mule$does,
         fawns = fawns + mule$fawns,
         total_harvest = bucks + does + fawns) %>% 
  
  #renaming bucks to wt_buck for clarity
  rename(wt_bucks = bucks) %>% 
  
  #adding mule bucks
  mutate(md_bucks = mule$bucks)


# calculating daily take --------------------------------------------------

#reading in hunting season dates
hunting_dates <- readxl::read_xlsx("data/raw/hunting_seasons.xlsx")

#checking the duration of hunting seasons
hunting_dates <- hunting_dates %>% 
  
  #adding number of days
  mutate(general_length = difftime(end, start, unit = "days"),
         md_buck_length = difftime(md_end, start, unit = "days"))


#calculating daily take
elk_harvest_estimate <- elk_harvest_estimate %>% 
  mutate(elk_daily_take = total_harvest/36)

deer_harvest_estimate <- deer_harvest_estimate %>% 
  mutate(deer_daily_take = total_harvest/36,
         md_buck_daily = md_bucks/22)


# applying elk migration timing to take -----------------------------------

#reading in elk migraiton timing
source("scripts/elk_migration_timing.R")

#applying daily take to november elk movement
nov_count <- nov_count %>% 
  
  #adding elk take
  left_join(elk_harvest_estimate %>% 
              dplyr::select(license_year, elk_daily_take),
            by = join_by(year == license_year)) %>% 
  
  #adding deer take
  left_join(deer_harvest_estimate %>% 
              dplyr::select(license_year, deer_daily_take, md_buck_daily),
            by = join_by(year == license_year)) %>% 
  
  #adding hunting season dates
  left_join(hunting_dates %>% 
              dplyr::select(1:4),
            by = join_by(year))
  
