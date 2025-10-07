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
#36 days is the rifle hunting season length every year
#22 days is the mule deer buck hunting season every year
elk_harvest_estimate <- elk_harvest_estimate %>% 
  rename(elk_harvest = total_harvest) %>% 
  mutate(elk_daily_take = elk_harvest/36)

deer_harvest_estimate <- deer_harvest_estimate %>%
  rename(deer_harvest = total_harvest) %>% 
  mutate(deer_daily_take = deer_harvest/36,
         md_buck_daily = md_bucks/22)


# applying elk migration timing to take -----------------------------------

#reading in elk migraiton timing
source("scripts/elk_migration_timing.R")

#applying daily take to november elk movement
nov_count <- nov_count %>% 
  
  #adding elk take
  left_join(elk_harvest_estimate %>% 
              dplyr::select(license_year, elk_daily_take, 
                            elk_harvest),
            by = join_by(year == license_year)) %>% 
  
  #adding deer take
  left_join(deer_harvest_estimate %>% 
              dplyr::select(license_year, deer_daily_take, md_buck_daily, 
                            deer_harvest, md_bucks),
            by = join_by(year == license_year)) %>% 
  
  #adding hunting season dates
  left_join(hunting_dates %>% 
              dplyr::select(1:4),
            by = join_by(year))
  


# calculating weighted take values ----------------------------------------
nov_count <- nov_count %>% 
  
  #shifting prop_available by the lowest value in each year
  #so the take values follow the same trend when multiplied
  group_by(year) %>%
  mutate(prop_shift = prop_available - min(prop_available)) %>% 
  ungroup %>% 
  
  # daily take number * proportion of elk available (shifted)
  mutate(wt_elk_take = elk_daily_take * prop_shift,
         wt_deer_take = deer_daily_take * prop_shift,
         wt_md_buck_take = md_buck_daily * prop_shift)
  

#calculating the total take given by weighted take values
wt_take_est <- nov_count %>% 
  group_by(year) %>% 
  summarize(wt_elk_est = sum(wt_elk_take),
            wt_deer_est = sum(wt_deer_take),
            wt_md_buck_est = sum(wt_md_buck_take)) %>% 
  
  #adding the total take numbers
  left_join(nov_count %>% 
              dplyr::select(year, elk_harvest, deer_harvest, md_bucks) %>% 
              group_by(year) %>% 
              slice(1),
            by = join_by(year)) %>% 
  
  #calculating the amount to adjust the weighted take values to meet the total harvest numbers
  #subtracting the weighted take from the total harvest
    #dividing the difference by number of hunt days
  mutate(adj_elk_est = (elk_harvest - wt_elk_est)/36,
         adj_deer_est = (deer_harvest - wt_deer_est)/36,
         adj_md_buck_est = (md_bucks - wt_md_buck_est)/22)

#adding harvest adjustment back to the weighted take values
nov_count <- nov_count %>% 
  
  #adding the adjustment number
  left_join(wt_take_est %>% 
              dplyr::select(year, adj_elk_est, adj_deer_est, adj_md_buck_est),
            by = join_by(year)) %>% 
  
  #adjusting weighted take values to match total harvest number and account for prey weight
  mutate(final_elk_take = wt_elk_take + adj_elk_est,
         final_deer_take = wt_deer_take + adj_deer_est,
         final_md_buck_take = wt_md_buck_take + adj_md_buck_est,
         
         #going to be using bison weight as a baseline
         #elk are .5x of bison
         #deer are .15x of bison
         final_elk_bms = wt_elk_take + adj_elk_est * .5,
         final_deer_bms = wt_deer_take + adj_deer_est * .15,
         final_md_buck_bms = wt_md_buck_take + adj_md_buck_est * .15)


#adjusting the mule deer take numbers to reflect the end of their hunting season
nov_count <- nov_count %>% 
  group_by(year) %>% 
  
  #changing final count to 0 if after mule buck season
  mutate(final_md_buck_take = if_else(day > day(md_end), 0, final_md_buck_take))
  


#applying biomass adjustment for each species



#double checking yearly take numbers
nov_count %>% 
  group_by(year) %>% 
  summarize(elk = sum(final_elk_take),
            deer = sum(final_deer_take),
            md_bucks = sum(final_md_buck_take))
#the take number doesnt add up to the total amount because I am only using november
#the hunting season always starts in october


#creating a single column with the combined take numbers
nov_count <- nov_count %>% 
  mutate(final_take = final_elk_take + final_deer_take + final_md_buck_take,
         final_bms = final_elk_bms + final_deer_bms + final_md_buck_bms)


#plotting take numbers
nov_count %>% 
  ggplot(aes(x = day, y = final_take, 
             group = year, col = factor(year))) +
  geom_line()

#plotting biomass numbers
nov_count %>% 
  ggplot(aes(x = day, y = final_bms, 
             group = year, col = factor(year))) +
  geom_line()
