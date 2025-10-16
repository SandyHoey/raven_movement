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
#37 days is the rifle hunting season length every year
#23 days is the mule deer buck hunting season every year
elk_harvest_estimate <- elk_harvest_estimate %>% 
  rename(elk_harvest = total_harvest) %>% 
  mutate(elk_daily_take = elk_harvest/37)

deer_harvest_estimate <- deer_harvest_estimate %>%
  rename(deer_harvest = total_harvest) %>% 
  mutate(deer_daily_take = deer_harvest/37,
         md_buck_daily = md_bucks/23)


# applying elk migration timing to take -----------------------------------

#reading in elk migraiton timing
source("scripts/elk_migration_timing.R")

#applying daily take to elk movement
daily_count <- daily_count %>% 
  
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
daily_count <- daily_count %>% 
  
  #shifting prop_available by the lowest value in each year
  #so the take values follow the same trend when multiplied
  group_by(year) %>%
  mutate(prop_scale = prop_available/sum(prop_available, na.rm=T)) %>%
  ungroup %>% 
  
  #calculating take number using the total take * scaled elk proportion available
  mutate(scale_elk_take = elk_harvest * prop_scale,
         scale_deer_take = deer_harvest * prop_scale,
         scale_md_buck_take = md_bucks * prop_scale) %>% 
  
  # # daily take number * proportion of elk available
  # mutate(wgt_elk_take = elk_daily_take * prop_available,
  #        wgt_deer_take = deer_daily_take * prop_available,
  #        wgt_md_buck_take = md_buck_daily * prop_available) %>% 
  # 
  # #changing final count to 0 if after mule buck season
  # mutate(wgt_md_buck_take = if_else((month == 11 & day > day(md_end)) |
  #                                       month == 12, 
  #                                     NA, wgt_md_buck_take)) %>% 
  
  #changing final count to 0 if after mule buck season
  mutate(scale_md_buck_take = if_else((month == 11 & day > day(md_end)) |
                                      month == 12, 
                                    NA, scale_md_buck_take))

  
#seeing if the scaled values are fine
daily_count %>% 
  group_by(year) %>% 
  summarize(elk = sum(scale_elk_take),
            deer = sum(scale_deer_take),
            md_buck = sum(scale_md_buck_take))


# #calculating the total take given by weighted take values
# wgt_take_est <- daily_count %>% 
#   group_by(year) %>% 
#   summarize(wgt_elk_est = sum(wgt_elk_take),
#             wgt_deer_est = sum(wgt_deer_take),
#             wgt_md_buck_est = sum(wgt_md_buck_take, na.rm = T)) %>% 
#   
#   #adding the total take numbers
#   left_join(daily_count %>% 
#               dplyr::select(year, elk_harvest, deer_harvest, md_bucks) %>% 
#               group_by(year) %>% 
#               slice(1),
#             by = join_by(year)) %>% 
#   
#   #calculating the amount to adjust the weighted take values to meet the total harvest numbers
#   #subtracting the weighted take from the total harvest
#     #dividing the difference by number of hunt days
#   mutate(adj_elk_amount = (elk_harvest - wgt_elk_est)/37,
#          adj_deer_amount = (deer_harvest - wgt_deer_est)/37,
#          adj_md_buck_amount = (md_bucks - wgt_md_buck_est)/23)


#adding harvest adjustment back to the weighted take values
daily_count <- daily_count %>% 
  
  mutate(
    #going to be using elk weight as a baseline
    #bison are 2.15x elk
    #deer are .3x elk
    final_elk_bms = scale_elk_take,
    final_deer_bms = scale_deer_take * .3,
    final_md_buck_bms = if_else(!is.na(scale_md_buck_take), #if the value is not NA (still buck season)
                                scale_md_buck_take * .3, #add the value
                                0) #otherwise leave the NA
    )
  
  # #adding the adjustment number
  # left_join(wgt_take_est %>% 
  #             dplyr::select(year, adj_elk_amount, adj_deer_amount, adj_md_buck_amount),
  #           by = join_by(year)) %>% 
  # 
  # #adjusting weighted take values to match total harvest number and account for prey weight
  # mutate(final_elk_take = wgt_elk_take + adj_elk_amount,
  #        final_deer_take = wgt_deer_take + adj_deer_amount,
  #        final_md_buck_take = if_else(!is.na(wgt_md_buck_take), #if the value is not NA (still buck season)
  #                                     wgt_md_buck_take + adj_md_buck_amount, #add the value
  #                                     0), #otherwise leave the NA
  #        
  #        #going to be using elk weight as a baseline
  #        #bison are 2.15x elk
  #        #deer are .3x elk
  #        final_elk_bms = wgt_elk_take + adj_elk_amount,
  #        final_deer_bms = (wgt_deer_take + adj_deer_amount) * .3,
  #        final_md_buck_bms = if_else(!is.na(wgt_md_buck_take), #if the value is not NA (still buck season)
  #                                    (wgt_md_buck_take + adj_md_buck_amount) * .3, #add the value
  #                                    0)) #otherwise leave the NA


#double checking yearly take numbers
# daily_count %>% 
#   group_by(year) %>% 
#   summarize(elk = sum(final_elk_take),
#             deer = sum(final_deer_take),
#             md_bucks = sum(final_md_buck_take, na.rm = T))


#creating a single column with the combined take numbers
daily_count <- daily_count %>% 
  mutate(final_take_bms = final_elk_bms + final_deer_bms + final_md_buck_bms)
  # mutate(final_take = final_elk_take + final_deer_take + final_md_buck_take,
  #        final_take_bms = final_elk_bms + final_deer_bms + final_md_buck_bms)


#plotting take numbers
# daily_count %>%
#   ggplot(aes(x = as.Date(paste(2000, md, sep = "-")), y = final_take,
#              group = year, col = factor(year))) +
#   geom_line()

#plotting biomass numbers
daily_count %>% 
  ggplot(aes(x = as.Date(paste(2000, md, sep = "-")), y = final_take_bms, 
             group = year, col = factor(year))) +
  geom_line()


# calculating floating window averages for previous days -------------------------------------------------------------------------
#only calculating for study periods
#calculating for early winter period

#making sure the full winter study period is present
daily_count <- daily_count %>% 
  
  #making a single date column
  mutate(date = make_date(year, month, day)) %>% 
  
  #group by year so there is no bleed over between seasons
  group_by(year) %>% 
  
  #completing the date sequence
  complete(date = seq(as.Date(paste0(cur_group(), "-11-15")),
                      as.Date(paste0(cur_group(), "-12-15")),
                      by = "day")) %>% 
  ungroup %>% 
  
  #making sure year, month, day columns are okay
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         md = paste(month, day, sep = "-"))


#function to calculate time period averages for hunting take in previous days
fwp_window_function <- function(window){
  
  #making sure the full winter study period is present
  data <- daily_count %>% 
    
    group_by(year) %>% 
    
  arrange(year, month, day) %>% 
    
    #adding the moving average
    mutate(!!paste0("bms_window_", window) := 
             slider::slide_dbl(final_take_bms, mean,
                               .before = window, .after = -1,
                               .complete = T, na.rm=T))

  return(data)
}

daily_count <- fwp_window_function(1)
daily_count <- fwp_window_function(3)
daily_count <- fwp_window_function(5) %>%
  
  #making sure the later days in the study are represented as a 0
  mutate(bms_window_1 = if_else(is.nan(bms_window_1), 0, bms_window_1),
         bms_window_3 = if_else(is.nan(bms_window_3), 0, bms_window_3),
         bms_window_5 = if_else(is.nan(bms_window_5), 0, bms_window_5))


#plotting moving average
daily_count %>% 
  ggplot(aes(x = as.Date(paste(2000, md, sep = "-")), y = bms_window_5, 
             group = year, col = factor(year))) +
  geom_line()
