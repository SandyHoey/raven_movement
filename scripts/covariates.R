#Creating covariates for model
library(dplyr)
library(lubridate)
library(data.table)


#currently contains all winter months (nov-mar)

#!!! needs to be restructured when I get the predictive kill data


# running code to get daily commute decisions -----------------------------
source("scripts/commute_decision.R")

#3 = has a least 1 point that day in Gardiner poly
#2 = has at least 1 point farther than 1 km from terr poly , but none in Gardiner poly
#1 = only has points in terr poly

#clearing environment of unnecessary variables
rm(list = setdiff(ls(), c("commute_df", "mcp90")))


#removing 7646 because there aren't enough winter points
#removing 7653 and 7596 because Canada
commute_df <- commute_df %>% 
  
  #renaming raven_id
  rename(raven_id = individual_local_identifier) %>% 
  
  filter(raven_id != "7646",
         raven_id != "7653",
         raven_id != "7596") %>% 
  
  #creating new binary columns for traveling to gardiner or staying in territory
  mutate(
    # terr_bin
    # 1 = left territory
    # 0 = stayed on territory
    terr_bin = if_else(commute == 1, 0, 1),
    # hunt_bin
    # 1 = visited hunting
    # 0 = visited other place
    hunt_bin = if_else((terr_bin = 1 & commute == 3), 1, 0))


# Distance to north entrance ---------------------------
## calculating distance between raven territories and the north entrance station

north_entrance_utm <- data.frame(easting = 523575, northing = 4985810) %>% 
  st_as_sf(coords=c("easting", "northing"), crs = "+proj=utm +zone=12")

#getting centroid of each ravens territory
terr_center <- mcp90 %>% 
  
  #extracting the polygon coordinates from the mcp
  ggplot2::fortify() %>% 
  
  #getting centroid by averaging
  group_by(id) %>% 
  rename(easting = long, northing = lat) %>% 
  summarize(easting = mean(easting), northing = mean(northing)) %>% 
  
  #transforming to utm
  st_as_sf(coords=c("easting", "northing"), crs = st_crs(north_entrance_utm)) %>% 
  
  #calculating distance from terr center to north entrance
  mutate(., dist2nentrance = as.numeric(st_distance(., north_entrance_utm))) %>% 
  st_drop_geometry

#adding to main dataframe
commute_df <- commute_df %>% 
  left_join(terr_center, by = join_by(raven_id == id))


# Time btwn kills -------------------------------------------------------------
## time between kills within territory

##'   kill density may be better (basic kill density)
##'   
##'   
##' !!!! PROBLEMS !!!!
##' Just use kill density
##' but if you want to bring this back it needs a rework
##' only calculate days between for early and late winter instead of the whole winter

kill_data <- read.csv("data/raw/wolf_project_carcass_data.csv")
kill_data$DOD <- mdy(kill_data$DOD)

#subset to only kills from 2019 onwards to match raven GPS data
#subset to only winter months (Nov, Dec, Mar)
#but removing kills that are in Jan-Mar of 2019
kill_data_recent <- kill_data %>%
  filter(year(DOD) >= 2019 & month(DOD) %in% c(11,12,3)) %>%
  filter(DOD >= as.Date("2019-11-01"))


#creating new column with the most accurate coords available
#order of accuracy ground -> aerial -> estimated ground
kill_data_recent <- kill_data_recent %>%
  mutate(easting = case_when(!is.na(GROUND.EAST) ~ GROUND.EAST,
                             !is.na(AERIAL.EAST) ~ AERIAL.EAST,
                             !is.na(EST.GROUND.EAST) ~ EST.GROUND.EAST),
         northing = case_when(!is.na(GROUND.NORTH) ~ GROUND.NORTH,
                              !is.na(AERIAL.NORTH) ~ AERIAL.NORTH,
                              !is.na(EST.GROUND.NORTH) ~ EST.GROUND.NORTH)) %>%
  filter(!is.na(easting)) %>%

  #removing cat kills
  filter(nchar(PACK) > 4)


#function to separate out the kills that are within each territory
#dist_from_terr: how far (m) a kill is from the territory to be counted towards that territory
kill_freq <- function(dist_from_terr = 0){
  ID <- mcp90$id

  #creating a list to put the kill information for kills inside each territory
  in_terr_kill_list <- vector("list", length(ID))
  names(in_terr_kill_list) <- ID

  #changing kill data into a format that can be used for the distance measurement
  tmp_sf <- st_as_sf(kill_data_recent, coords=c("easting", "northing"),
                     crs="+proj=utm +zone=12")

  for(i in 1:length(ID)){

    #calculating distance
    tmp_dist <- as.numeric(st_distance(tmp_sf, st_as_sf(mcp90[i,])))

    #putting all rows with distance == 0 into the list
    #and ordering by date
    in_terr_kill_list[[i]] <- subset(kill_data_recent, tmp_dist <= dist_from_terr) %>%
      arrange(DOD)

  }
  return(in_terr_kill_list)
}

in_terr_kill_list <- kill_freq(dist_from_terr = 3000)


# #counting the days between consecutive kills within each territory
# #Summer is messing up days since 
# #7485/7494 (old faithful) has no kills within 3 km of territory, so making the days between the max value (30)
# day_betwn_kill <- lapply(in_terr_kill_list, function(x){
#   if(nrow(x) != 0){
#     #empty vector to attach all the values of days since previous carcass
#     days_since <- c()
#     
#     #start and end dates for each winter period
#     winter_start <- as.Date(paste0(seq(min(year(x$DOD))-1, max(year(x$DOD))),"-11-01"))
#     winter_end <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))+1),"-03-31"))
#     
#     for(w in 1:length(winter_start)){
#       
#       tmp_winter <- subset(x, DOD >= winter_start[w] & 
#                              DOD <= winter_end[w])
#       
#       #has an NA value since the first carcass of a winter period cant have a "days since last carcass"
#       if(nrow(tmp_winter) == 1){
#         days_since <- c(days_since, NA)
#       }else if(nrow(tmp_winter) == 0){
#       }else{
#         days_since <- c(days_since, NA, diff(tmp_winter$DOD))
#       }
#     }
#     
#     x$days_since <- days_since
#   } else(days_since <- 30)
# })
# 
# 
# #calculating average day between kills for individuals
# avg_day_betwn_kill <- day_betwn_kill %>% 
#   lapply(mean, na.rm = T) %>% 
#   do.call("rbind",.) %>% 
#   as.data.frame() %>% 
#   rename(avg_day_btwn = V1)
# 
# colnames(avg_day_betwn_kill) <- "avg_day_btwn"
# avg_day_betwn_kill <- mutate(avg_day_betwn_kill, raven_id = rownames(avg_day_betwn_kill)) 



# Kill density -------------------------------------------------------------
## of carcasses in territory/# of days(30)

##'   going to be calculated only for winter studies when kill detection is best
##' 
##' old faithful birds have no kills in terr, need to figure out how that is handled
##'   probably just a 0 

kill_density <- lapply(in_terr_kill_list, function(x){
  if(nrow(x) != 0){
    #empty vector to attach all the values of days since previous carcass
    days_since <- c()
    
    #start and end dates for each winter period
    early_winter_start <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))),"-11-15"))
    early_winter_end <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))),"-12-15"))
    late_winter_start <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))),"-03-01"))
    late_winter_end <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))),"-03-30"))
    
    
    #dataframe to put the kill density numbers for each winter sample period
    density_df <- data.frame(year = rep(year(early_winter_start), 2), 
                             period = rep(c("early", "late"), each = length(early_winter_start)), 
                             density = NA)
    
    for(w in 1:length(early_winter_start)){
      
      early_winter <- subset(x, DOD >= early_winter_start[w] & 
                               DOD <= early_winter_end[w])
      late_winter <- subset(x, DOD >= late_winter_start[w] & 
                              DOD <= late_winter_end[w])
      
      
      #early winter density
      density_df[w, "density"] <- nrow(early_winter)/30
      
      #late winter density
      density_df[w+length(late_winter_start), "density"] <- nrow(late_winter)/30
    }
    return(density_df)
  }
})

#am going to use average kill density for each individual
#the kill density is calculated from winter study periods, so there isn't a number for
#the other months anyways
avg_kill_density <- bind_rows(kill_density, .id = "raven_id") %>% 
  group_by(raven_id) %>% 
  summarize(avg_density = mean(density))



# Active kill -------------------------------------------------------------
## presence of an active kill within the territory

##' active is < than 3 days old (<= 2 days)
##' days_since is the number of days since the kill was made, including the day of the kill

active_kill_fctn <- function(days_since = 3){
  commute_df$active_kill <- 0
  
  tapply(commute_df, commute_df$raven_id,
         FUN = function(x){
           ID <- unique(x$raven_id)
           tmp_kills <- in_terr_kill_list[[ID]]
           
           #looping through each GPS point to see if there is a active kill
           for(i in 1:nrow(x)){
             tmp_GPS <- x[i,]
             
             time_diff <- difftime(tmp_kills$DOD, as.Date(tmp_GPS$date), units = "days")
             
             if(sum(time_diff >= 0 & time_diff < days_since) >= 1){
               x[i, "active_kill"] <- 1
             }
           }
           return(x)
         })
}

commute_df <- bind_rows(active_kill_fctn())



# Hunting season-------------------------------------------------------------
##   binary covariate for if the hunting season is in effect

##' FWP hunting season is to down to the day
##' march for tribal bison hunting (This actually depends on bison movement)

#reading in hunting dates
hunting_dates <- readxl::read_xlsx("data/raw/hunting_seasons.xlsx")

commute_df <- commute_df %>% 
  
  #adding month, day columns
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  
  #adding hunting end date
  left_join(hunting_dates %>% 
              dplyr::select(year, start, end),
            by = join_by(year)) %>% 
  rename(start_hunt = start,
         end_hunt = end) %>% 
  
  #creating new boolean column for hunting season
  mutate(hunt = if_else((format(date, "%m-%d") >= 
                           format(start_hunt, "%m-%d")) &
                          (format(date, "%m-%d") <= 
                             format(end_hunt, "%m-%d")), 
                        1, #days in nov before end date
                        0))  #otherwise no hunting == 0 n nov before end date

  


# FWP hunting take ------------------------------------------------------------

#adding FWP hunting estimates
source("scripts/fwp_hunting_estimates.R")

commute_df <- commute_df %>%
  left_join(daily_count %>%
              dplyr::select(year, month, day, 
                            final_take_bms, 
                            contains("window")),
            by = join_by(year, month, day))



# Tribal bison take --------------------------------------------------------------
#!!!! some code is included and commented out for using a hunting start date 
  # calculating daily take
  # adding the daily biomass to the window columns based on start date of the hunt


#reading in data
#year on this data is the year at the time of march, not the hunting season year
bison_take <- read.csv("data/raw/bison_hunt.csv")

commute_df <- bison_take %>% 
  
  #making a single column with the greatest take value from the FEIS or IBMP
  mutate(bison_daily_take = if_else(is.na(take), ibmp,
                              if_else(is.na(ibmp), take,
                                      if_else(take > ibmp, take, ibmp))),
         
         #dividing by hunting days - add back in when I get the hunt start dates
         # end_date = as.Date(paste0(year(start_date), "-3-31")),
         # bison_daily_take = bison_daily_take/(difftime(end_date, start_date, units = "days") + 1),
         bison_daily_take = bison_daily_take/31,
         
         #making the biomass based around elk weight = 1x
         bison_daily_bms = bison_daily_take * 2.15) %>% 
  
  #joining to commute data 
  rename(bison_hunt_start = start_date) %>% 
  dplyr::select(year, bison_hunt_start, bison_daily_take, bison_daily_bms) %>% 
  right_join(commute_df, by = join_by(year)) %>% 
  
  
  #reorganizing columns to keep raven and date data up front
  relocate(bison_daily_take, bison_daily_bms,
           .after = final_take_bms) %>% 
  relocate(bison_hunt_start, 
           .after = start_hunt) %>% 
  
  #adding the bison biomass number to the moving window columns
  # {if(month(bison_hunt_start) == 3){
  #   mutate(bms_window_1 = if_else(month == 3 & day >= day(start(date)), bison_daily_bms,
  #                                 if_else(month == 3 & day < day(start(date)), 0, bms_window_1)),
  #          bms_window_3 = if_else(month == 3 & day >= day(start(date)), bison_daily_bms,
  #                                 if_else(month == 3 & day < day(start(date)), 0, bms_window_3)),
  #          bms_window_5 = if_else(month == 3 & day >= day(start(date)), bison_daily_bms,
  #                                 if_else(month == 3 & day < day(start(date)), 0, bms_window_5)))
  #   } else(
  #     mutate(bms_window_1 = if_else(month == 3, bison_daily_bms, bms_window_1),
  #            bms_window_3 = if_else(month == 3, bison_daily_bms, bms_window_3),
  #            bms_window_5 = if_else(month == 3, bison_daily_bms, bms_window_5))
  #     )
  # }

  mutate(bms_window_1 = if_else(month == 3, bison_daily_bms, bms_window_1),
         bms_window_3 = if_else(month == 3, bison_daily_bms, bms_window_3),
         bms_window_5 = if_else(month == 3, bison_daily_bms, bms_window_5)) 



# Weekends ----------------------------------------------------------------
#adding weekend effect for hunting

commute_df <- commute_df %>% 
  
  mutate(weekend = if_else(weekdays(date) %in% c("Saturday", "Sunday"), 1, 0))


# clearing up tagged pairs ------------------------------------------------

# High bridge pair (7654 & 7530)
# Tower pair (7484_2 & 7493_2)

#looking at the number of days for each raven
high_m <- commute_df %>% 
  filter(raven_id == "7654")
high_f <- commute_df %>% 
  filter(raven_id == "7530")
tower_m <- commute_df %>% 
  filter(raven_id == "7484_2")
tower_f <- commute_df %>% 
  filter(raven_id == "7493_2")

nrow(high_m)
nrow(high_f)

nrow(tower_m)
nrow(tower_f)

#females are both better than males

commute_df <- commute_df %>% 
  filter(raven_id != "7654",
         raven_id != "7484_2")


# reading out csv to cleaned data folder ----------------------------------
#so this doesn't have to be run every time to work with model script

write.csv(commute_df, "data/clean/commute_data.csv")
