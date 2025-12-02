#Creating covariates for model
library(dplyr)
library(lubridate)
library(data.table)


#currently contains all winter months (nov-mar)


# running code to get daily commute decisions -----------------------------
source("scripts/commute_decision.R")

#3 = has a least 1 point that day in Gardiner poly
#2 = has at least 1 point farther than 1 km from terr poly , but none in Gardiner poly
#1 = only has points in terr poly


#removing 7646 because there aren't enough winter points
#removing 7653 and 7596 because Canada
commute_df <- commute_df %>% 
  
  #renaming raven_id
  rename(raven_id = individual_local_identifier) %>% 
  
  filter(raven_id != "7646",
         raven_id != "7653",
         raven_id != "7596") %>% 
  
  #creating new binary columns for traveling to Gardiner or staying in territory
  mutate(
    # terr_bin
    # 1 = left territory
    # 0 = stayed on territory
    terr_bin = if_else(commute != 1, TRUE, FALSE),
    # hunt_bin
    # 1 = visited hunting
    # 0 = visited other place
    hunt_bin = if_else(commute == 3, TRUE, FALSE))


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


# FUNCTION: Kills in territory -------------------------------------------------------------

#function to separate out the kills that are within each territory
  #' data: dataframe
  #' dist_from_terr: how far (m) a kill is from the territory to be counted towards that territory
  #' coords: columns that have the x and y (x, y)
  #' crs: coordinate reference system for the coordinates
  #' start: column name for the start of the kill (date of death, cluster start)
kill_in_terr <- function(data, dist_from_terr, coords, crs, start){
  ID <- mcp90$id

  #creating a list to put the kill information for kills inside each territory
  in_terr_kill_list <- vector("list", length(ID))
  names(in_terr_kill_list) <- ID

  #changing kill data into a format that can be used for the distance measurement
  tmp_sf <- st_as_sf(data, coords = coords,
                     crs = crs)

  for(i in 1:length(ID)){

    #calculating distance
    tmp_dist <- as.numeric(st_distance(tmp_sf, st_as_sf(mcp90[i,])))

    #putting all rows with distance == 0 into the list
    #and ordering by date
    in_terr_kill_list[[i]] <- subset(data, tmp_dist <= dist_from_terr) %>%
      arrange(start)

  }
  return(in_terr_kill_list)
}


#reading in wolf project kill database
kill_data <- readr::read_csv("data/raw/wolf_project_carcass_data.csv") %>% 
  janitor::clean_names()
kill_data$dod <- mdy(kill_data$dod)


#subset to only kills from 2019 onwards to match raven GPS data
#subset to only winter months (Nov, Dec, Mar)
#but removing kills that are in Jan-Mar of 2019
kill_data_recent <- kill_data %>%
  filter(year(dod) >= 2019 & month(dod) %in% c(11,12,3)) %>%
  filter(dod >= as.Date("2019-11-01"))


#creating new column with the most accurate coords available
#order of accuracy ground -> aerial -> estimated ground
kill_data_recent <- kill_data_recent %>%
  mutate(easting = case_when(!is.na(ground_east) ~ ground_east,
                             !is.na(aerial_east) ~ aerial_east,
                             !is.na(est_ground_east) ~ est_ground_east),
         northing = case_when(!is.na(ground_north) ~ ground_north,
                              !is.na(aerial_north) ~ aerial_north,
                              !is.na(est_ground_north) ~ est_ground_north)) %>%
  filter(!is.na(easting)) %>%
  
  #removing cat kills
  filter(nchar(pack) > 4)

#wolf project database: in territory kills for eac raven
in_terr_kill_list <- kill_in_terr(kill_data_recent, dist_from_terr = 1000,
                               coords = c("easting", "northing"), crs = "+proj=utm +zone=12",
                               start = "dod")


#reading in RF data
source("scripts/clean_rf_data.R")
rf_in_terr_kill_list <- kill_in_terr(kill_data_rf, dist_from_terr = 1000,
                               coords = c("easting", "northing"), crs = "+proj=utm +zone=12",
                               start = "kill_start_date")


# Average kill density -------------------------------------------------------------
## of carcasses in territory/# of days(30) 
## averaged over all years of data for that raven

##' old faithful birds have no kills in terr, need to figure out how that is handled
##'   probably just a 0 
kill_density_list <- lapply(in_terr_kill_list, function(x){
  if(nrow(x) != 0){
    #empty vector to attach all the values of days since previous carcass
    days_since <- c()
    
    #start and end dates for each winter period
    early_winter_start <- as.Date(paste0(seq(min(year(x$dod)), max(year(x$dod))),"-11-15"))
    early_winter_end <- as.Date(paste0(seq(min(year(x$dod)), max(year(x$dod))),"-12-15"))
    late_winter_start <- as.Date(paste0(seq(min(year(x$dod)), max(year(x$dod))),"-03-01"))
    late_winter_end <- as.Date(paste0(seq(min(year(x$dod)), max(year(x$dod))),"-03-30"))
    
    
    #dataframe to put the kill density numbers for each winter sample period
    density_df <- data.frame(year = rep(year(early_winter_start), 2), 
                             period = rep(c("early", "late"), each = length(early_winter_start)), 
                             density = NA)
    
    for(w in 1:length(early_winter_start)){
      
      early_winter <- subset(x, dod >= early_winter_start[w] & 
                               dod <= early_winter_end[w])
      late_winter <- subset(x, dod >= late_winter_start[w] & 
                              dod <= late_winter_end[w])
      
      
      #early winter density
      density_df[w, "density"] <- nrow(early_winter)/30
      
      #late winter density
      density_df[w+length(late_winter_start), "density"] <- nrow(late_winter)/30
    }
    return(density_df)
  }
})

rf_kill_density_list <- lapply(rf_in_terr_kill_list, function(x){
  if(nrow(x) != 0){
    #empty vector to attach all the values of days since previous carcass
    days_since <- c()
    
    #start and end dates for each winter period
    early_winter_start <- as.Date(paste0(seq(min(year(x$kill_start_date)), max(year(x$kill_start_date))),"-11-15"))
    early_winter_end <- as.Date(paste0(seq(min(year(x$kill_start_date)), max(year(x$kill_start_date))),"-12-15"))
    late_winter_start <- as.Date(paste0(seq(min(year(x$kill_start_date)), max(year(x$kill_start_date))),"-03-01"))
    late_winter_end <- as.Date(paste0(seq(min(year(x$kill_start_date)), max(year(x$kill_start_date))),"-03-30"))
    
    
    #dataframe to put the kill density numbers for each winter sample period
    density_df <- data.frame(year = rep(year(early_winter_start), 2), 
                             period = rep(c("early", "late"), each = length(early_winter_start)), 
                             rf_density = NA)
    
    for(w in 1:length(early_winter_start)){
      
      early_winter <- subset(x, kill_start_date >= early_winter_start[w] & 
                               kill_start_date <= early_winter_end[w])
      late_winter <- subset(x, kill_start_date >= late_winter_start[w] & 
                              kill_start_date <= late_winter_end[w])
      
      
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
avg_kill_density <- bind_rows(kill_density_list, .id = "raven_id") %>% 
  group_by(raven_id) %>% 
  summarize(avg_terr_kill_density = mean(density))
rf_avg_kill_density <- bind_rows(rf_kill_density_list, .id = "raven_id") %>% 
  group_by(raven_id) %>% 
  summarize(rf_avg_terr_kill_density = mean(density))

commute_df <- commute_df %>% 
  #wolf project database
  left_join(avg_kill_density) %>% 
  #Rf predictive 
  left_join(rf_avg_kill_density) %>% 
  #making kill_density 0 when NA since there were no kills in its territory
  #having a row in the commute_df means the raven was taking points that day
  mutate(avg_terr_kill_density = if_else(is.na(avg_terr_kill_density), 0, 
                                         avg_terr_kill_density),
         rf_avg_terr_kill_density = if_else(is.na(rf_avg_terr_kill_density), 0, 
                                         rf_avg_terr_kill_density))


# Active kill -------------------------------------------------------------
## presence of an active kill within the territory

  #' data: in_terr_kill_list from the kill_in_terr function
  #' days_since: the number of days since the kill was made (day of the kill is 0)
active_kill_fctn <- function(data, days_since, start, end){
  #new column for the binary predictor
  commute_df$active_kill <- FALSE
  
  #separating each raven in the main data set and matching it to the inside territory kill lists
  active_kill_list <- tapply(commute_df, commute_df$raven_id,
                             FUN = function(x){
                               ID <- unique(x$raven_id)
                               tmp_kills <- data[[ID]]
                               
                               # looping through each GPS point to see if there is an active kill that day
                               for(i in 1:nrow(x)){
                                 tmp_GPS <- x[i,]
                                 
                                 # calculating time difference in days to start of carcass (0 = kill on that day) for all kills in territory
                                 time_diff_start <- difftime(as.Date(tmp_GPS$date),
                                                             tmp_kills %>% 
                                                               pull(start), 
                                                             units = "days") %>% 
                                   as.numeric()
                                 # calculating time difference in days to end of carcass for all kills in territory
                                  # for wolf project database this is the same as start 
                                  # for RF predictive, this is the cluster end date
                                 time_diff_end <- difftime(as.Date(tmp_GPS$date),
                                                           tmp_kills %>% 
                                                             pull(end), 
                                                           units = "days") %>% 
                                   as.numeric()
                                 
                                 # if GPS point is after the kill is made and before the days_since argument for any of the kills in territory
                                  # active_kill is TRUE
                                 if(sum(time_diff_start >= 0 & time_diff_end <= days_since) >= 1){
                                   x[i, "active_kill"] <- TRUE
                                 }
                               }
                               return(x)
                             })
  return(bind_rows(active_kill_list)$active_kill)
}

#wolf project database
commute_df <- commute_df %>% 
  mutate(
    #wolf project kill database
    active_kill = active_kill_fctn(in_terr_kill_list, days_since = 2, 
                                   start = "dod", end = "dod"),
    #RF predictive kills
    rf_active_kill = active_kill_fctn(rf_in_terr_kill_list, days_since = 1, 
                                      start = "kill_start_date", end = "kill_end_date"))


# Hunting season-------------------------------------------------------------
##   binary covariate for if the hunting season is in effect

##' FWP hunting season is to down to the day
##' march for tribal bison hunting (This actually depends on bison movement)

#reading in hunting dates
fwp_dates <- readxl::read_xlsx("data/raw/hunting_seasons.xlsx")%>% 
  dplyr::select(year, start, end) %>% 
  rename(fwp_start_hunt = start,
         fwp_end_hunt = end)
bison_dates <- readr::read_csv("data/raw/bison_hunt.csv") %>% 
  dplyr::select(year, start_date) %>% 
  rename(bison_start_hunt = start_date) %>% 
  mutate(bison_start_hunt = lubridate::mdy(bison_start_hunt),
         winter_year = year - 1) %>% 
  dplyr::select(-year)

commute_df <- commute_df %>% 
  #adding month, day columns
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>% 
  #adding hunting end date
  left_join(fwp_dates)  %>% 
  left_join(bison_dates) %>% 
  
  #creating new boolean column for hunting season
    #' TRUE = active hunting season
  mutate(
    #FWP season
    hunt_season = if_else((format(date, "%m-%d") >= 
                           format(fwp_start_hunt, "%m-%d")) &
                          (format(date, "%m-%d") <= 
                             format(fwp_end_hunt, "%m-%d")), 
                        TRUE, FALSE),
    # tribal bison season
    hunt_season = if_else(date >= bison_start_hunt, TRUE, hunt_season))

  


# FWP hunting take ------------------------------------------------------------
#adding FWP hunting estimates

source("scripts/fwp_hunting_estimates.R")

commute_df <- commute_df %>%
  left_join(daily_count %>%
              dplyr::select(year, month, day, 
                            final_take_bms, final_take),
            by = join_by(year, month, day))


# Tribal bison take --------------------------------------------------------------
# adding daily bison take values from NPS Bison Project
# days without recorded values are remaining as 0 since they didn't start survey efforts until bison moved out of the park and became available to take

#reading in daily take data
bison_daily_take <- readr::read_csv("data/clean/bison_daily_take.csv") %>%
  rename(bison_take = take) %>% 
  mutate(date = lubridate::mdy(date))

#adding to covariate data
commute_df <- commute_df %>%
  left_join(bison_daily_take) %>%
  # making NA days 0 instead
  mutate(bison_take = if_else(is.na(bison_take), 0, bison_take),
         final_take_bms = if_else(is.na(final_take_bms), 0, final_take_bms),
         final_take = if_else(is.na(final_take), 0, final_take) ) %>%
  # adding bison take to main hunter take column and adding bison multiplier for relative biomass
  mutate(final_take_bms = final_take_bms + bison_take*2.15,
         final_take = final_take + bison_take)


# #!!!! some code is included and commented out for using a hunting start date 
#   # calculating daily take
#   # adding the daily biomass to the window columns based on start date of the hunt
# 
# 
# #reading in data
# #year on this data is the year at the time of march, not the hunting season year
# bison_take <- readr::read_csv("data/raw/bison_hunt.csv")
# 
# commute_df <- bison_take %>% 
#   
#   #making a single column with the greatest take value from the FEIS or IBMP
#   mutate(bison_daily_take = if_else(is.na(take), ibmp,
#                               if_else(is.na(ibmp), take,
#                                       if_else(take > ibmp, take, ibmp))),
#          
#          #dividing by hunting days - add back in when I get the hunt start dates
#          # end_date = as.Date(paste0(year(start_date), "-3-31")),
#          # bison_daily_take = bison_daily_take/(difftime(end_date, start_date, units = "days") + 1),
#          bison_daily_take = bison_daily_take/31,
#          
#          #making the biomass based around elk weight = 1x
#          bison_daily_bms = bison_daily_take * 2.15) %>% 
#   
#   #joining to commute data 
#   rename(bison_hunt_start = start_date) %>% 
#   dplyr::select(year, bison_hunt_start, bison_daily_take, bison_daily_bms) %>% 
#   right_join(commute_df, by = join_by(year)) %>% 
#   
#   
#   #reorganizing columns to keep raven and date data up front
#   relocate(bison_daily_take, bison_daily_bms,
#            .after = final_take_bms) %>% 
#   relocate(bison_hunt_start, 
#            .after = start_hunt) %>% 
#   
#   #adding the bison biomass number to the moving window columns
#   # {if(month(bison_hunt_start) == 3){
#   #   mutate(bms_window_1 = if_else(month == 3 & day >= day(start(date)), bison_daily_bms,
#   #                                 if_else(month == 3 & day < day(start(date)), 0, bms_window_1)),
#   #          bms_window_3 = if_else(month == 3 & day >= day(start(date)), bison_daily_bms,
#   #                                 if_else(month == 3 & day < day(start(date)), 0, bms_window_3)),
#   #          bms_window_5 = if_else(month == 3 & day >= day(start(date)), bison_daily_bms,
#   #                                 if_else(month == 3 & day < day(start(date)), 0, bms_window_5)))
#   #   } else(
#   #     mutate(bms_window_1 = if_else(month == 3, bison_daily_bms, bms_window_1),
#   #            bms_window_3 = if_else(month == 3, bison_daily_bms, bms_window_3),
#   #            bms_window_5 = if_else(month == 3, bison_daily_bms, bms_window_5))
#   #     )
#   # }
# 
#   mutate(bms_window_1 = if_else(month == 3, bison_daily_bms, bms_window_1),
#          bms_window_3 = if_else(month == 3, bison_daily_bms, bms_window_3),
#          bms_window_5 = if_else(month == 3, bison_daily_bms, bms_window_5),
#          final_take_bms = if_else(month == 3, bison_daily_bms, final_take_bms)) 


# Hunting categorical take ----------------------------------------------------
# #adding just high or low periods/years instead of numerical values
# 
# #low period is before nov 7
# #low period for bison is years < 1 (which is low values of below 10 in a season)
# 
# commute_df <- commute_df %>% 
#   
#   mutate(take_high_low = if_else(final_take_bms == 0, "zero",
#                                  if_else(
#                                    #in the early FWP season (before Nov 7)
#                                    (paste(month, day, sep = "-") >= format(start_hunt, "%m-%d") &
#                                       paste(month, day, sep = "-") <= "11-7"),
#                                    "low",
#                                    "high")),
#          take_high_low = if_else(#in a low bison take year (< 10 in a season)
#                                     month == 3 & bison_daily_take < 1,
#                                     "low",
#                                     take_high_low))

# Weekends ----------------------------------------------------------------
#adding weekend effect for hunting

commute_df <- commute_df %>% 
  
  mutate(weekend = if_else(weekdays(date) %in% c("Saturday", "Sunday"), TRUE, FALSE))


# Clearing up tagged pairs ------------------------------------------------

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


# Winter study periods (early/late) ------------------------------------------

commute_df <- commute_df %>% 
  mutate(study_period = if_else(month %in% c(10, 11, 12, 1, 2), 
                                if_else(month %in% c(10, 11, 12), "early", "mid"), 
                                "late"))



# Yearly kill density -----------------------------------------------------
## kills in territory/30 days

## distinct kill density for each year and study period

#turning kill_density_list into dataframe that can be joined to commute_df
kill_density_df <- kill_density_list %>% 
  #turning list into single dataframe while retaining raven_id as a column
  bind_rows(.id = "source") %>% 
  #fixing column names
  rename(raven_id = source,
         yearly_terr_kill_density = density,
         study_period = period)

#adding yearly kill density to main data
commute_df <- commute_df %>% 
  left_join(kill_density_df) %>% 
  #making kill_density 0 when NA since there were no kills in its territory
  #having a row in the commute_df means the raven was taking points that day
  mutate(yearly_terr_kill_density = if_else(is.na(yearly_terr_kill_density), 0, 
                                         yearly_terr_kill_density))




# Other raven movement decisions ------------------------------------------
# how many OTHER tagged ravens decided to make what movement decisions

# Calculating how total many ravens out of all ravens chose to 
  #' leave their territory
  #' visit the Gardiner hunting district
group_commute_decision <- commute_df %>% 
  group_by(date) %>% 
  summarize(group_left_terr = sum(terr_bin),
            group_visit_hunt = sum(hunt_bin),
            #total number of ravens with data for that day
            #need to subtract one when adding to commute_df to remove that raven from calculations
            n_raven_daily = n())

#adding group commute decision to main dataframe
commute_df <- commute_df %>% 
  left_join(group_commute_decision) %>% 
  
  #editing the values of group commute to remove the decision of that row
  mutate(group_left_terr = if_else(terr_bin == TRUE, #if that raven left its territory
                                   group_left_terr - 1, #remove 1 raven from the group that chose to leave
                                   group_left_terr), #else leave the number alone
         group_visit_hunt = if_else(hunt_bin == TRUE, #if that raven visited Gardiner
                                    group_visit_hunt - 1, #remove 1 raven from the group that visited Gardiner
                                    group_visit_hunt),
         n_raven_daily = if_else(n_raven_daily == 1, NA, n_raven_daily - 1)) %>% 
  
  #turning the raw values into a proportion
  mutate(prop_group_left_terr = group_left_terr/n_raven_daily,
         prop_group_visit_hunt = group_visit_hunt/n_raven_daily) %>% 
  
  #removing raw value of group raven daily decisions
  dplyr::select(-c(group_left_terr, group_visit_hunt, n_raven_daily))



# Individual decision history --------------------------------------------

#looking to see about the days since the last data point
commute_df <- commute_df %>%
  group_by(raven_id, year, study_period) %>%
  arrange(date) %>%
  mutate(
    #adding days since the previous data point
    days_since_last = as.numeric(date - lag(date)),
  #' previous day = 1668 days
  #' 2 days = 1712 days
  #' 3 days = 1732 days
  #' 4 days = 1752 days
  #' 7 days = 1768 days

#adding decisions of previous day
    #previous day history
    previous_decision_terr = if_else(days_since_last == 1, 
                                     lag(terr_bin), 
                                     NA),
    previous_decision_hunt = if_else(days_since_last == 1, 
                                     lag(hunt_bin), 
                                     NA))


# Weather -------------------------------------------------------------
# # temperature history from Red Lodge
# temp_history <- readr::read_csv("data/raw/red_lodge_weather.csv")

#weather history from NOAA for Mammoth (temperature and snow depth)
weather_history <- readr::read_csv("data/raw/noaa_weather_ncei.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  #removing average temperature column because its empty 
  dplyr::select(-tavg_degrees_fahrenheit) %>% 
  #renaming for simplicity
  rename(temp_max = tmax_degrees_fahrenheit,
         temp_min = tmin_degrees_fahrenheit,
         precip = prcp_inches,
         snow_fall = snow_inches,
         snow_depth = snwd_inches)

commute_df <- commute_df %>% 
  left_join(weather_history)

# commute_df <- commute_df %>% 
#   left_join(temp_history)
# 
# plot(tmax_degrees_fahrenheit ~ temp_max, data = commute_df)
# Writing out csv to cleaned data folder ----------------------------------
#so this doesn't have to be run every time to work with model script

write.csv(commute_df %>%
            ungroup() %>% 
            #removing unnecessary columns
            dplyr::select(-c(commute, bison_take, year, bison_start_hunt, fwp_start_hunt, 
                             fwp_end_hunt, days_since_last, winter_year, temp_min)),
          "data/clean/commute_data.csv")
