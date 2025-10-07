#Creating covariates for model
library(dplyr)
library(lubridate)
library(data.table)

#needs to be restructured when I get the predictive kill data



# reading in and subsetting data ------------------------------------------

#reading in all raven points
#removing columns with NA coords
all_gps <- readr::read_csv("data/clean/all_raven_gps_clean58.csv") %>% 
  
  #removing useless columns
  dplyr::select(-c(...1, visible, bar_barometric_pressure, data_decoding_software,
                   eobs_activity, eobs_activity_samples, eobs_key_bin_checksum,
                   eobs_speed_accuracy_estimate, eobs_start_timestamp, eobs_status,
                   eobs_temperature, eobs_type_of_fix, eobs_used_time_to_get_fix,
                   ground_speed, heading, height_above_ellipsoid, height_above_msl,
                   height_raw, import_marked_outlier, mag_magnetic_field_raw_x,
                   mag_magnetic_field_raw_y, mag_magnetic_field_raw_z,
                   manually_marked_outlier, orientation_quaternion_raw_w,
                   orientation_quaternion_raw_x, orientation_quaternion_raw_y,
                   orientation_quaternion_raw_z, sensor_type, individual_taxon_canonical_name,
                   tag_local_identifier, study_timezone))
  
all_gps <- subset(all_gps, !is.na(utm_easting))


#removing 7646 because there arent enough winter points
#removing 7653 and 7596 because Canada
all_gps <- subset(all_gps, individual_local_identifier != "7646")
all_gps <- subset(all_gps, individual_local_identifier != "7653")
all_gps <- subset(all_gps, individual_local_identifier != "7596")


#importing demographic information
#terr: territorial birds with nest inside Yellowstone
#trans: birds that transitioned between breeder and nonbreeder
raven_id <- readxl::read_excel("data/raw/ravens_banding_tagging.xlsx",sheet=1)
terr <- subset(raven_id, `status (reviewed 8/1/24)` == "territorial" & 
                 raven_id$`inside NationalPark` == "yes")$`tag-id`
trans <- subset(raven_id, `status (reviewed 8/1/24)` %like% "Trans")


#pulling territorials
terr_gps <- subset(all_gps, individual_local_identifier %in% terr)


#pulling trans territorials into their active territorial periods
#trans means that they changed their breeding status one direction or the other
trans_gps <- all_gps[all_gps$individual_local_identifier %in% trans$`tag-id`,]

trans_gps <- do.call("rbind", tapply(trans_gps, INDEX=trans_gps$individual_local_identifier, 
                                    FUN=function(x){
                                      ind <- trans[trans$`tag-id` == x[1,]$individual_local_identifier,]
                                      
                                      #has only an end date
                                      if(is.na(ind$`start date`) & !is.na(ind$`leave date`)){
                                        tmp <- x[as.Date(x$study_local_timestamp) < ym(ind$`leave date`),]
                                        return(tmp)
                                      }
                                      
                                      #has only a start date
                                      if(!is.na(ind$`start date`) & is.na(ind$`leave date`)){
                                        tmp <- x[as.Date(x$study_local_timestamp) > ym(ind$`start date`),]
                                        return(tmp)
                                      }
                                      
                                      
                                      #should be excluded
                                      if(is.na(ind$`start date`) & is.na(ind$`leave date`)){
                                        return()
                                      }
                                    }))


#combining trans and territorial datasets
#!!!SKIP this step if you want to exclude trans 
terr_gps <- rbind(terr_gps, trans_gps)


#pulling out only winter points (subject to change the month)
winter_gps <- terr_gps[month(terr_gps$study_local_timestamp) %in% c(10,11,12,3),]



# Distance to terr -------------------------------------------------------------
## distance to territory

source("scripts/Home Range (MCP).R")

mcp_in <- function(){
  ID <- mcp90$id
  
  for(i in 1:length(ID)){
    tmp_data <- subset(winter_gps, individual_local_identifier == ID[i])
    tmp_sf <- st_as_sf(tmp_data, coords=c("utm_easting", "utm_northing"), 
                       crs="+proj=utm +zone=12")
    
    #in kilometers
    tmp_data$dist_terr <- as.numeric(st_distance(tmp_sf, st_as_sf(mcp90[i,])))/1000
    
    if(i != 1){
      output_df <- rbind(output_df, tmp_data)
    } else{
      output_df <- tmp_data
    }
  }
  
  return(output_df)
}
#adding binary metric for inside/outside territory
gps_final <- mcp_in()
gps_final$within_terr <- ifelse(gps_final$dist_terr == 0, 1, 0)



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


#function to seperate out the kills that are within each territory
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
# avg_day_betwn_kill <- mutate(avg_day_betwn_kill, individual_local_identifier = rownames(avg_day_betwn_kill)) 



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
avg_kill_density <- bind_rows(kill_density, .id = "individual_local_identifier") %>% 
  group_by(individual_local_identifier) %>% 
  summarize(avg_density = mean(density))



# Active kill -------------------------------------------------------------
## presence of an active kill within the territory

##' active is < than 3 days old (<= 2 days)
##' days_since is the number of days since the kill was made, including the day of the kill

active_kill_fctn <- function(days_since = 3){
  gps_final$active_kill <- 0
  
  tapply(gps_final, gps_final$individual_local_identifier,
         FUN = function(x){
           ID <- unique(x$individual_local_identifier)
           tmp_kills <- in_terr_kill_list[[ID]]
           
           #looping through each GPS point to see if there is a active kill
           for(i in 1:nrow(x)){
             tmp_GPS <- x[i,]
             
             time_diff <- difftime(tmp_kills$DOD, as.Date(tmp_GPS$study_local_timestamp), units = "days")
             
             if(sum(time_diff >= 0 & time_diff < days_since) >= 1){
               x[i, "active_kill"] <- 1
             }
           }
           return(x)
         })
}

gps_final <- bind_rows(active_kill_fctn())



# Hunting season-------------------------------------------------------------
##   binary covariate for if the hunting season is in effect

##' FWP hunting seaosn is to down to the day
##' march for tribal bison hunting (This actually depends on bison movement)

#reading in hunting dates
hunting_dates <- readxl::read_xlsx("data/raw/hunting_seasons.xlsx")

gps_final <- gps_final %>% 
  
  #adding month, day columns
  mutate(year = year(study_local_timestamp),
         month = month(study_local_timestamp),
         day = day(study_local_timestamp)) %>% 
  
  #adding hunting end date
  left_join(hunting_dates %>% 
              dplyr::select(year, start, end),
            by = join_by(year)) %>% 
  
  #creating new boolean column for hunting season
  mutate(hunt = if_else((format(study_local_timestamp, "%m-%d") >= 
                           format(start, "%m-%d")) &
                          (format(study_local_timestamp, "%m-%d") <= 
                             format(end, "%m-%d")), 
                        1, #days in nov before end date
                        0))  #otherwise no hunting == 0 n nov before end date

  


# Hunting take ------------------------------------------------------------

#adding FWP hunting estimates
source("scripts/fwp_hunting_estimates.R")

gps_final <- gps_final %>%
  left_join(daily_count %>%
              dplyr::select(year, month, day, final_take, final_take_bms),
            by = join_by(year, month, day))



