#predicting where hunter kills are using raven GPS data
#METHODS
  #cluster
    #' find clusters across all space, then restrict clusters to hunting areas
  #HMM
    #' couldn't get to work

library(dplyr)
library(moveHMM)
library(data.table)
library(sf)
library(GPSeqClus)
library(suncalc)
library(lubridate)


# reading in and subsetting data ------------------------------------------

#reading in all raven points
allGPS <- readr::read_csv("data/clean/all_raven_gps_clean28.csv") %>% 
  
  #removing rows with NA coords
  filter(!is.na(utm_easting)) %>% 
  
  #filter to only winter 
  filter(month(study_local_timestamp) %in% 11:12)


# #removing 7653 and 7596 because Canada
# allGPS <- subset(allGPS, individual_local_identifier != "7653")
# allGPS <- subset(allGPS, individual_local_identifier != "7596")


#importing demographic information
#nb: nonbreeding birds
#trans: birds that transitioned between breeder and nonbreeder
# ravenID <- readxl::read_excel("data/raw/ravens_banding_tagging.xlsx",sheet=1)
# nb <- subset(ravenID, `status (reviewed 8/1/24)` == "vagrant")$`tag-id`
# trans <- subset(ravenID, `status (reviewed 8/1/24)` %like% "Trans")
# 
# 
# #subsetting territorials from entire GPS df
# nbGPS <- subset(allGPS, individual_local_identifier %in% nb)
# 
# 
# #subsetting trans territorials into their active territorial periods from entire GPS df
# transGPS <- allGPS[allGPS$individual_local_identifier %in% trans$`tag-id`,]
# 
# transGPS <- do.call("rbind", tapply(transGPS, INDEX=transGPS$individual_local_identifier,
#                                     FUN=function(x){
#                                       ind <- trans[trans$`tag-id` == x[1,]$individual_local_identifier,]
# 
#                                       #has only an end date
#                                       if(is.na(ind$`start date`) & !is.na(ind$`leave date`)){
#                                         tmp <- x[as.Date(x$study_local_timestamp) > lubridate::ym(ind$`leave date`),]
#                                         return(tmp)
#                                       }
# 
#                                       #has only a start date
#                                       if(!is.na(ind$`start date`) & is.na(ind$`leave date`)){
#                                         tmp <- x[as.Date(x$study_local_timestamp) < lubridate::ym(ind$`start date`),]
#                                         return(tmp)
#                                       }
# 
# 
#                                       #should be excluded
#                                       if(is.na(ind$`start date`) & is.na(ind$`leave date`)){
#                                         return()
#                                       }
#                                     }))
# 
# 
# #combining trans and territorial datasets
# #!!!SKIP this step if you want to exclude trans
# terrGPS <- rbind(nbGPS, transGPS)




# #transforming raven points to sf dataframe
# #allows a distance metric to the polygon to be calculated
# sf_raven <- st_as_sf(gps_w, coords=c("utm_easting", "utm_northing"),
#                       crs="+proj=utm +zone=12")
# 
# 
# ##restricting area to only GPS points in Gardiner




# cluster -----------------------------------------------------------------
cluster_raw <- allGPS %>% 
  
  #pulling out only necessary data for clustering
  dplyr::select(study_local_timestamp, location_lat, location_long, individual_local_identifier) %>% 
  mutate(AID = paste0(individual_local_identifier, "_", as.Date(study_local_timestamp)),
         study_local_timestamp = force_tz(study_local_timestamp, tz = "MST")) %>% 
  rename(TelemDate = study_local_timestamp,
         Long = location_long,
         Lat = location_lat) %>%
  
  #remove days that don't have 2 points in Gardiner (the min for a GPS cluster)
  group_by(AID) %>%
  filter(n() >= 4) %>% 
  as.data.frame %>% 
  
  arrange(AID)

#getting the sunrise and sunset times for Gardiner
daytime <- getSunlightTimes(date = unique(as.Date(cluster_raw$TelemDate)),
                 lat =  45.031173,
                 lon = -110.703206,
                 keep = c("sunrise", "sunset"),
                 tz = "MST")


#adding missed fixes to the data
  #unique ID date_combos
  id_date <- unique(cluster_raw$AID)
  
  #creating new dataframe that will include the missed fixes
  cluster_NA <- cluster_raw

  for(i in 1:length(id_date)){
  #pulling out specific individual and day
  tmp <- subset(cluster_NA, AID == id_date[i])
  
  #rounding the time of the GPS points to nearest 30 minute
  round_time <- round_date(tmp$TelemDate, unit = "30 minutes")
  
  #pulling out the sun times for the day
  tmp_sun <- subset(daytime, date == as.Date(tmp$TelemDate[1]))
  
  #creating sequence of 30 minutes from sun times
  time_seq <- seq(from = ceiling_date(tmp_sun$sunrise, unit = "30 minutes"), 
                  to = floor_date(tmp_sun$sunset, unit = "30 minutes"), 
                  by = "30 mins")
  
  #finding which GPS fixes were missed
  missing <- time_seq[-which(time_seq %in% round_time)]
  
  #adding missed GPS fixes back into dataset
    #doesn't run if no times are missing
  if(length(missing > 0)){
    cluster_NA <- cluster_NA %>% 
      bind_rows(data.frame(TelemDate = missing, Lat = NA, Long = NA, AID = tmp$AID[1])) %>% 
      arrange(AID)
  }

}
  
  
#actually defining clusters
cluster_output <- GPSeq_clus(cluster_NA,
                       search_radius_m = 50,
                       window_days = 1,
                       clus_min_locs = 4,
                       show_plots = c(F, "mean"))

#pulling out clusters
clusters <- cluster_output[[2]] %>% 
  
  #adding ID
  mutate(ID = sub("(_\\d{4}-\\d{2}-\\d{2})$", "", AID))

#transforming to sf class
#allows a distance metric to the polygon to be calculated
sf_cluster <- clusters %>% 
  st_as_sf(coords=c("g_c_Long", "g_c_Lat"),
           crs="epsg:4326")

#reading in gardiner polygon
gardiner_poly <- st_read("data/raw/gardiner_hunt.kml") %>%
  st_transform(crs = st_crs(sf_cluster))

#calculating distance from points to polygon
clusters$dist2Gardiner <- as.numeric(st_distance(sf_cluster, gardiner_poly))

#subsetting to only 0 distance (in the polygon)
cluster_hunt <- subset(clusters, dist2Gardiner == 0)

######################################
#sampling number of individuals so its the same number through all years
#testing 2019-2022 with 7 individuals
ind_year <- cluster_hunt %>%
  filter(year(clus_start) %in% c(2019:2022)) %>%
  group_by(year(clus_start)) %>%
  reframe(ID = unique(ID))

colnames(ind_year) <- c("year", "ID")

#individuals to remove in certain years
#2019: 7490
#2020: 7672, 7655 7637, 7494
#2021: 7563, 7561, 7487, 7648
#2022: 8900, 7487, 7492 probably just keep all of these since # of individuals is so low

set.seed(10)
ind_sample <- ind_year %>%
  
  # #remove these low point individuals
  # filter(!(year == 2019 & ID == "7490"),
  #        !(year == 2020 & ID %in% c("7672", "7655", "7637", "7494")),
  #        !(year == 2021 & ID %in% c("7563", "7561", "7487", "7648"))) %>% 
  
  #manually choosing individuals to optimize days and points per day (2019-2021)
  filter((year == 2019 & ID %in% c("7658", "7643", "7645", "7493", "7492", "7484", "7489", "7495", "7487", "7637", "7488")) |
           (year == 2020 & ID %in% c("7532", "7645", "7487", "7564", "7668", "7492", "7489", "7649", "7661", "7488", "7663")) |
           (year == 2021 & ID %in% c("7661", "7490_3", "7645_2", "7656", "7668", "7492", "7638", "7652_2", "7675_2", "7663", "7667_2"))) %>% 
  
  group_by(year) %>%
  group_split() 
# %>%
#   lapply(function(x){
#     sample_ID <- sample(x$ID, 7)
#   })

names(ind_sample) <- c(2019:2021)

cluster_sample <- clusters %>% slice(0)

for(i in 1:length(ind_sample)){
  tmp_data <- subset(clusters, year(clus_start) == names(ind_sample)[i])
  tmp_sample <- ind_sample[[i]]

  cluster_sample <- subset(tmp_data, tmp_data$ID %in% tmp_sample$ID) %>%
    bind_rows(cluster_sample)
}


######################################

#adding utm coordinates
cluster_sample <- cluster_sample %>% 
  st_as_sf(coords=c("g_c_Long", "g_c_Lat"),
           crs="epsg:4326") %>% 
  st_transform(crs = "+proj=utm +zone=12") %>% 
  mutate(easting = st_coordinates(geometry)[,1], 
         northing = st_coordinates(geometry)[,2])

#combining cluster points that are the same
  #on consecutive days
  #within 50 meters
combine_cluster <- function(gps_sf, datetime_col = "clus_start", 
                                 distance_threshold, day_gap) {


  # Ensure datetime column exists
  if (!datetime_col %in% names(gps_sf)) stop("datetime column not found.")
  
  # Extract date from datetime
  gps_sf <- gps_sf %>%
    mutate(date = as.Date(.data[[datetime_col]]))
  
  n <- nrow(gps_sf)
  coords <- st_coordinates(gps_sf)
  dates <- gps_sf$date
  
  # Build edge list based on distance and date proximity
  edges <- data.frame(from = integer(), to = integer())
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (abs(dates[i] - dates[j]) <= day_gap) {
        dist_ij <- sqrt(sum((coords[i, ] - coords[j, ])^2))
        if (dist_ij <= distance_threshold) {
          edges <- rbind(edges, data.frame(from = i, to = j))
        }
      }
    }
  }
  
  # Build graph and assign connected components
  g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(id = 1:n))
  comps <- igraph::components(g)
  
  gps_sf$group_id <- NA_integer_
  gps_sf$group_id[as.integer(igraph::V(g)$name)] <- comps$membership
  
  # Handle unconnected (isolated) points
  missing <- which(is.na(gps_sf$group_id))
  if (length(missing) > 0) {
    gps_sf$group_id[missing] <- seq(max(gps_sf$group_id, na.rm = TRUE) + 1,
                                    length.out = length(missing))
  }
  return(gps_sf)
}

cluster_roost <- combine_cluster(cluster_sample, 
                                 distance_threshold = 1000, day_gap = 2) %>% 

#removing clusters that are probably roosts
#looking at number of night points
#night is defined in the cluster algorithm as suncalc() sunrise/sunset
  filter(night_prop < .1) %>% 
  
  #only using days in early winter study 11-15 to 12-15
  filter(clus_end >= as.Date(paste(year(clus_end), 11, 15, sep = "-")),
         clus_start <= as.Date(paste(year(clus_start), 12, 15, sep = "-")))


#creating a new dataframe with the start and stop times for each cluster
cluster_meta <- data.frame(group_id = unique(cluster_roost$group_id))


cluster_final <- cluster_roost %>% 
  st_drop_geometry %>% 
  group_by(group_id) %>% 
  summarise(start_date = as.Date(min(clus_start)),
            end_date = as.Date(max(clus_start)),
            easting = mean(easting),
            northing = mean(northing)) %>% 
  right_join(cluster_meta, by = join_by(group_id)) %>% 
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12")


# testing cluster at wolf kills -------------------------------------------

wolf_kills <- readr::read_csv("data/raw/wolf_project_carcass_data.csv") %>% 
  janitor::clean_names() %>% 
  
  #fix DOD format and add year column
  mutate(dod = mdy(dod),
         year = year(dod)) %>% 
  
  #filter year and wolf kill probability
  filter(year %in% 2019:2021,
         cod %in% c("DEFINITE WOLF", "PROBABLE WOLF", "POSSIBLE WOLF") |
           kill_type == "SCAVENGE FRESH CARCASS") %>% 
  
  #create column with most accurate utm locations
  mutate(easting = case_when(!is.na(ground_east) ~ ground_east,
                             !is.na(aerial_east) ~ aerial_east,
                             !is.na(est_ground_east) ~ est_ground_east),
         northing = case_when(!is.na(ground_north) ~ ground_north,
                              !is.na(aerial_north) ~ aerial_north,
                              !is.na(est_ground_north) ~ est_ground_north)) %>% 
  subset(!is.na(easting)) %>% 
  
  #create sf object
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12") %>% 
  
  #transform to match sf_cluster
  st_transform(crs = st_crs(sf_cluster))


#metric of how clusters match wolf kills
  
  #reading in northern range polygon
  sf_cluster$nrange <- st_read("data/raw/northern_range_poly.kml") %>%
    st_transform(crs = st_crs(sf_cluster)) %>% 
    st_make_valid %>% 
    st_distance(sf_cluster, .) %>% 
    as.numeric
  
  #reading in YNP boundary polygon
  sf_cluster$park <- st_read("data/raw/parkpoly.kml") %>%
    st_transform(crs = st_crs(sf_cluster)) %>% 
    st_make_valid %>% 
    st_distance(sf_cluster, .) %>% 
    as.numeric
  
  #function to compare to wolf kills
  check_proximity_time <- function(max_dist_m, max_days) {
    # Ensure both are in the same CRS: transform wolf_kills to cluster_park CRS if needed
    if (st_crs(cluster_park) != st_crs(wolf_kills)) {
      wolf_kills <- st_transform(wolf_kills, st_crs(cluster_park))
    }
    
    result <- logical(nrow(cluster_park))
    
    for (i in seq_len(nrow(cluster_park))) {
      row1 <- cluster_park[i, ]
      
      if (is.na(row1$start_date)) {
        result[i] <- FALSE
        next
      }
      
      # Filter wolf_kills rows by date range relative to row1$start_date
      time_match <- wolf_kills %>%
        filter(
          dod <= row1$start_date,
          dod + days(max_days) >= row1$start_date
        )
      
      if (nrow(time_match) == 0) {
        result[i] <- FALSE
        next
      }
      
      # Calculate distances on lon/lat (st_distance on geographic will return meters)
      dists <- st_distance(row1$geometry, time_match$geometry)
      dists <- as.numeric(dists)
      
      result[i] <- any(dists <= max_dist_m)
    }
    
    return(result)
  }
  
  
#pipe to get metrics of cluster accuracy  
cluster_park <- sf_cluster %>% 
  
  #only inside the park
  filter(park == 0, nrange == 0) %>% 
  
  #combining clusters
  combine_cluster(distance_threshold = 200, day_gap = 2) %>%   
  
  #remove roost
  filter(night_prop == 0) %>% 
  
  #only using days in early winter study 11-15 to 12-15
  filter(clus_end >= as.Date(paste(year(clus_end), 11, 15, sep = "-")),
         clus_start <= as.Date(paste(year(clus_start), 12, 15, sep = "-"))) %>% 
  
  st_transform(crs = "+proj=utm +zone=12") %>% 
  mutate(.,
         easting = st_coordinates(.)[1],
         northing = st_coordinates(.)[2]) %>% 
  group_by(group_id) %>% 
  summarise(start_date = as.Date(min(clus_start)),
            end_date = as.Date(max(clus_start)),
            easting = mean(easting),
            northing = mean(northing)) %>% 
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=12") %>% 
  st_transform(crs = 4326)

#calculating if there is a wolf kill within 200 m and 2 days before 
cluster_park <- cluster_park %>% 
  mutate(wolf_check = check_proximity_time(max_dist_m = 200, max_days = 2))

#summary stats
nrow(cluster_park)
sum(cluster_park$wolf_check == T)/nrow(cluster_park)

#plot against raven clusters
mapview::mapview(wolf_kills %>%
                   mutate(month = month(dod)) %>% 
                   filter(year == 2020, month %in% 11:12) %>% 
                   dplyr::select(geometry),
                 col.region = "red") +
  mapview::mapview(sf_cluster %>%
                     mutate(year = year(clus_start)) %>% 
                     filter(year == 2020) %>% 
                     dplyr::select(geometry),
                   col.region = "blue")
# QA/QC -------------------------------------------------------------------
# # Ensure group_id is a factor (so each group gets a unique color)
# gps_data <- gps_data %>%
#   mutate(group_id = as.factor(group_id))
# 
# # Plot on satellite map
#mapview::mapview(cluster_final %>% filter(year(start_date) == 2024), map.type = "Esri.WorldImagery")


#how many clusters per year
table(year(cluster_final$start_date))


#how many individuals giving GPS per year
(a <- cluster_roost %>% 
    filter(year(clus_start) > 2018) %>% 
    group_by(year(clus_start)) %>% 
    summarise(ind_per_year = length(unique(ID))))


#average number of ravens that visited each cluster
(b <- cluster_roost %>% 
  st_drop_geometry %>% 
  group_by(group_id) %>% 
  summarise(ind = length(unique(ID)), year = mean(year(clus_start))) %>% 
  group_by(year) %>% 
  summarise(mean(ind)))


#clusters per individual
as.vector(table(year(cluster_final$start_date)))/a$ind_per_year


#number of points in each year from these individuals
total_GPS_points <- c(NA,NA,NA,NA)
days_per_year <- c(NA,NA,NA,NA)
for(i in 1:length(ind_sample)){
  tmp_data <- subset(cluster_raw, year(TelemDate) == names(ind_sample)[i])
  tmp_sample <- ind_sample[[i]]
  
  total_GPS_points[i] <- tmp_data[tmp_data$individual_local_identifier %in% tmp_sample$ID,] %>% 
    nrow
  days_per_year[i] <- length(unique(as.Date(tmp_data$TelemDate)))
  
}

#number of GPS points each year in Gardiner
gardiner_GPS <- cluster_raw %>% 
  st_as_sf(coords=c("Long", "Lat"),
           crs="epsg:4326") %>% 
  mutate(., dist2Gardiner = as.numeric(st_distance(., gardiner_poly))) %>% 
  filter(dist2Gardiner == 0)

gardiner_GPS_points <- c(NA,NA,NA,NA)
gardiner_days_per_year <- c(NA,NA,NA,NA)
for(i in 1:length(ind_sample)){
  tmp_data <- subset(gardiner_GPS, year(TelemDate) == names(ind_sample)[i])
  tmp_sample <- ind_sample[[i]]
  
  gardiner_GPS_points[i] <- tmp_data[tmp_data$individual_local_identifier %in% tmp_sample$ID,] %>% 
    nrow
  gardiner_days_per_year[i] <- length(unique(as.Date(tmp_data$TelemDate)))
  
}


#look at missed fixes
missed_fix <- cluster_NA %>% 
  
  #adding ID
  mutate(ID = sub("(_\\d{4}-\\d{2}-\\d{2})$", "", AID),
         year = year(TelemDate)) %>% 
  
  filter((year == 2019 & ID %in% c("7658", "7643", "7645", "7493", "7492", "7484", "7489", "7495", "7487", "7637", "7488")) |
           (year == 2020 & ID %in% c("7532", "7645", "7487", "7564", "7668", "7492", "7489", "7649", "7661", "7488", "7663")) |
           (year == 2021 & ID %in% c("7661", "7490_3", "7645_2", "7656", "7668", "7492", "7638", "7652_2", "7675_2", "7663", "7667_2"))) %>% 
  
  
  group_by(ID, year) %>% 
  
  summarise(n = n(),
            missed_fix = sum(is.na(Lat)),
            prop_na = sum(is.na(Lat))/n()) %>% 
  arrange(ID)

missed_fix %>% 
  group_by(year) %>% 
  summarise(n_mean = mean(n),
            na_prop_mean = mean(prop_na))



#looking at the number of fixes per day (doesn't include missed fixes)
fix_per_day <- cluster_raw %>% 
  mutate(year = year(TelemDate)) %>% 
  group_by(individual_local_identifier, year) %>% 
  summarise(n_day = length(unique(AID)),
            mean_points_per_day = n()/length(unique(AID)))
  
fix_per_day %>% 
  group_by(year) %>% 
  summarise(day_mean = mean(n_day),
            mean_points_per_day = mean(mean_points_per_day))

fix_year_group <- fix_per_day %>% 
  ungroup %>%
  group_by(year) %>% 
  group_split() %>% 
  purrr::set_names(2018:2024)

fix_year_group %>% 
  lapply(function(x){hist(x$mean_points_per_day, main = x[1,"year"])})
#getting a lot more points per day in 2019

#restricting to only individuals that have clusters in Gardiner
gardiner_fix_year <- cluster_hunt %>%
  st_drop_geometry %>% 
  mutate(year = year(clus_start)) %>% 
  group_by(year(clus_start)) %>% 
  dplyr::select(ID, year) %>% 
  group_split %>%
  purrr::set_names(2019:2024) %>% 

  {lapply(fix_year_group, function(x){
    if(unique(x$year) != 2018){
      x[x$individual_local_identifier %in% .[[paste(unique(x$year))]]$ID,]
    }

  })}
  

  


#duration of cluster
cluster_final %>% 
  st_drop_geometry() %>% 
  mutate(year = year(start_date),
         duration = as.numeric(difftime(end_date, start_date, unit = "days"))) %>% 
  group_by(year) %>% 
  group_split() %>% 
  # summarise(duration_mean = mean(duration),
  #           duration_max = max(duration),
  #           duration_min = min(duration)) %>% 
  lapply(function(x){hist(x$duration, main = x[1,"year"])})
  

#number of daily clusters (before grouping)
cluster_hunt %>%  #only Gardiner
  st_drop_geometry %>% 
  mutate(day = as.Date(clus_start),
         year = year(clus_start)) %>% 
  group_by(year, day) %>% 
  summarise(cluster_per_day = length(AID)) %>% 
  ungroup(day) %>% 
  summarise(mean_clus_per_day = mean(cluster_per_day))

clusters %>% #all clusters
  mutate(day = as.Date(clus_start),
         year = year(clus_start)) %>% 
  group_by(year, day) %>% 
  summarise(cluster_per_day = length(AID)) %>% 
  ungroup(day) %>% 
  summarise(mean_clus_per_day = mean(cluster_per_day))
  

### NEXT STEPS
# (done) check the number of missed fixes for each individual/year. Make sure the fix rate stays high
#look at the duration of clusters in each year
#look at the clusters that happen inside the park
  #do the clusters characteristics look the same (duration)
#do model validation with wolf kills to make sure clusters are picking up things
  #start with larger radius and then get slower, see what clusters are lost 


#when sampling to 7 individuals only (to match number of ind in 22), the number in 2019 and 2020 stay higher
#the number of total points (removing points on days with <4 gps) don't follow the trend
#especially for GPS points in Gardiner, there are actually more GPS points in 21, 22 when there are less clusters
#all days have some coverage


    

  
# HMM ---------------------------------------------------------------------
# 
# #prepping data for HMM package
# input_data <- gps_final %>% 
#   
#   #creating a ID column moveHMM can recognize
#   mutate(ID = paste0(individual_local_identifier, "_", as.Date(study_local_timestamp))) %>%
#   
#   #selecting only relevant columns
#   dplyr::select(ID, utm_easting, utm_northing) %>% 
#   
#   #removing days for individuals when there are less than 3 GPS points
#   group_by(ID) %>% 
#   filter(n() > 3) %>% 
#   
#   #prepping data with moveHMM function
#   prepData(type = "UTM", coordNames = c("utm_easting", "utm_northing"))
# 
# 
# 
# ## initial model parameters
#   #step
#   hist(input_data$step)
#   stepMean0 <- c(1, 1000)
#   stepSD0 <- c(1, 10)
#   stepPar0 <- c(stepMean0, stepSD0)
# 
#   #turn angle
#   hist(input_data$angle)
#   turnMean0 <- c(pi, 0)
#   #concentration: how clustered values are around the mean
#   #higher number = more concentrated around mean
#   turnCon0 <- c(1, 10)
#   turnPar0 <- c(turnMean0, turnCon0)
# 
#   
# ## running the model 
# (model_3s <- fitHMM(input_data, nbStates = 2,
#                     stepPar0 = stepPar0, anglePar0 = turnPar0))
# 
# #determining state of each point
# input_data$vit3 <- viterbi(model_3s)
# plot(y ~ x, data = input_data, col = vit)
