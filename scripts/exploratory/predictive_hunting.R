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


# #importing demographic information
# #nb: nonbreeding birds
# #trans: birds that transitioned between breeder and nonbreeder
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
  
  #remove days that don't have 4 points in Gardiner (the min for a GPS cluster)
  group_by(AID) %>%
  filter(n() >= 2) %>% 
  as.data.frame %>% 
  
  arrange(AID)

#getting the sunrise and sunset times for Gardiner
daytime <- getSunlightTimes(date = unique(as.Date(cluster_raw$TelemDate)),
                 lat =  45.031173,
                 lon = -110.703206,
                 keep = c("sunrise", "sunset"),
                 tz = "MST")


#adding missed fixes to the data
id_date <- unique(cluster_raw$AID)
  #creating new dataframe that will include the missed fixes
  cluster_NA <- cluster_raw

  for(i in 1:length(id_date)){
  #pulling out specific individual and day
  tmp <- subset(cluster_raw, AID == id_date[i])
  
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
    cluster_raw <- cluster_raw %>% 
      bind_rows(data.frame(TelemDate = missing, Lat = NA, Long = NA, AID = tmp$AID[1])) %>% 
      arrange(AID)
  }

}
  
  
#actually defining clusters
cluster_output <- GPSeq_clus(cluster_raw,
                       search_radius_m = 50,
                       window_days = 1,
                       clus_min_locs = 4,
                       show_plots = c(F, "mean"))

#pulling out clusters
clusters <- cluster_output[[2]]

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
  
#adding utm coordinates
cluster_hunt <- cluster_hunt %>% 
  st_as_sf(coords=c("g_c_Long", "g_c_Lat"),
           crs="epsg:4326") %>% 
  st_transform(crs = "+proj=utm +zone=12") %>% 
  mutate(easting = st_coordinates(geometry)[,1], 
         northing = st_coordinates(geometry)[,2])

#combining cluster points that are the same
  #on consecutive days
  #within 50 meters
combine_cluster <- function(gps_sf, datetime_col = "clus_start", 
                                 distance_threshold = 50, day_gap = 2) {

  library(igraph)

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
  g <- graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(id = 1:n))
  comps <- components(g)
  
  gps_sf$group_id <- NA_integer_
  gps_sf$group_id[as.integer(V(g)$name)] <- comps$membership
  
  # Handle unconnected (isolated) points
  missing <- which(is.na(gps_sf$group_id))
  if (length(missing) > 0) {
    gps_sf$group_id[missing] <- seq(max(gps_sf$group_id, na.rm = TRUE) + 1,
                                    length.out = length(missing))
  }
  return(gps_sf)
}

cluster_roost <- combine_cluster(cluster_hunt) %>% 

#removing clusters that are probably roosts
#looking at number of night points
#night is defined in the cluster algorithm as suncalc() sunrise/sunset
  filter(night_prop < .2) %>% 
  
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


# #plotting   
# # Ensure group_id is a factor (so each group gets a unique color)
# gps_data <- gps_data %>%
#   mutate(group_id = as.factor(group_id))
# 
# # Plot on satellite map
mapview::mapview(cluster_final %>% filter(year(start_date) == 2024), map.type = "Esri.WorldImagery")

#how many clusters per year
table(year(cluster_final$start_date))

#how many individuals giving GPS per year
a <- cluster_raw %>% 
  filter(year(TelemDate) > 2018) %>% 
  group_by(year(TelemDate)) %>% 
  summarise(ind_per_year = length(unique(individual_local_identifier)))

#number of ravens that visited each cluster
b <- cluster_roost %>% 
  mutate(ID = sub("(_\\d{4}-\\d{2}-\\d{2})$", "", AID)) %>% 
  st_drop_geometry %>% 
  group_by(group_id) %>% 
  summarise(ind = length(unique(ID)), year = mean(year(clus_start))) %>% 
  group_by(year) %>% 
  summarise(mean(ind))

#plotting "number of tagged individuals" vs "average individuals per cluster"
plot(x = a$ind_per_year, y = b$`mean(ind)`)
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
