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
  combine_cluster(distance_threshold = 1000, day_gap = 2) %>%   
  
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
 
