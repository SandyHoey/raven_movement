# categorizes the movement decisions of ravens into 3  groups
  # 1 = stay on terr
  # 3 = visit hunting region
  # 2 = visit other location

# reading in raven territories
source("scripts/home_range_mcp.R")

# reading in raven GPS data with distances to various locations
source("scripts/dist_to_gardiner.R")


# calculating distance to territory
# using 90% mcp
# distance calculated in meters
gps_in_mcp <- function(data){
  ID <- mcp90$id
  
  for(i in 1:length(ID)){
    tmp_data <- subset(data, individual_local_identifier == ID[i])
    tmp_sf <- st_as_sf(tmp_data, coords=c("utm_easting", "utm_northing"), 
                       crs="+proj=utm +zone=12")
    
    tmp_data$dist2terr <- as.numeric(st_distance(tmp_sf, st_as_sf(mcp90[i,]), unit = ))
    
    if(i != 1){
      output_df <- rbind(output_df, tmp_data)
    } else{
      output_df <- tmp_data
    }
  }
  
  return(output_df)
}

dist2poly <- gps_in_mcp(terr_fw_gps)


# getting the number of points in territory and Gardiner 
info_table <- tapply(dist2poly, INDEX = dist2poly$individual_local_identifier,
       FUN = function(x){
         info_table <- rep(NA, 4)
         names(info_table) <- c("Gardiner", "Terr", "Other", "Total")
         
         info_table[1] <- nrow(subset(x, dist2fwp == 0))
         info_table[2] <- nrow(subset(x, dist2terr == 0))
         info_table[3] <-  nrow(subset(x, dist2fwp != 0 & dist2terr != 0))
         info_table[4] <- nrow(x)
           
         return(info_table)
       })


# number of points adds up properly
# lapply(info_table, function(x){
# 
#   return(c(sum(x[1:3]), x[4]))
# })


# adding the hunting end dates
hunt_dates <- readxl::read_xlsx("data/raw/hunting_seasons.xlsx") %>%
  dplyr::select(year, end) %>% 
  rename(hunt_end = end)
dist2poly <- dist2poly %>% 
  # adding month and year columns
  mutate(year = year(study_local_timestamp),
         month = month(study_local_timestamp)) %>% 
  # column with the year of that winter (March 1, 2020 is from winter 2019) 
  mutate(winter_year = if_else(month %in% c(1:3), year-1, year)) %>% 
  # adding MTFWP hunting end date
  left_join(hunt_dates, by = join_by(winter_year == year))


# creating a column that shows if the raven decided to commute or leave terr that day
# 3 = has a least 1 point that day in Gardiner hunting polys
# 2 = has at least 1 point farther than 1 km from terr poly , but none in Gardiner poly
# 1 = only has points in terr poly
ID <- unique(dist2poly$individual_local_identifier)

commute_list <- tapply(dist2poly, INDEX = dist2poly$individual_local_identifier,
       FUN = function(x){
         
         # pulling unique dates
         dates <- unique(as.Date(x$study_local_timestamp))
         
         tmp_date_df <- data.frame(ID = x[1, "individual_local_identifier"], 
                                   winter_year = if_else(month(dates) %in% c(1:3), year(dates)-1, year(dates)),
                                   date = dates, commute = NA, dump = NA, n_point = NA) 
         
         # cycling through all the dates for each individual to tell where the
         # individual ended up that day (terr, other, Gardiner)
         for(d in 1:length(dates)){
           # filtering to all GPS points for a single day
           tmp_dayta <- x %>% filter(as.Date(study_local_timestamp) == dates[d])
           
           # changing the polygon used to determine if a hunting visit happen based on the hunting season
           # any time after the end of MTFWP season uses the smaller bison polygon
           if(x[1,"study_local_timestamp"] <= x[1, "hunt_end"]){ # if the GPS date is before the end of rifle hunting season
             if(any(tmp_dayta[,"dist2fwp"] == 0)){ # make decisions based on larger rifle hunting polygon
               tmp_date_df[d,"commute"] <- 3
             } else if(any(tmp_dayta[,"dist2terr"] > 1000)){
               tmp_date_df[d,"commute"] <- 2
             } else{
               tmp_date_df[d,"commute"] <- 1
             }
           } else{ # if its after the end of rifle hunting season
             if(any(tmp_dayta[,"dist2bison"] == 0)){ # make decisions based on the smaller bison hunting polygon
               tmp_date_df[d,"commute"] <- 3
             } else if(any(tmp_dayta[,"dist2terr"] > 1000)){
               tmp_date_df[d,"commute"] <- 2
             } else{
               tmp_date_df[d,"commute"] <- 1
             }
           }
           
           # adding the number of points that day
           tmp_date_df[d, "n_point"] <- nrow(tmp_dayta)
           
           # adding if any of the points were at the dump/sewage
           if(any(tmp_dayta[,"dist2dump"] == 0)){ 
             tmp_date_df[d, "dump"] <- TRUE
             } else(
               tmp_date_df[d, "dump"] <- FALSE
             )
           }
           
         return(tmp_date_df)
       })

commute_df <- do.call("rbind", commute_list)


# plotting individual raven commute decision per day
# layout(matrix(1:20, nrow = 4, ncol = 5))
# sapply(commute_list,
#        FUN = function(x){
#          plot(commute~date, x,
#               main = x[1, "individual_local_identifier"],
#               cex = 0.7, yaxp = c(0,3,3))
# })



# calculating the number of commute days that included a visit to the dump
  # all ravens
  commute_df %>% 
    filter(commute == 3) %>% 
    group_by(dump) %>% 
    summarise(n())
  713/(713+1980)
  # 26.5% of the time a raven visits the dump when they search the hunting area
  # individual basis
  commute_df %>% 
    filter(commute == 3) %>% 
    group_by(individual_local_identifier) %>% 
    summarise(no_visit = sum(dump == F),
              visit = sum(dump == T)) %>% 
    mutate(prop_visit_dump = visit/(visit + no_visit)) %>% 
    summarize(mean = mean(prop_visit_dump),
              min = min(prop_visit_dump),
              max = max(prop_visit_dump),
              sd = sd(prop_visit_dump))
  
  # summarizing the average time between leaving the territory and arriving on the hunting grounds
  commute_time <- dist2poly %>% 
    # restrict to days that ravens wnet to hunting area
    mutate(date = as.Date(study_local_timestamp)) %>% 
    left_join(commute_df %>% 
                # only relevant columns
                dplyr::select(individual_local_identifier, date, commute)) %>% 
    filter(commute == 3) %>% 
    # sort chronologically
    arrange(individual_local_identifier, study_local_timestamp) %>%
    # keep first point in hunting and last point before leaving terr
    group_by(individual_local_identifier, date) %>%
    # add column to tell which hunting region to use
    mutate(active_dist = if_else(study_local_timestamp <= hunt_end, 
                                 dist2fwp, dist2bison)) %>%
    # identify the first "arrival" point
    mutate(arrival_row = which(active_dist == 0)[1]) %>%
    # drop days with no valid arrival
    filter(!is.na(arrival_row)) %>%
    # keep only rows in territory up to arrival
    mutate(row_id = row_number()) %>%
    filter(row_number() == arrival_row | (row_number() < arrival_row & dist2terr == 0)) %>%
    # from the territory points, keep only the closest one before arrival
    mutate(keep = case_when(row_id == arrival_row ~ TRUE,
                            row_id == max(row_id[dist2terr == 0 & row_id < arrival_row], na.rm = TRUE) ~ TRUE,
                            TRUE ~ FALSE)) %>%
    filter(keep) %>%
    select(-active_dist, -arrival_row, -row_id, -keep) %>% 
    # drop days without 2 points
    filter(n() == 2) %>%
    # calculating commute time
    mutate(commute_time = as.numeric(difftime(study_local_timestamp[2], 
                                              study_local_timestamp[1],
                                              units = "hours"))) %>% 
    # summarize results for individuals
    group_by(individual_local_identifier) %>% 
    summarize(avg_commute = mean(commute_time))
    
  