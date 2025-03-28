#Using Hidden Markov Movement model to categorized the movements of ravens 

library(tidyverse)
library(moveHMM)
library(data.table)
library(readxl)


# reading in and subsetting data ------------------------------------------

#reading in all raven points
#removing columns with NA coords
allGPS <- read_csv("data/clean/all_raven_gps_clean58.csv")
allGPS <- subset(allGPS, !is.na(utm.easting))


#removing 7646 because there arent enough winter points
#removing 7653 and 7596 because Canada
allGPS <- subset(allGPS, individual.local.identifier != "7646")
allGPS <- subset(allGPS, individual.local.identifier != "7653")
allGPS <- subset(allGPS, individual.local.identifier != "7596")


#importing demographic information
#terr: territorial birds with nest inside Yellowstone
#trans: birds that transitioned between breeder and nonbreeder
ravenID <- read_excel("data/raw/ravens_banding_tagging.xlsx",sheet=1)
terr <- subset(ravenID, `status (reviewed 8/1/24)` == "territorial" & 
                 ravenID$`inside NationalPark` == "yes")$`tag-id`
trans <- subset(ravenID, `status (reviewed 8/1/24)` %like% "Trans")


#subsetting territorials from entire GPS df
terrGPS <- subset(allGPS, individual.local.identifier %in% terr)


#subsetting trans territorials into their active territorial periods from entire GPS df
transGPS <- allGPS[allGPS$individual.local.identifier %in% trans$`tag-id`,]

transGPS <- do.call("rbind", tapply(transGPS, INDEX=transGPS$individual.local.identifier, 
                                    FUN=function(x){
                                      ind <- trans[trans$`tag-id` == x[1,]$individual.local.identifier,]
                                      
                                      #has only an end date
                                      if(is.na(ind$`start date`) & !is.na(ind$`leave date`)){
                                        tmp <- x[as.Date(x$study.local.timestamp) < ym(ind$`leave date`),]
                                        return(tmp)
                                      }
                                      
                                      #has only a start date
                                      if(!is.na(ind$`start date`) & is.na(ind$`leave date`)){
                                        tmp <- x[as.Date(x$study.local.timestamp) > ym(ind$`start date`),]
                                        return(tmp)
                                      }
                                      
                                      
                                      #should be excluded
                                      if(is.na(ind$`start date`) & is.na(ind$`leave date`)){
                                        return()
                                      }
                                    }))


#combining trans and territorial datasets
#!!!SKIP this step if you want to exclude trans 
terrGPS <- rbind(terrGPS, transGPS)


#pulling out only winter points (subject to change the month)
GPS_W <- terrGPS[month(terrGPS$study.local.timestamp) %in% c(10,11,12,1,2,3),]



# creating covariates measurements -------------------------------------------------------

##1 adding distance to territory
source("scripts/exploratory/Home Range (MCP).R")

mcp_in <- function(){
  ID <- mcp90$id
  
  for(i in 1:length(ID)){
    tmp_data <- subset(GPS_W, individual.local.identifier == ID[i])
    tmp_sf <- st_as_sf(tmp_data, coords=c("utm.easting", "utm.northing"), 
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
dist2poly <- mcp_in()
dist2poly$within_terr <- ifelse(dist2poly$dist_terr == 0, 1, 0)



##2 time between kills within territory
##'   this may be inaccurate because kill detection goes down outside of winter
##'     study periods when the plane doesnt fly
##'     especially for interior territories
##'  variable 3 may be better (basic kill density)
##'  !!!!should probably remove cat kills from consideration

kill_data <- read.csv("data/raw/wolf_project_carcass_data.csv")
kill_data$DOD <- mdy(kill_data$DOD)

#subset to only kills from 2019 onwards to match raven GPS data
#subset to only winter months (Nov-Mar)
#but removing kills that are in Jan-Mar of 2019
kill_data %>% 
  subset(year(DOD) >= 2019 & month(DOD) %in% c(11,12,1,2,3)) %>% 
  subset(DOD >= as.Date("2019-11-01")) -> kill_data_recent 


#creating new column with the most accurate coords available
#order of accuracy ground -> aerial -> estimated ground
kill_data_recent %>% 
  mutate(easting = case_when(!is.na(GROUND.EAST) ~ GROUND.EAST,
                             !is.na(AERIAL.EAST) ~ AERIAL.EAST,
                             !is.na(EST.GROUND.EAST) ~ EST.GROUND.EAST),
         northing = case_when(!is.na(GROUND.NORTH) ~ GROUND.NORTH,
                             !is.na(AERIAL.NORTH) ~ AERIAL.NORTH,
                             !is.na(EST.GROUND.NORTH) ~ EST.GROUND.NORTH)) %>% 
  subset(!is.na(easting))-> kill_data_recent


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
      subset(kill_data_recent, tmp_dist <= dist_from_terr) %>% 
        arrange(DOD) -> in_terr_kill_list[[i]]
      
  }
  return(in_terr_kill_list)
}

in_terr_kill_list <- kill_freq(dist_from_terr = 3000)


#counting the days between consecutive kills within each territory
#Summer is messing up days since 
day_betwn_kill <- lapply(in_terr_kill_list, function(x){
  #empty vector to attach all the values of days since previous carcass
  days_since <- c()
  
  #start and end dates for each winter period
  winter_start <- as.Date(paste0(seq(min(year(x$DOD))-1, max(year(x$DOD))),"-11-01"))
  winter_end <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))+1),"-03-31"))
  
  for(w in 1:length(winter_start)){
    
    tmp_winter <- subset(x, DOD >= winter_start[w] & 
                           DOD <= winter_end[w])
    
    #has an NA value since the first carcass of a winter period cant have a "days since last carcass"
    if(nrow(tmp_winter) == 1){
      days_since <- c(days_since, NA)
    }else if(nrow(tmp_winter) == 0){
    }else{
      days_since <- c(days_since, NA, diff(tmp_winter$DOD))
    }
  }

  x$days_since <- days_since
})


#calculating average day between kills for individuals
day_betwn_kill %>% 
  lapply(mean, na.rm = T) %>% 
  do.call("rbind",.) %>% 
  as.data.frame() -> avg_day_betwn_kill
colnames(avg_day_betwn_kill) <- "avg_day_btwn"
avg_day_betwn_kill <- mutate(avg_day_betwn_kill, individual.local.identifier = rownames(avg_day_betwn_kill)) 



##3 kill density
##'   # of carcasses in territory/# of days(30)
##'   going to be calculated only for winter studies when kill detection is best
##'   !!!!should probably remove cat kills from consideration

kill_density <- lapply(in_terr_kill_list, function(x){
  #empty vector to attach all the values of days since previous carcass
  days_since <- c()
  
  #start and end dates for each winter period
  early_winter_start <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))),"-11-15"))
  early_winter_end <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))),"-12-15"))
  late_winter_start <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))),"-03-01"))
  late_winter_end <- as.Date(paste0(seq(min(year(x$DOD)), max(year(x$DOD))),"-03-30"))
  
  
  #a dataframe to put the kill density numbers for each winter sample period
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
})

#am going to use average kill density for each individual
#the kill density is calculated from winter study periods, so there isn't a number for
  #the other months anyways
bind_rows(kill_density, .id = "individual.local.identifier") %>% 
  group_by(individual.local.identifier) %>% 
  summarize(avg_density = mean(density)) -> avg_kill_density



#4 presence of an active kill within the territory
##' active is less than 3 days old
##' days_since is the number of days since the kill was made, including the day of the kill

active_kill_fctn <- function(days_since = 3){
  dist2poly$active_kill <- 0
  
  tapply(dist2poly, dist2poly$individual.local.identifier,
       FUN = function(x){
         ID <- unique(x$individual.local.identifier)
         tmp_kills <- in_terr_kill_list[[ID]]
         
         #looping through each GPS point to see if there is a active kill
         for(i in 1:nrow(x)){
           tmp_GPS <- x[i,]
           
           time_diff <- difftime(tmp_kills$DOD, as.Date(tmp_GPS$study.local.timestamp), units = "days")
           
           if(sum(time_diff >= 0 & time_diff < days_since) >= 1){
             x[i, "active_kill"] <- 1
           }
         }
         return(x)
       })
}
  
dist2poly <- bind_rows(active_kill_fctn())

##3 hunting season
##'   binary covariate for if the hunting seaosn is in effect
##'   Oct 26 - Dec 1 for general ungulate season (FWP)
##'   march for tribal bison hunting (double check this, its probably longer)
monthday <- format(dist2poly$study.local.timestamp, "%m-%d")
dist2poly$hunt <- 0
dist2poly[which(monthday >= "10-26" & monthday < "12-1" |
                  monthday >= "03-01" & monthday <= "03-30"),]$hunt <- 1


# Data check --------------------------------------------------------------
# #checking to see how regular the time intervals are between points
# #only for the breeders 
# #difference in minutes (it says seconds, but its not)
# ID <- unique(GPS_W$tag.local.identifier)
# for(i in 1:length(ID)){
#   
#   #subsetting an individual
#   tmp_GPS_ID <- subset(GPS_W, tag.local.identifier == ID[i])
#   
#   days <- unique(date(tmp_GPS_ID$study.local.timestamp))
#   
#   
#   for(d in 1:length(days)){
#     
#     #subsetting a single day
#     tmp_GPS_day <- subset(tmp_GPS_ID, date(study.local.timestamp) == days[d])
#     
#     if(d == 1 & i == 1){
#       #finding time difference between consecutive points
#       time_diff <- diff(tmp_GPS_day$study.local.timestamp, )/60
#     } else{
#       time_diff <- c(time_diff, diff(tmp_GPS_day$study.local.timestamp)/60)
#       
#     }
#   }
# }
# 
# 
# hist(as.numeric(time_diff))
# range(time_diff)
# quantile(time_diff, probs = .96)
# time_diff[which(time_diff >500)]
# length(time_diff)
# #going to use 58 minute minimum time between points
# #this keeps 96% of the values at 1 hour
# #has a few thousand more points than 59 minutes
# #it is half the total number of points as 29 minutes




# Model fitting -----------------------------------------------------------

#subsetting to only relevant columns
input_data <- dist2poly[,c("utm.easting", "utm.northing", "individual.local.identifier", 
                           "dist_terr", "within_terr", "active_kill", "hunt")]

#merging the other covariates
##' covariate 2: average day between kills within territory (not currently included because lots of NaN values)
##' covariate 3: average kill density within territory
#input_data <- left_join(input_data, avg_day_betwn_kill, by = "individual.local.identifier")
input_data <- left_join(input_data, avg_kill_density, by = "individual.local.identifier")

## prepping data
#standardize scale of all covariates
input_data <- cbind(input_data, scale(input_data[,-c(1:3)]))
names(input_data) <- c(c("x", "y", "ID"),
                       names(input_data)[4:8], 
                       paste0(names(input_data)[-c(1:8)], "_scale"))

#everything in kilometers
input_data$x <- input_data$x/1000
input_data$y <- input_data$y/1000

#adding dist_terr_scale^2
input_data <- input_data %>% 
  mutate(dist_terr_scale_sqrd = dist_terr_scale^2)

#transform dataframe into HMM compatible 
input_data <- prepData(input_data, type = "UTM")
#!! there are some birds with low points I should probably remove
# as of 1/14/2025
# 8902: 9 points
# 7489_2 87 points


## fitting model (2 state: feed, travel)
hist(input_data$step)
stepMean0 <- c(1, 20)
stepSD0 <- c(1, 20)
stepPar0 <- c(stepMean0, stepSD0)

hist(input_data$angle)
turnMean0 <- c(pi, 0)
#concentration: how clustered values are around the mean
#higher number = more concentrated around mean
turnCon0 <- c(1, 20)
turnPar0 <- c(turnMean0, turnCon0)

#running the model 
(model_2s <- fitHMM(input_data, nbStates = 2, stepPar0 = stepPar0,
                   anglePar0 = turnPar0, 
                   formula = ~active_kill_scale + avg_density_scale + 
                     dist_terr_scale_sqrd + hunt))

CI(model_2s, alpha = 0.95)


#defining the state of each point within the input_data
#then plotting individual GPS points
input_data$vit <- viterbi(model_2s)
#plot(y ~ x, data = input_data, col = vit)


## square distance to territory to get at the bimodal foraging (at terr and gardiner)

#covariates
# binary value for if the bird is within its territory or within gardiner
# interaction for binary territory and kill frequency within territory
# distance to nearest carcass
# scale() then square

# test plotting for stationary variable from Sarah B ----------------------

# tries to pull out probably of being in a certain state give the covariates

# hmmm = HMM model output
#'  Function to extract stationary state probabilities & plot predicted responses
stay_probs <- function(hmmm) {
  stay_pr <- stationary(hmmm, covs = data.frame(active_kill_scale = 0,
                                              avg_density_scale = 0,
                                              dist_terr_scale_sqrd = 0,
                                              hunt = 0))
  stay_pr <- stay_pr[[1]]
  #data frame of the covariates (standardized)
  stay_mu0 <- stationary(hmmm, covs = data.frame(active_kill_scale = 0,
                                                 avg_density_scale = 0,
                                                 dist_terr_scale_sqrd = 0,
                                                 hunt = 0))
  print(stay_mu0)
  
  #'  Plot stationary state probabilities and extract predicted estimates
  fig <- plotStationary(hmmm)
  stationary_probs <- list(stay_pr, fig)
  
  return(stationary_probs)
}

#stay_probs(MODEL NAME)

stay_smr <- stay_probs(model_2s)

