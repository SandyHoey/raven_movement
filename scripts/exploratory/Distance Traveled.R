#maximum daily displacement
#Step length per hour
#only done for breeders in the park

library(tidyverse)
library(readxl)
library(data.table) #use of %like%
library(sp)


#reading in all raven points
#removing columns with NA coords
allGPS <- read_csv("data/clean/all_raven_gps_clean29.csv")
allGPS <- subset(allGPS, !is.na(utm.easting))


#importing demographic information
#terr: territorial birds inside the park (inclduing interrior birds)
#trans: birds that transitioned between breeder and nonbreeder
#7485 (Old Faithful) currently not included because she transitioned more than once
#have to consider interior birds commuting to West instead of Gardiner 
ravenID <- read_excel("ravens_banding_tagging.xlsx",sheet=1)
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



#subsetting the GPS points to only fall/winter (Nov-Dec)
terrfwGPS <- terrGPS[month(terrGPS$study.local.timestamp) %in% c(11, 12, 1, 2, 3),]



#gathering bird ID
#not creating any restrictions for number of points
#will do restrictions for number of days with 2 or more points
fw_ID <- unique(terrfwGPS$individual.local.identifier)


#creating empty lists to place the distance traveled by each bird per day
fw_displace_list <- vector("list", length=length(fw_ID))
names(fw_displace_list) <- fw_ID


#distance for UTM = sqrt((E1-E2)^2 +(N1-N2)^2)
#distance loop for FW
for(i in 1:length(fw_ID)){
  tmp_data <- terrfwGPS[terrfwGPS$individual.local.identifier == fw_ID[i],]
  tmp_displace <- data.frame(date = unique(as.Date(tmp_data$study.local.timestamp)),
                             distance = NA, npoints = NA)
  #distance loop for each day
  for(d in 1:nrow(tmp_displace)){
    tmp_dayta <- tmp_data[as.Date(tmp_data$study.local.timestamp) == tmp_displace[d,1],]
    
    first_point <- tmp_dayta[1,]
    
    #calculating distance from each point to first point of the day
    #NA when there is only one point on a day 
    tmp_displace[d,"distance"] <- max(sqrt((first_point$utm.easting-tmp_dayta[2:nrow(tmp_dayta),"utm.easting"])^2 
                                  + (first_point$utm.northing-tmp_dayta[2:nrow(tmp_dayta),"utm.northing"])^2))
    
    
    #adding the number of points for that day
    tmp_displace[d,"npoints"] <- nrow(tmp_dayta)
  }

  #adding individual ID
  tmp_displace$ID <- fw_ID[i]
  
  
  #entering average distance per day for a bird into a list
  fw_displace_list[[i]] <- tmp_displace
  
}


#combining list of max distance into one dataframe
#each row has no individual ID
fw_displace_df <- do.call("rbind", fw_displace_list)
fw_displace_df[fw_displace_df$distancekm > 80 & !is.na(fw_displace_df$distancekm),]


#General summary of maximum daily displacement
#the >100km travels are by Lake Male going to Cody
summary(fw_displace_df)


####distance maximum displacement by all in park territorials####
#it is bimodal around 0 (roost/territory) and 40 (Gardiner)
fw_displace_df$distancekm <- fw_displace_df$distance/1000
hist(fw_displace_df$distancekm,
     xlab="Maximum Displacement (km)",
     main="In Park Territorial Breeders",
     breaks=seq(0,110,1),
     freq=T)


####creating hist for each individual####
#layout(matrix(1:20, nrow=4, ncol=5))
lapply(fw_displace_list, FUN = function(x){
  hist(x$distance/1000,
       xlab="",ylab="",
       main=x[1,"ID"],
       breaks=seq(0,110,1),
       freq=T)
  
})
#tons of variation in how many choose to commute


####choice to commute or not sequentially####
#3= commute >20km
#2= intermediate 5-20km
#1= territory <5km
#this does not take into account the location of territories
#so some individuals may have a commute that is in the intermediate range
#territory could also probably be a lot smaller
#this also assumes the bird returns to the territory every night, but we know that isnt the case

fw_displace_df$commute <- NA
fw_displace_df[!is.na(fw_displace_df$distancekm) & fw_displace_df$distancekm > 20,]$commute <- 3
fw_displace_df[!is.na(fw_displace_df$distancekm) & fw_displace_df$distancekm < 5,]$commute <- 1
fw_displace_df[is.na(fw_displace_df$commute),]$commute <- 2


#layout(matrix(1:20, nrow=4, ncol=5))
for(i in 1:length(unique(fw_displace_df$ID))){
  tmpx <- subset(fw_displace_df, ID == unique(fw_displace_df$ID)[i])
  
  plot(commute~date, tmpx,
       main = tmpx[1,"ID"],
       cex = 0.7)
}


####distance between points by time of day####
#need to create a new data frame that has all of the distances between consecutive 
#points instead of only the max displacement

#creating empty lists to place the distance traveled by each bird per day
fw_distance_list <- vector("list", length=length(fw_ID))
names(fw_displace_list) <- fw_ID


for(i in 1:length(fw_ID)){
  tmp_data <- terrfwGPS[terrfwGPS$individual.local.identifier == fw_ID[i],]
  tmp_days <- unique(as.Date(tmp_data$study.local.timestamp))
  tmp_list_days <- vector("list", length=length(tmp_days))
  names(tmp_list_days) <- tmp_days
  
  #distance loop for each day
  for(d in 1:length(tmp_list_days)){
    tmp_dayta <- tmp_data[as.Date(tmp_data$study.local.timestamp) == tmp_days[d],]
    tmp_dayta$distance <- NA
    
    #skipping if there is only one point that day
    if(nrow(tmp_dayta) > 1){
      
      #calculating the step distance
      for(t in 1:(nrow(tmp_dayta)-1)){
        tmp_step <- tmp_dayta[c(t, t+1),]
        
        #NA value for steps greater than 1.1 hours
        if(difftime(ymd_hms(tmp_step[2,]$timestamp),ymd_hms(tmp_step[1,]$timestamp), units = "mins") < 65){
          tmp_dayta[t,"distance"] <- sqrt((tmp_step[1,"utm.easting"]-tmp_step[2,"utm.easting"])^2 
                                          + (tmp_step[1,"utm.northing"]-tmp_step[2,"utm.northing"])^2)
        } else{
          tmp_dayta[t,]$distance <- NA
          
        }
        
      }
    }
    
    tmp_list_days[[d]] <- tmp_dayta
  }
  
  #combining all days for an individual into one dataframe
  tmp_distance <- do.call("rbind", tmp_list_days)
  
  
  #entering average distance per day for a bird into a list
  fw_distance_list[[i]] <- tmp_distance
  
}

fw_distance_df <- do.call("rbind", fw_distance_list)


#average step distance per hour
#but probably should match up with the minimum time per step described in the 
#for loop (line 194) 1.1 hours 65 minutes
#to make the most sense
fw_distance_df$hour <- hour(ymd_hms(fw_distance_df$study.local.timestamp))
avrg_dist_hr <- tapply(fw_distance_df$distance, INDEX = fw_distance_df$hour, FUN = mean, na.rm=T)
plot(avrg_dist_hr ~ names(avrg_dist_hr),
     main = "Average distance per step",
     xlab = "Hour", ylab = "Distance (m)",
     xlim = c(5,20), xaxp = c(5,20,15))
lines(avrg_dist_hr ~ names(avrg_dist_hr))



#average step distance per hour for individuals
lapply(fw_distance_list, FUN = function(x){
  
            x$hour <- hour(ymd_hms(x$study.local.timestamp))
            avrg_dist_hr_ID <- tapply(x$distance, INDEX = x$hour, FUN = mean, na.rm=T)
            plot(avrg_dist_hr_ID ~ names(avrg_dist_hr_ID), 
                 main = paste0("Average distance per step (", x[1,]$individual.local.identifier, ")"),
                 xlab = "Hour", ylab = "Distance (m)",
                 xlim = c(5,20), xaxp = c(5,20,15))
            lines(avrg_dist_hr_ID ~ names(avrg_dist_hr_ID))
  
})
