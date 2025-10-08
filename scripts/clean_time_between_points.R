#--Cameron Ho
#--ESRM Senior capstone
#--Under John Marzluff and Matthias Loretto
#--Raven foraging in the Greater Yellowstone Ecosystem

##--Code used to clean all GPS points for MSc work
##--turned into a function with arguments
  #data: dataframe
  #interval: how much time is allowed between points (default 28min)



library(tidyverse) #read in xlsx file
#library(data.table) #use of %like% and transformation to long format dataframes
#library(dplyr) #for fixing data (removing duplicates and combining data.frames)
library(readr) #extract number from string
#library(tidyr)
#library(rgdal)


# All Points --------------------------------------------------------------
data.all <- janitor::clean_names(read_csv("data/raw/ravenGPS_movebank.csv"))

##---------------------------- Fixing data ---------------------------------------

#interval = the minimum amount of time allowed between points
cleanGPS <- function(data, interval=28){
  
  #pulling out all raven ID
  ID <- unique(data$individual_local_identifier)
  
  #making datetime a readable format
  data$study_local_timestamp <- as.POSIXct(data$study_local_timestamp)
  data <- data[order(data$study_local_timestamp),]
  
  
  #removing points too close in time (foraging points)
  print("Starting cleaning loop")
  for(i in 1:length(ID)){
    ID.tmp <- subset(data, individual_local_identifier == ID[i])
    
    for(j in 1:(nrow(ID.tmp)-1)){
      if(j == 0){
        j <- 1
      }
      repeat{ 
        time.diff.tmp <- as.numeric(difftime(ID.tmp$study_local_timestamp[j+1],ID.tmp$study_local_timestamp[j], units = "mins"))
        if(time.diff.tmp < interval & j < nrow(ID.tmp)){
          ID.tmp <- ID.tmp[-(j+1),]
        }
        if(time.diff.tmp >= interval | is.na(time.diff.tmp) == TRUE) break}
      
    }
    if(i == 1){
      tmp.data <- ID.tmp
    }
    if(i >= 2){
      tmp.data <- rbind(ID.tmp, tmp.data) 
    }
    
    print(paste0(i, "/", length(ID), " at ", Sys.time()))
  }

  
  return(tmp.data)
}


data.clean <- cleanGPS(data.all, interval=58)

write.csv(data.clean, "data/clean/all_raven_gps_clean58.csv")
 