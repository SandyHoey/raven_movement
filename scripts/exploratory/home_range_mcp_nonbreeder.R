#MCP 95 for nonbreeders looking at shifts in home range between seasons

library(adehabitatHR)
library(data.table) #use of %like%
library(tidyverse)
library(readxl)
library(sf)
library(sp)
library(mapview)



#reading in all raven points
#removing columns with NA coords
allGPS <- read_csv("data/clean/all_raven_gps_clean29.csv")
allGPS <- subset(allGPS, !is.na(utm.easting))


#removing 7646 because there arent enough winter points
#removing 7653 and 7596 because Canada
allGPS <- subset(allGPS, individual.local.identifier != "7646")
allGPS <- subset(allGPS, individual.local.identifier != "7653")
allGPS <- subset(allGPS, individual.local.identifier != "7596")


#importing demographic information
#nb: vagrant birds
#trans: birds that transitioned between breeder and nonbreeder
ravenID <- read_excel("data/raw/ravens_banding_tagging.xlsx",sheet=1)
nb <- subset(ravenID, `status (reviewed 8/1/24)` == "vagrant")$`tag-id`
trans <- subset(ravenID, `status (reviewed 8/1/24)` %like% "Trans")


#subsetting territorials from entire GPS df
nbGPS <- subset(allGPS, individual.local.identifier %in% nb)


#subsetting trans territorials into their active territorial periods from entire GPS df
transGPS <- allGPS[allGPS$individual.local.identifier %in% trans$`tag-id`,]

transGPS <- do.call("rbind", tapply(transGPS, INDEX=transGPS$individual.local.identifier, 
                                    FUN=function(x){
                                      ind <- trans[trans$`tag-id` == x[1,]$individual.local.identifier,]
                                      
                                      #has only an end date
                                      if(is.na(ind$`start date`) & !is.na(ind$`leave date`)){
                                        tmp <- x[as.Date(x$study.local.timestamp) > ym(ind$`leave date`),]
                                        return(tmp)
                                      }
                                      
                                      #has only a start date
                                      if(!is.na(ind$`start date`) & is.na(ind$`leave date`)){
                                        tmp <- x[as.Date(x$study.local.timestamp) < ym(ind$`start date`),]
                                        return(tmp)
                                      }
                                      
                                      
                                      #should be excluded
                                      if(is.na(ind$`start date`) & is.na(ind$`leave date`)){
                                        return()
                                      }
                                    }))


#combining trans and territorial datasets
#!!!SKIP this step if you want to exclude trans 
nbGPS <- rbind(nbGPS, transGPS)



#subsetting the GPS points into summer and winter (subject to change the month)
nbGPS_S <- nbGPS[month(nbGPS$study.local.timestamp) %in% c(5,6,7,8),]
nbGPS_W <- nbGPS[month(nbGPS$study.local.timestamp) %in% c(10,11,12,1,2,3),]


#transforming into a sf format so it can be used to create an mcp
sfS <- st_as_sf(nbGPS_W, coords = c("utm.easting", "utm.northing"), crs = 32612)
sfW <- st_as_sf(nbGPS_S, coords = c("utm.easting", "utm.northing"), crs = 32612)


#plotting gps points per bird to make sure they are on territory 
#tapply(mysf, INDEX = mysf$individual.local.identifier, FUN = mapview)


#creating mcp for each individual
sfS %>% 
  dplyr::select(c("individual.local.identifier", "geometry")) %>% 
  as_Spatial() %>%
  mcp(percent = 50, unout = "km2") -> mcp95_S

sfW %>% 
  dplyr::select(c("individual.local.identifier", "geometry")) %>% 
  as_Spatial() %>%
  mcp(percent = 50, unout = "km2") -> mcp95_W


#visualizing the two mcp for a single bird
#change th row to change the bird
mapview(mcp95_W[2,]) +
  mcp95_S[2,]



#the shift between seasons still has a ton of overlap in most cases
#the winter is more concentrated around Gardiner
#but the Summer still often includes Gardiner, just also has a lot more

#Not sure what to think about this