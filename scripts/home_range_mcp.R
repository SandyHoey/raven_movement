#MCP 95 for in park breeders during the summer

library(adehabitatHR)
library(data.table) #use of %like%
library(tidyverse)
library(sf)
library(sp)
library(mapview)



#reading in all raven points
#removing columns with NA coords
allGPS <- read_csv("data/clean/all_raven_gps_clean29.csv")
allGPS <- subset(allGPS, !is.na(utm.easting))


#importing demographic information
#terr: territorial birds inside the park (inclduing interrior birds)
#trans: birds that transitioned between breeder and nonbreeder
#7485 (Old Faithful) transitioned more than once
  # currently using only the first breeding period (old faithful sinclair)
  # she also went back to a different territory in 2024 (old faithful snotel)
#have to consider interior birds commuting to West instead of Gardiner 
ravenID <- readxl::read_excel("data/raw/ravens_banding_tagging.xlsx", sheet=1)
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



#subsetting the GPS points to only breeding season (subject to change the month)
terrGPS <- terrGPS[month(terrGPS$study.local.timestamp) %in% c(5,6,7),]


#plotting gps points per bird to make sure they are on territory 
mysf <- st_as_sf(terrGPS, coords = c("utm.easting", "utm.northing"), crs = 32612)
#tapply(mysf, INDEX = mysf$individual.local.identifier, FUN = mapview)


#creating mcp for each individual
# mcp95 <- mysf %>% 
#   dplyr::select(c("individual.local.identifier", "geometry")) %>% 
#   as_Spatial() %>%
#   mcp(percent = 95, unout = "km2")
# 
# 
# mapview(mcp95[,1])


#95% mcp had the mammoth birds with a massive territory including rainbow lakes and gardiner
#can either cut that down by lowering mcp % or restricting points to a smaller window
#most of the points far form the territory were in may and july
#june is pretty centered
mcp90 <- mysf %>% 
  dplyr::select(c("individual.local.identifier", "geometry")) %>% 
  as_Spatial() %>%
  mcp(percent = 90, unout = "km2")


mapview(mcp90[,1])
