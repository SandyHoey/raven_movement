#percentage of raven GPS points that occur in the Gardiner/Jardine
#area during the fall/winter season (Nov-Mar)

library(sf)
library(tidyverse)
library(data.table)


#reading in all raven points
#removing columns with NA coords
allGPS <- readr::read_csv("data/clean/all_raven_gps_clean29.csv")
allGPS <- subset(allGPS, !is.na(utm.easting))



#transforming raven points to sf dataframe
#allows a distance metric to the polygon to be calculated
sf_ravens_all <- st_as_sf(allGPS, coords=c("utm.easting", "utm.northing"), 
                          crs="+proj=utm +zone=12")


#reading in Gardiner/Jardine kml
#transforming latlong to UTM to match the GPS points
Gardinerkml <- st_read("data/raw/gardiner_polygon.kml")
Gardinerkml <- st_transform(Gardinerkml, crs = st_crs(sf_ravens_all))


#calculating distance between GPS points and Gardine/Jardine kml
#0 == inside the polygon
allGPS$dist2Gardiner <- as.numeric(st_distance(sf_ravens_all, Gardinerkml))


#importing demographic information
#terr: territorial birds inside the park
#trans: birds that transitioned between breeder and nonbreeder
#7485 (Old Faithful) currently not included because she transitioned more than once
ravenID <- readxl::read_excel("data/raw/ravens_banding_tagging.xlsx",sheet=1)
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
terrfwGPS <- terrGPS[month(terrGPS$study.local.timestamp) %in% c(10, 11, 12, 1, 2, 3),]


        
        
#calculating the percent of GPS points that are inside the Gardiner/Jardine polygon
#for territorial birds

terrfwGardiner <- tapply(terrfwGPS, 
                         terrfwGPS$individual.local.identifier, 
                         function(x){
                           nrow(x[x$dist2Gardiner == 0,])/nrow(x)
})
terrfwGardiner
range(terrfwGardiner)
hist(terrfwGardiner)


