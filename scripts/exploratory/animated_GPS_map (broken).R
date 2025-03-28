#this script creates an animation of raven movement during the winter
#to show commuting behavior of territorial ravens

library(tidyverse)
library(gganimate)
library(rnaturalearth) #to get basemap 
library(gifski)
library(janitor)
library(readxl)
library(data.table)


#reading in movement data
gps_data <- read_csv("data/clean/all_raven_gps_clean28.csv")
gps_data <- clean_names(gps_data)



# subsetting territorial birds --------------------------------------------
#importing demographic information
#terr: territorial birds inside the park (inclduing interrior birds)
#trans: birds that transitioned between breeder and nonbreeder
#7485 (Old Faithful) currently not included because she transitioned more than once
#have to consider interior birds commuting to West instead of Gardiner 
ravenID <- read_excel("data/raw/ravens_banding_tagging.xlsx", sheet=1)
ravenID <- clean_names(ravenID)
terr <- subset(ravenID, status_reviewed_8_1_24 == "territorial" & 
                 ravenID$inside_national_park == "yes")$tag_id
trans <- subset(ravenID, status_reviewed_8_1_24 %like% "Trans")


#subsetting territorials from entire GPS df
terrGPS <- subset(gps_data, individual_local_identifier %in% terr)


#subsetting trans territorials into their active territorial periods from entire GPS df
transGPS <- gps_data[gps_data$individual_local_identifier %in% trans$tag_id,]

transGPS <- do.call("rbind", tapply(transGPS, INDEX=transGPS$individual_local_identifier, 
                                    FUN=function(x){
                                      ind <- trans[trans$tag_id == x[1,]$individual_local_identifier,]
                                      
                                      #has only an end date
                                      if(is.na(ind$start_date) & !is.na(ind$leave_date)){
                                        tmp <- x[as.Date(x$study_local_timestamp) < ym(ind$leave_date),]
                                        return(tmp)
                                      }
                                      
                                      #has only a start date
                                      if(!is.na(ind$start_date) & is.na(ind$leave_date)){
                                        tmp <- x[as.Date(x$study_local_timestamp) > ym(ind$start_date),]
                                        return(tmp)
                                      }
                                      
                                      
                                      #should be excluded
                                      if(is.na(ind$start_date) & is.na(ind$leave_date)){
                                        return()
                                      }
                                    }))


#combining trans and territorial datasets
#!!!SKIP this step if you want to exclude trans 
terrGPS <- rbind(terrGPS, transGPS)


# using move package ------------------------------------------------------

#grabbing basemap
bg = ne_countries(scale = "large", continent = 'north america', returnclass = "sf")


#creating static plot
p <- ggplot() +
  
  # basemap
  geom_sf(data = bg) +
  coord_sf(xlim = range(terrGPS$location_long, na.rm = TRUE),
           ylim = range(terrGPS$location_lat, na.rm = TRUE),
           expand = FALSE) +
  
  # lines and points
  geom_path(data = terrGPS, aes(x = location_long, y = location_lat,
                                group = individual_local_identifier), 
            alpha = 0.3) +
  geom_point(data = terrGPS, aes(x = location_long, y = location_lat,
                                 group = individual_local_identifier),
             alpha = 0.7, shape = 21, size = 2) +
  
  
  # formatting
  scale_fill_viridis_c(option = "inferno") +
  scale_color_viridis_c(option = "inferno") +
  scale_size_continuous(range = c(0.1,10)) +
  labs(x = NULL, y = NULL) +
  theme_dark()+
  theme(panel.grid = element_blank())
p


anim = p + 
  transition_reveal(along = study_local_timestamp)+
  ease_aes('linear')

animate(anim, fps = 10)
