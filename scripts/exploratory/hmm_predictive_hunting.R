#Using Hidden Markov Movement model to categorized the movements of ravens 

library(dplyr)
library(momentuHMM)
library(data.table)
library(sf)


# reading in and subsetting data ------------------------------------------

#reading in all raven points
#removing columns with NA coords
allGPS <- readr::read_csv("data/clean/all_raven_gps_clean58.csv")
allGPS <- subset(allGPS, !is.na(utm.easting))


#removing 7646 because there arent enough winter points
#removing 7653 and 7596 because Canada
allGPS <- subset(allGPS, individual.local.identifier != "7646")
allGPS <- subset(allGPS, individual.local.identifier != "7653")
allGPS <- subset(allGPS, individual.local.identifier != "7596")


#importing demographic information
#nb: nonbreeding birds
#trans: birds that transitioned between breeder and nonbreeder
ravenID <- readxl::read_excel("data/raw/ravens_banding_tagging.xlsx",sheet=1)
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
terrGPS <- rbind(nbGPS, transGPS)


#pulling out only winter points (subject to change the month)
gps_w <- terrGPS[month(terrGPS$study.local.timestamp) %in% c(10,11,12,1,2,3),]


#transforming raven points to sf dataframe
#allows a distance metric to the polygon to be calculated
sf_raven <- st_as_sf(gps_w, coords=c("utm.easting", "utm.northing"), 
                      crs="+proj=utm +zone=12")


##restricting area to only GPS points in Gardiner
#reading in gardiner polygon
gardiner_poly <- st_read("data/raw/gardiner_hunt.kml") %>% 
  st_transform(crs = st_crs(sf_raven))
#calculating distance from points to polygon
gps_w$dist2Gardiner <- as.numeric(st_distance(sf_raven, gardiner_poly))
#subsetting to only 0 distance (in the polygon)
gps_final <- subset(gps_w, dist2Gardiner == 0)



# HMM ---------------------------------------------------------------------

#prepping data for HMM package
input_data <- gps_final %>% 
  
  #creating a ID column moveHMM can recognize
  mutate(ID = paste0(individual.local.identifier, "_", as.Date(study.local.timestamp))) %>%
  
  #selecting only relevant columns
  dplyr::select(ID, utm.northing, utm.easting) %>% 
  group_by(ID) %>% 
  summarise(mean_n = n())
  filter(n() <= 5)
  
  #prepping data with moveHMM function
  prepData(type = "UTM", coordNames = c("utm.easting", "utm.northing"))


#looking at individuals to see point distribution
ID_split <- input_data %>% 


# label states
stateNames <- c("encamped","exploratory", "travel")


## initial model parameters
  #step
  hist(input_data$step)
  stepMean0 <- c(1, 1000, 5000)
  stepSD0 <- c(1, 10, 20)
  stepPar0 <- c(stepMean0, stepSD0)

  #turn angle
  hist(input_data$angle)
  turnMean0 <- c(pi, 1, 0)
  #concentration: how clustered values are around the mean
  #higher number = more concentrated around mean
  turnCon0 <- c(1, 10, 20)
  turnPar0 <- c(turnMean0, turnCon0)

  
## running the model 
(model_3s <- fitHMM(input_data, nbStates = 3, 
                    dist = list(step = "gamma", angle = "wrpcauchy"),
                    Par0 = list(step = stepPar0, angle = anglePar0),
                    stateNames = stateNames))

#determining state of each point
input_data$vit3 <- viterbi(model_3s)
plot(y ~ x, data = input_data, col = vit)
