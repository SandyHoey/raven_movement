#Using Hidden Markov Movement model to categorized the movements of ravens 

library(moveHMM)


#creating covariates
source("scripts/covariates.R")


# Data check --------------------------------------------------------------
# #checking to see how regular the time intervals are between points
# #only for the breeders 
# #difference in minutes (it says seconds, but its not)
# ID <- unique(GPS_W$tag_local_identifier)
# for(i in 1:length(ID)){
#   
#   #subsetting an individual
#   tmp_GPS_ID <- subset(GPS_W, tag_local_identifier == ID[i])
#   
#   days <- unique(date(tmp_GPS_ID$study_local_timestamp))
#   
#   
#   for(d in 1:length(days)){
#     
#     #subsetting a single day
#     tmp_GPS_day <- subset(tmp_GPS_ID, date(study_local_timestamp) == days[d])
#     
#     if(d == 1 & i == 1){
#       #finding time difference between consecutive points
#       time_diff <- diff(tmp_GPS_day$study_local_timestamp, )/60
#     } else{
#       time_diff <- c(time_diff, diff(tmp_GPS_day$study_local_timestamp)/60)
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
input_data <- dist2poly[,c("utm_easting", "utm_northing", "individual_local_identifier", 
                           "dist_terr", "within_terr", "active_kill", "hunt")]

#merging the other covariates
##' covariate 2: average day between kills within territory (not currently included because lots of NaN values)
##' covariate 3: average kill density within territory
#input_data <- left_join(input_data, avg_day_betwn_kill, by = "individual_local_identifier")
input_data <- left_join(input_data, avg_kill_density, by = "individual_local_identifier")

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

