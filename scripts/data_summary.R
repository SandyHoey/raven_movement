#gets basic statistics about the data for reporting in the beginning of the results

library(dplyr)

#reading in model data without any edits
all <- readr::read_csv("data/clean/commute_data.csv")


#decision days
nrow(all) #total days
mean(table(all$raven_id))
sd(table(all$raven_id))
max(table(all$raven_id))


#total number of ravens included in study
length(unique(all$raven_id))


#number of GPS points per day
hist(all$n_point, 
     breaks = 1:max(all$n_point))
mean(all$n_point)
sd(all$n_point)


