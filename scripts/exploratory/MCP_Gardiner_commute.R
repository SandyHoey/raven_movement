source("scripts/exploratory/Home Range (MCP).R")
source("scripts/exploratory/%inGardiner.R")


#calculating distance to territory
#using 90% mcp
#distance calculated in meters
mcp_in <- function(){
  ID <- mcp90$id
  
  for(i in 1:length(ID)){
    tmp_data <- subset(terrfwGPS, individual.local.identifier == ID[i])
    tmp_sf <- st_as_sf(tmp_data, coords=c("utm.easting", "utm.northing"), 
                       crs="+proj=utm +zone=12")
    
    tmp_data$dist2Terr <- as.numeric(st_distance(tmp_sf, st_as_sf(mcp90[i,]), unit = ))
    
    if(i != 1){
      output_df <- rbind(output_df, tmp_data)
    } else{
      output_df <- tmp_data
    }
  }
  
  return(output_df)
}

dist2poly <- mcp_in()

#adding the wolf/hunting periods
  # Oct 25-Nov 14 rifle hunting only
  # Nov 15-Nov 30 wolf + hunting
  # Dec 1-Dec 15 wolf only
dist2poly <- dist2poly %>% 
  mutate(day = day(study.local.timestamp),
         month = month(study.local.timestamp),
         period = "neither")
dist2poly[dist2poly$month == 10 | (dist2poly$month == 11 & dist2poly$day < 15),]$period <- "hunt"
dist2poly[dist2poly$month == 11 & dist2poly$day >= 15,]$period <- "both"
dist2poly[dist2poly$month == 12 & dist2poly$day <= 15,]$period <- "wolf"



#getting the number of points in territory and Gardiner 
info_table <- tapply(dist2poly, INDEX = dist2poly$individual.local.identifier,
       FUN = function(x){
         info_table <- rep(NA, 4)
         names(info_table) <- c("Gardiner", "Terr", "Other", "Total")
         
         info_table[1] <- nrow(subset(x, dist2Gardiner == 0))
         info_table[2] <- nrow(subset(x, dist2Terr == 0))
         info_table[3] <-  nrow(subset(x, dist2Gardiner != 0 & dist2Terr != 0))
         info_table[4] <- nrow(x)
           
         return(info_table)
       })


#number of points adds up properly
# lapply(info_table, function(x){
# 
#   return(c(sum(x[1:3]), x[4]))
# })


#creating a column that shows if the raven decided to commute or leave terr that day
#3 = has a least 1 point that day in Gardiner poly
#2 = has at least 1 point farther than 1 km from terr poly , but none in Gardiner poly
#1 = only has points in terr poly
ID <- unique(dist2poly$individual.local.identifier)

commute_list <- tapply(dist2poly, INDEX = dist2poly$individual.local.identifier,
       FUN = function(x){
         
         dates <- unique(as.Date(x$study.local.timestamp))
         period <- x %>% 
           group_by(as.Date(study.local.timestamp)) %>% 
           slice(1) %>% 
           ungroup %>% 
           pull(period)
         tmp_date_df <- data.frame(ID = x[1, "individual.local.identifier"],
                                          date = dates, period, commute = NA) 
         
         #cycling through all the dates for each individual to tell where the
         #individual ended up that day (terr, other, Gardiner)
         for(d in 1:length(dates)){
           tmp_dayta <- subset(x, as.Date(study.local.timestamp) == dates[d])
           
           if(any(tmp_dayta[,"dist2Gardiner"] == 0)){
             tmp_date_df[d,"commute"] <- 3
           } else if(any(tmp_dayta[,"dist2Terr"] > 1000)){
             tmp_date_df[d,"commute"] <- 2
           } else{
             tmp_date_df[d,"commute"] <- 1
           }
         }
         return(tmp_date_df)
       })



#plotting raven commute decision per day
#layout(matrix(1:20, nrow=4, ncol=5))
sapply(commute_list, 
       FUN = function(x){
         plot(commute~date, x,
              main = x[1, "individual.local.identifier"],
              cex = 0.7, yaxp = c(0,3,3))
})


#calculating the percentage of points in each commute category
#did remove the old faithful birds
commute_percent <- commute_list %>% 
  do.call("rbind",.) %>% 
  mutate(month = month(.$date)) %>% 
  filter(!(individual.local.identifier %in% c("7494", "7485"))) %>% 
  group_by(month) %>% 
  summarise(terr = sum(commute == 1)/n()*100,
            mid = sum(commute == 2)/n()*100,
            Gardiner = sum(commute == 3)/n()*100)
  

#plotting the percentage of each commute category by month
commute_percent %>% 
pivot_longer(cols = c(terr, mid, Gardiner), 
               names_to = "commute", values_to = "percent") %>% 
  mutate(month = fct_relevel(as.character(month), c("11", "12", "1", "2", "3"))) %>% 
  ggplot(aes(x = month, y = percent, 
             group = commute, col = commute)) +
  geom_line(lwd = 1) +
  labs(y = "Percent of days") +
  theme_classic() +
  scale_y_continuous(expand=c(0,0), limits = c(0, 65))


#plotting the different combinations of wolf/hunter periods
period_percent <- commute_list %>% 
  do.call("rbind",.) %>% 
  filter(!(individual.local.identifier %in% c("7494", "7485"))) %>% 
  group_by(period) %>% 
  summarise(terr = sum(commute == 1)/n()*100,
            mid = sum(commute == 2)/n()*100,
            Gardiner = sum(commute == 3)/n()*100)

