# calculating distance to territory
# using 90% mcp
# distance calculated in meters
gps_in_mcp <- function(data){
  ID <- mcp90$id
  
  # splitting by individual
  for(i in 1:length(ID)){
    tmp_data <- subset(data, individual_local_identifier == ID[i])
    tmp_sf <- st_as_sf(tmp_data, coords=c("utm_easting", "utm_northing"), 
                       crs="+proj=utm +zone=12")
    
    tmp_data$dist2terr <- as.numeric(st_distance(tmp_sf, st_as_sf(mcp90[i,]), unit = ))
    
    if(i != 1){
      output_df <- rbind(output_df, tmp_data)
    } else{
      output_df <- tmp_data
    }
  }
  
  return(output_df)
}