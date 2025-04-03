#getting HMM model and mcp90 from other scripts
source("scripts/exploratory/HMM.R")
source("scripts/exporatory/Home Range (MCP).R")


#calculating center point of summer mcp
mcp_center <- data.frame(ID = as.data.frame(mcp90)$id, northing=NA, easting=NA)
for(i in 1:21){
  tmp_bounds <- mcp90@polygons[[i]]@Polygons[[1]]@coords
  mcp_center[i, "northing"] <- mean(tmp_bounds[,1])
  mcp_center[i, "easting"] <- mean(tmp_bounds[,2])
}


# preparing HMM data
# only entrenched
# removing points outside of Yellowstone
ynp_poly <- st_read("data/raw/parkpoly.kml") %>% 
  st_transform(crs="+proj=utm +zone=12")

input_data <- input_data %>% 
  filter(vit == 1) %>% 
  mutate(easting = x*1000, northing = y*1000) %>% 
  st_as_sf(coords = c("easting", "northing"), crs="+proj=utm +zone=12")

park_dist <- st_distance(input_data, ynp_poly) %>% 
  as.numeric()

within_park <- input_data[park_dist == 0,]


# generating mcp 
within_park %>% 
  dplyr::select(c("ID", "geometry")) %>% 
  as_Spatial() %>%
  mcp(percent = 90, unout = "km2") -> mcp_entrenched

mcp90_df <- as.data.frame(mcp90)
mcp_entrenched_df <- as.data.frame(mcp_entrenched[,2])
cbind(mcp90_df, mcp_entrenched_df)
mapview(mcp90)
mapview(mcp_entrenched)

