#getting HMM model and mcp90 from other scripts
source("scripts/exploratory/HMM.R")
source("scripts/exporatory/Home Range (MCP).R")


# preparing HMM data
# only entrenched
# removing entrenched in other places
input_data %>% 
  filter(vit == 1) %>% 
  mutate(easting = x*1000, northing = y*1000) %>% 
  st_as_sf(coords = c("easting", "northing"), crs = 32612) 



%>% 
  dplyr::select(c("ID", "geometry")) %>% 
  as_Spatial() %>%
  mcp(percent = 100, unout = "km2") -> mcp_entrenched

mcp90_df <- as.data.frame(mcp90)
mcp_entrenched_df <- as.data.frame(mcp_entrenched[,2])
cbind(mcp90_df, mcp_entrenched_df)
