Explanation of what each file is used for and each column (column type, units)

gardiner_hunt_poly_roads (shp): a polygon for the Gardiner hunting region based on a buffer outside Yellowstone boundary and roads as potential hunting access points
	gardiner_mtfwp_region: 5 km away from all roads 10 km from park boundary. Used for MTFWP rifle huntig season
	gardiner_bison_region: 1 km away from the two parrallel roads (Hwy 89 & Olf Yellowstone Trail) that lead from Gardiner to Yankee Jim, stopping at the cattle grate. Used for the period after the MTFWP rifle season ends

mcp90_shapefile (shp): exported shapefiles for the raven territories calculated using a 90% minimum convex polygon

all_raven_gps_clean##: the raw raven GPS movement data taken from movebank and cleaned so that the minimum amount of time between consecutive points for an individual is ## (number at the end of file name). These files are created and written form clean_time_between_points.R
	Only explaining relevant columns since there is a lot of junk from Movebank that isn't used
	timestamp (datetime): raven GPS point date and time at UTC-0
	location.long (num): raven GPS point longitude
	location.lat (num): raven GPS point latitude
	tag.local.identifier (num): the unique ID for the GPS tag
	individual.local.identifier (chr): the individual identity of the raven, this also seperates ravens that had the same tag (e.g. 7490, 7490_2)
	utm.easting (num): raven GPS point easting
	utm.northing (num): raven GPs point northing
	utm.zone: UTM zone that the raven GPS point is in
	study.local.timestamp (datetime): raven GPS point date and time at UTC-7 (MST) and UTC-6 (MDT)

bison_daily_take (csv): has daily bison take values from the northern range from surveys done by the Bison Project
	date (date): survey date
	take (num): number of bison found to be taken that day

commute_data: the movement decisions about if a raven left its territory and visited the Gardiner hunting region and the associated covariates for modeling. File is created and written from covariates.R as a master script of many smaller scripts for specific things.
	raven_id (chr): the individual identity of the raven, this also seperates ravens that had the same tag (e.g. 7490, 7490_2)
	date (date): no explanation
	hunting_period (chr): 3 categories that say if that day is during a hunting period, winter study period, or both
	dump (bool): if the raven visited the Gardiner dump and sewage pond that day (T) or not (F)
	n_point (num): the number of GPS points from that raven on that day
	terr_bin (bool): if the raven left its territory (T) or not (F). Created by using the 'commute' column
	hunt_bin (bool): if the raven had GPS points in the Gardiner hunting district (T) or not (F). Created using the 'commute' column
	dist2nentrance (num, meters): how far the center of each ravens territory (90% minimum convex polygon crated from breeding season movement) is from the Yellowstone north entrance station
	avg_terr_kill_density (num): the average number of wolf acquired carcasses in a ravens territory over 30 days taken from all winter study periods where that raven was GPs tagged and a territory holder
	active_kill (bool): if there is an active wolf acquired carcass within 1 km of a ravens territory (T) or not (F). A carcass is active from the time of the kill until 1 day after wolves have abandoned the carcass
	start_hunt (date): the start date for the FWP rifle hunting season
	bison_hunt_start (date): the start date for when bison hunting actually started in late winter. This is variable because bison hunting is dependent on bison migration
	end_hunt (date): the end date for the FWP rifle hunting season
	hunt_season (bool): if that day is within the FWP rifle hunting season (T) or not (F)
	final_take_bms (num): the estimated daily take of elk/deer/bison taking into account the estimated season take from FWP phone surveys, tribal bison take, ungulate movement based on elk GPS collars for the FWP season, and relative weights of ungulates (2.15 elk = bison, 0.3 elk = deer)
	bms_window_1 (num): the hunting biomass available the previous day taken from 'final_take_bms'
	bms_window_3 (num): the average hunting biomass available the previous 3 days calculated from 'final_take_bms'
	bms_window_5 (num): the average hunting biomass available the previous 5 days calculated from 'final_take_bms'
	take_high_low (chr): 3 categories that say if the biomass available that day was high (>1), low (<1), or 0. taken from 'final_take_bms'
	weekend (bool): if that day was a weekend (T) or not (F)
	study_period (chr): 3 categories that say if the day is in November/December (early), January/February (mid), or March (late)
	yearly_terr_kill_density (num): the number of wolf acquired carcasses in a ravens territory over 30 days taken from the winter study periods
	prop_group_left_terr (num): the proportion of other ravens (excluding the raven whos row it is) that left their territories that day
	prop_group_visit_hunt (num): the proportion of other ravens (excluding the raven whos row it is) that visited the Gardiner hunting district that day
	previous_decision_terr (bool): if the raven left its territory the previous day (T) or not (F)
	previous_decision_hunt (bool): if the raven viisted the Gardiner huntin district the previous day (T) or not (F)

mar/nov_gps_outside_ynp: territorial raven GPS data outside the park for the months of November and march. These are used to create heatmaps of the locations used to find food in the Gardiner hunting district.
	location_lat (num): raven GPS point latitude
	location_long (num): raven GPS point longitude