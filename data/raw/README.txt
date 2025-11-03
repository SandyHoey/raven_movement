Explanation of what each file is used for and each column (column type, units)

bison_hunt (csv): take values and hunitng start dates from the tribal take of bison from Bison FEIS and IBMP
	year (fac): year the bison take value is for
	take (num): bison take value from NPS Final Environmental Impact Statement
	start_date (date): date that tribal bison hunting started dependant on bison movement
	ibmp (num): bison take value from Interagency Bison Management Program

breeding_raven_gps (points): Google Earth file that has all of the territorial raven GPS points

cooke_city_weather (csv): history of temperature from Red Lodge (https://www.wunderground.com/history/monthly/us/mt/red-lodge/KRED/date/2025-11)
	date (date): date of weather
	temp_max (num): maximum daily temperature
	temp_mean (num): average daily temperature (not sure how this is calculated)
	temp_min (num): minimum daily temperature

CUT_hunt (polygon): Google Earth polygon that represents that tribal bison huting area in the Gardiner hunting district

elk_GPS_2025-09-03 (csv): GPS data from the collared elk in the Northern Herd of Yellowstone
	source (chr): study that the elk was collared for
	ID (fac): unique identifier for each elk
	dt (datetime): date and time for the GPS point
	utm_e (num): easting for the collared elk
	utm_n (num): northing for the collared elk
	precision (num): measure of estimated precision of the GPS point
	precision_type (chr): method of estimating GPS point precision
	serial (fac): UNKNOWN

fwp_deer_harvest_estimate (csv): estimated mule/white-tail deer take in HD313 from MTFWP phone surveys of hunters (https://myfwp.mt.gov/fwpPub/harvestReports)
	License Year (fac): year for the estimated take
	Hunting District (fac): hunting district for the estimated take
	Deer Species (chr): deer species for the estimated take
	Residency (chr): wether the estimated take is for in state or out of state hunters
	Days (num): estimated hunting days spent by all hunters
	Days per Hunter (num): estimated hunting days spent by each hunter
	Total Harvest (num): estimated hunter take across all hunting methods
	Bucks (num): estimated take of bucks
	Does (num): estimated take of does
	Bow (num): estimated take of a deer species by bow hunting
	Rifle (num): estimated take of a deer species by rifle hunting
	Less than 4 Points (num): estimated take of bucks with less than 4 points on one antler
	4 or More Points (num): estimated take of bucks with 4 or more points on one antler

fwp_elk_harvest_estimate (csv): estimated elk take in HD313 from MTFWP phone surveys of hunters (https://myfwp.mt.gov/fwpPub/harvestReports)
	License Year (fac): year for the estimated take
	Hunting District (fac): hunting district for the estimated take
	Residency (chr): wether the estimated take is for in state or out of state hunters
	Days (num): estimated hunting days spent by all hunters
	Days per Hunter (num): estimated hunting days spent by each hunter
	Total Harvest (num): estimated hunter take across all hunting methods
	Bulls (num): estimated take of bucks
	Cows (num): estimated take of does
	Calves (num): estimated take of calves
	Bow (num): estimated take by bow hunting
	Rifle (num): estimated take by rifle hunting
	Spike Bull Elk (num): estimated take of bucks with spikes
	Less than 6 Points (num): estimated take of bucks between 2 and 6 points on one antler
	6 or More Points (num): estimated take of bucks with 6 or more points on one antler

gardiner_dump (polygon): Google Earth polygon that represents that landfill and waste water treatment ponds outside of Gardiner, MT

gardiner_polygon (polygon): Google Earth polygon that represents that represents the Gardiner hunting district + the town of Gardiner, MT

hunting_season (csv): start and end dates from the MTFWP rifle hunting season
	year (fac): year 
	start (date): start date of the MTFWP rifle hunting season
	end (date): end date of the MTFWP rifle hunting season
	md_end (date): start date of protections for mule deer bucks prohibiting take

jardine_hunt (polygon): Google Earth polygon that represents the Jardine area of the Gardiner hunting district used for ungulate hunting during the MTFWP season

northern_range_poly (polygon): Google Earth polygon that represents the northern range of Yellowstone

parkpoly (polygon): Google Earth polygon that represents the boundary of Yellowstone National Park

ravenGPS_movebank (csv): GPS data from all common ravens in the Greater Yellowstone Ecosystem tagged as a part of this study
	timestamp (datetime): raven GPS point date and time at UTC-0
	location.long (num): raven GPS point longitude
	location.lat (num): raven GPS point latitude
	tag.local.identifier (num): the unique ID for the GPS tag
	individual.local.identifier (chr): the individual identity of the raven, this also seperates ravens that had the same tag (e.g. 7490, 7490_2)
	utm.easting (num): raven GPS point easting
	utm.northing (num): raven GPs point northing
	utm.zone: UTM zone that the raven GPS point is in
	study.local.timestamp (datetime): raven GPS point date and time at UTC-7 (MST) and UTC-6 (MDT)

raven_banding_tagging (csv): metadata about each captured and process common ravens
	tag-id (fac): the individual identity of the raven, this also seperates ravens that had the same tag (e.g. 7490, 7490_2)
	molt (chr): if the raven was molting at the time of capture
	location (chr): name of landmark feature where the raven was captured
	inside NationalPark (bool): if the raven mainly lives inside Yellowstone (yes) or outside (no)
	breeding (chr): category that says if the raven is a breeder at the time of capture
	status (reviewed 8/1/24): category that says if the raven is a breeder as of 8/1/2024
	Fate (as of 8/1/24): category that says if the mortality status of the raven and working status of GPS tag
	Fate Date (date): date of death or tag failure
	cause of death (chr): the cause of death of the raven

wolf_project_carcass_data: information about all carcasses detected by the Yellowstone Wolf Project
	Kill number (chr): unique number that matches that provides the link to other data files for metadata about carcasses located
	Cougar Kill . (fac): unique ID for detected cougar kills (probably incomplete)
	STUDY PERIOD (chr): if the carcass was made during one of the Yellowstone Wolf Project winter study periods
	KILL TYPE (chr): category that says what the cause of death was for the carcass (probably incomplete)
	PACK TYPE (chr): UNKNOWN
	SPECIES ID (chr): category that says what the prey species is
	DOD (date): date of death (or approximate) of the carcass
	DOD ESTIMATED (bool): if the date of death is estimated (YES), meaning likely more than a few days period of uncertainty, or not (NO)
	DATE DETECTED GROUND (date): date that the carcass was detected by from the ground
	DATE DETECTED AIR (date): date that the carcass was detected from aerial surveys (most often via plane)
	N RANGE (bool): if the carcass was located within the northern range of Yellowstone (YES) or not (NO)
	LOCATION (chr): name of landmark feature where the carcass is located
	GROUND EAST (num): easting of the carcass using handheld GPS at the site
	GROUND NORTH (num): northing of the carcass using handheld GPS at the site
	ELEV (num, meter): elevation of the site of the carcass from an unknown source
	EST GROUND EAST (num): estimated easting of the carcass from the ground detection
	EST GROUND NORTH (num): estimated northing of the carcass from the ground detection
	AERIAL EAST (num): easting of the carcass from the aerial detection
	AERIAL NORTH (num): northing of the carcass from the aerial detection
	SEX (chr): category sex of the prey
	MALE AGE (chr): category that says the age class of the male based on antler growth
	AGE CLASS (chr): categorical age of the prey
	TEETH WEAR: (chr): UNKNOWN
	COD: cause of death of the carcass
	COD EXPLANATION: written description of how the cause of death was determined based on field evidence or direct observations
	WOLVES PRESENT: ID numbers of tagged wolves present at the carcass
	. OF WOLVES (num): number of wolves present at the carcass
	PACK (chr): the name of the pack that was first at the carcass
	COUGARS PRESENT (chr): which cougars were present at the carcass
	. OF COUGARS (num): number of cougars present at the carcass
	PRED/SCAVENGER PRESENT (bool): if the carcass had a predator or scavenger detected at the site (Y) or not (N)
	. OF GRIZZLY (num): number of grizzly bears scavengeing at the carcass detected from the ground
	. OF BLACK BEAR (num): number of black bears scavengeing at the carcass detected from the ground
	. OF COYOTE (num): number of coyotes scavengeing at the carcass detected from the ground
	. OF FOX (num): number of foxes scavengeing at the carcass detected from the ground
	. OF MT LION (num): number of cougars scavengeing at the carcass detected from the ground, not including the one identified as having created the carcass
	. OF RAVEN (num): number of ravens scavengeing at the carcass detected from the ground
	. OF MAGPIE (num): number of magpies scavengeing at the carcass detected from the ground
	. OF BALD EAGLE(num): number of bald eagles scavengeing at the carcass detected from the ground
	. OF GOLDEN EAGLE (num): number of golden eagles scavengeing at the carcass detected from the ground
	. OF UNKNOWN EAGLE (num): number of eagles of unknown species scavengeing at the carcass detected from the ground
	. OF WOLVES (num): number of wolves scavengeing at the carcass detected from the ground, not including the individuals from the pack/group identified as having created the carcass
	WOLF PACK SCAVENGED (chr): the name of the packs that scavenged at the carcass
	PRED/SCAVENGER OTHER (chr): speices of scavengers not represent in the count columns that was detected at the carcass
	PRED/SCAVENGER UNKNOWN (chr): UNKNOWN
	WOLF SCAVENGER UTILIZATION (fac): UNKNOWN
	KILL DISCOVERED (chr): category that says which sampling methods (aerial, hiking, or ground) first detected the carcass (multiple methods can detect it on the same day)
	GPS CLUSTER . (chr): unique identifier for the wolf or cougar GPS cluster that is associated with the carcass
	RANDOM POINT . (chr): UNKNOWN (blank column)
	OBS (chr): names of the observers that detected the carcass