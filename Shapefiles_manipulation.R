#### Introduction ####

# This script is to mainuplate all shapefiles required for this project  



#### Hyperparameters ####

# download hyperparameters
source('Hyper_parameters.R')

# various projections
crs_merc = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# default CRS projection to use for all shapefiles
crs_default = crs_merc


# export_flag = TRUE

#### Packages ####

# data manipulation packages
library(dplyr)
library(tidyr)

# mapping packages
library(rgdal)
library(sp)



#### Toronto Ward Subdivision SHP ####

# read in Ward/Subdivision shapefile
if (!exists('ward_subdivision_shp')){
  temp_path <- paste0(input_location, "Shapefiles\\Voting Subdivisions")
  ward_subdivision_shp <- readOGR(dsn = temp_path, layer = "VOTING_SUBDIVISION_2014_WGS84")}


# check (CRS) projection
ward_subdivision_shp@proj4string

# change projection
ward_subdivision_reproj_shp <- sp::spTransform(ward_subdivision_shp, 
                                               CRS = crs_default)

# check (CRS) projection
ward_subdivision_reproj_shp@proj4string

# change feature names to show the name of the file
# names(ward_subdivision_reproj_shp) <- paste0('SD_', names(ward_subdivision_reproj_shp))

# convert to dataframe
ward_subdivision_df <- as.data.frame(ward_subdivision_shp)



#### Census Tracts SHP ####

# read in census tracts shapefile
# if (!exists('census_tracts_shp')){
#   temp_path <- paste0(input_location, "Shapefiles\\Census Tracts")
#   census_tracts_shp <- readOGR(dsn = temp_path, layer = "gct_000b11a_e")}



#### Dissemination Area SHP ####

# read in census tracts shapefile
if (!exists('dissemination_area_shp')){
  temp_path <- paste0(input_location, "Shapefiles\\Dissemination Areas")
  dissemination_area_shp <- readOGR(dsn = temp_path, layer = "gda_000b11a_e")}

# convert to dataframe
dissemination_area_df <- as.data.frame(dissemination_area_shp)

# check (CRS) projection
dissemination_area_shp@proj4string

# change projection
dissemination_area_reproj_shp <- sp::spTransform(dissemination_area_shp, 
                                                 CRS = crs_default)

# check (CRS) projection
dissemination_area_reproj_shp@proj4string

# change feature names to show the name of the file
# names(dissemination_area_reproj_shp) <- paste0('DA_', names(dissemination_area_reproj_shp))


# some temporary filtering
# dissemination_area_shp_ontario <- dissemination_area_reproj_shp[(dissemination_area_reproj_shp$PRNAME=="Ontario"), ]
# dissemination_area_shp_toronto <- dissemination_area_reproj_shp[(dissemination_area_reproj_shp$CDNAME=="Toronto") & (dissemination_area_reproj_shp$DAUID=='35204460'), ]
# dissemination_area_shp_toronto <- dissemination_area_reproj_shp[(dissemination_area_reproj_shp$CDNAME=="Toronto"), ]

# visualize shapefile
# plot(dissemination_area_shp_ontario, axes = TRUE)

# extract centre-of-mass
trueCentroids = rgeos::gCentroid(dissemination_area_reproj_shp 
                                 , byid = TRUE
                                 # , id = c()
                                 )

# this is to show the code is working
# plot(dissemination_area_shp_toronto)
# points(trueCentroids, pch=2) # regos way to get center of polygon
# points(coordinates(dissemination_area_shp_toronto),pch=1) # another way to get center of polygon



#### Federal Electoral Districts SHP ####

# read in federal electoral districts shapefile
# if (!exists('federal_electoral_shp')){
#   temp_path <- paste0(input_location, "Shapefiles\\Federal Electoral Districts\\Polling Division Boundaries\\2015\\Digital")
#   federal_electoral_shp <- readOGR(dsn = temp_path, layer = "PD_A")}

# change projections
# federal_electoral_reproj_shp <- spTransform(federal_electoral_shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# alternative projections
# federal_electoral_reproj_shp <- spTransform(federal_electoral_shp, CRS("+init=epsg:26978"))
# federal_electoral_reproj_shp <- spTransform(federal_electoral_shp, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0+x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs"))

# plot
# plot(federal_electoral_reproj_shp, axes = TRUE)

# convert to dataframe
# federal_electoral_df <- as.data.frame(federal_electoral_shp, stringsAsFactors = FALSE) %>%
#   select(FED_NUM, PD_NUM) %>% 
#   distinct() %>%
#   arrange(FED_NUM, PD_NUM) %>%
#   filter(FED_NUM==35001)

# I wondered if PD_NUM matched Polling_Station_Number
# PD_NUM only goes upto 501 unlike Polling_Station_Number which goes upto 612  


# filter for just Ontario
# Use fednum for now
# federal_electoral_shp_toronto <- federal_electoral_shp[(federal_electoral_shp$FED_NUM >= 35001) & (federal_electoral_shp$FED_NUM <= 35121), ]
# plot(federal_electoral_shp_toronto, axes = TRUE)


# reduce the polygons to just points


# find out which points lie in which polygon of the ward/subdivision shapefile



#### Linking different shapefiles together ####

# overlay points
# if x = "SpatialPoints", y = "SpatialPolygons"
# sp::over returns a numeric vector of length equal to the number of points; the number is the index (number) of the polygon of y in which a point falls; NA denotes the point does not fall in a polygon; if a point falls in multiple polygons, the last polygon is recorded.

# extract centroids
trueCentroids = rgeos::gCentroid(dissemination_area_reproj_shp 
                                 , byid = TRUE
                                 )

# check (CRS) projection
trueCentroids@proj4string

# overlay dissemination area points over ward_subdivision
# note that this dataframe is mostly empty because our ward_subdivision file is only for Toronto
# This means that any dissemination area points outside of Toronto don't fall in any polygon within Toronto
temp <- sp::over(x = trueCentroids, 
                 y = ward_subdivision_reproj_shp) %>%
  bind_cols(dissemination_area_df)

# set row names as Area ID from the Dissemination Area shapefile
# temp$DAUID <- rownames(temp) 

# reset row names
# rownames(temp) <- 1:nrow(temp)

# temp <- temp %>%
  # select(AREA_ID, DAUID)

# add to master table
master_linkage <- data.frame(stringsAsFactors = FALSE)

master_linkage <- master_linkage %>%
  bind_rows(temp)

# Geo_code=35010155
# Geo_nom=155



#### Export procedures ####

# export master_linkage csv
if (export_flag==TRUE) {
  
  # check if directory exists and if not, then create it
  subDir <- "Shapefiles\\Linking_Shapefiles"
  
  ifelse(!dir.exists(file.path(output_location, subDir)), 
         dir.create(file.path(output_location, subDir)), 
         FALSE)
  
  # output to location
  temp_path = paste0(output_location, '\\', subDir, '\\Master_linkage.csv')
  write.csv(x = master_linkage, file = temp_path, row.names = FALSE, na = '')}


# export reprojected Toronto Ward Subdivision shapefile
if (export_flag==TRUE) {
  
  # check if directory exists and if not, then create it
  subDir <- "Shapefiles\\Toronto_Ward_Subdivisions"
  
  ifelse(!dir.exists(file.path(output_location, subDir)), 
         dir.create(file.path(output_location, subDir)), 
         FALSE)
  
  # output to location
  temp_path = paste0(output_location, '\\', subDir)
  writeOGR(ward_subdivision_reproj_shp, dsn = temp_path, layer = "toronto_ward_subdivision", driver="ESRI Shapefile")}





# export reprojected federal electoral districts shapefile
# if (export_flag==TRUE) {
#   
#   # check if directory exists and if not, then create it
#   subDir <- "Shapefiles\\Federal_Electoral_Districts"
#   ifelse(!dir.exists(file.path(output_location, subDir)), 
#          dir.create(file.path(output_location, subDir)), 
#          FALSE)
#   
#   # output to location
#   temp_path = paste0(output_location, '\\', subDir)
#   writeOGR(federal_electoral_reproj_shp, dsn = temp_path, layer = "federal_electoral_districts_15", driver="ESRI Shapefile")}
