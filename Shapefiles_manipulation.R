#### Introduction ####

# This script is to mainuplate all shapefiles required for this project  



#### Hyperparameters ####

# download hyperparameters
source('C:\\Users\\fmqai\\Documents\\Election_Results\\Election_Results\\Hyper_parameters.R')



#### Packages ####

# data manipulation packages
library(dplyr)
library(tidyr)

# mapping packages
library(rgdal)
library(sp)



#### Shapefiles ####

# read in ward/subdivision shapefile
if (!exists('ward_subdivision_shp')){
  temp_path <- paste0(input_location, "Shapefiles\\Voting Subdivisions")
  ward_subdivision_shp <- readOGR(dsn = temp_path, layer = "VOTING_SUBDIVISION_2014_WGS84")}


# convert to dataframe
# ward_subdivision_df <- as.data.frame(ward_subdivision_shp)




# read in federal electoral districts shapefile
if (!exists('federal_electoral_shp')){
  temp_path <- paste0(input_location, "Shapefiles\\Federal Electoral Districts\\Polling Division Boundaries\\2015\\Digital")
  federal_electoral_shp <- readOGR(dsn = temp_path, layer = "PD_A")}

# change projections
# federal_electoral_reproj_shp <- spTransform(federal_electoral_shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# alternative projections
# federal_electoral_reproj_shp <- spTransform(federal_electoral_shp, CRS("+init=epsg:26978"))
# federal_electoral_reproj_shp <- spTransform(federal_electoral_shp, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0+x_0=0.0 +y_0=0 +k=1.0 +units=m +no_defs"))

# plot
# plot(federal_electoral_reproj_shp, axes = TRUE)



# convert to dataframe
federal_electoral_df <- as.data.frame(federal_electoral_shp, stringsAsFactors = FALSE) %>%
  select(FED_NUM, PD_NUM) %>% 
  distinct() %>%
  arrange(FED_NUM, PD_NUM) %>%
  filter(FED_NUM==35001)

# I wondered if PD_NUM matched Polling_Station_Number
# PD_NUM only goes upto 501 unlike Polling_Station_Number which goes upto 612  


# filter for just Ontario
# Use fednum for now
federal_electoral_shp_toronto <- federal_electoral_shp[(federal_electoral_shp$FED_NUM >= 35001) & (federal_electoral_shp$FED_NUM <= 35121), ]
plot(federal_electoral_shp_toronto, axes = TRUE)


# reduce the polygons to just points


# find out which points lie in which polygon of the ward/subdivision shapefile




#### Export procedures ####

# export reprojected federal electoral districts shapefile
if (export_flag==TRUE) {
  
  # check if directory exists and if not, then create it
  subDir <- "Shapefiles"
  ifelse(!dir.exists(file.path(output_location, subDir)), 
         dir.create(file.path(output_location, subDir)), 
         FALSE)
  
  # output to location
  temp_path = paste0(output_location, '\\', subDir)
  writeOGR(federal_electoral_reproj_shp, dsn = temp_path, layer = "federal_electoral_districts_15", driver="ESRI Shapefile")}
