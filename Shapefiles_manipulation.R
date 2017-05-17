#### Introduction ####

# This script is to mainuplate all shapefiles required for this project  



#### Packages ####

# data manipulation packages
library(dplyr)
library(tidyr)

# mapping packages
library(rgdal)
library(sp)



#### Shapefiles ####

# read ward/subdivision shapefile
if (!exists('ward_subdivision_shp')){
  temp_path <- paste0(input_location, "Shapefiles\\Voting Subdivisions")
  ward_subdivision_shp <- readOGR(dsn = temp_path, layer = "VOTING_SUBDIVISION_2014_WGS84")}


# convert to dataframe
ward_subdivision_df <- as.data.frame(ward_subdivision_shp)




# read federal/ shapefile
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
federal_electoral_df <- as.data.frame(federal_electoral_shp, stringsAsFactors = FALSE)

# filter for just Ontario


#### Export procedures ####
