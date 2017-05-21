#### Introduction ####

# This script is to build clean census data at the dissemination area level



#### Hyperparamaters ####

# download hyperparameters
source('Hyper_parameters.R')




#### Packages ####

# data manipulation packages
library(dplyr)
library(tidyr)




#### Custom functions ####

source('Custom_functions.R')



#### Load data ####

# read in data
if (!exists('dissemination_area')){
  temp_path <- paste0(input_location, 'Census Data\\Dissemination Area\\98-316-XWE2011001-1501-ONT.csv')
  dissemination_area <- read.csv(file = temp_path, header = TRUE, stringsAsFactors = FALSE)}

# quickly view data
temp <- head(dissemination_area)

#### Clean data ####

# set default
dissemination_area_cleaned <- dissemination_area

# exploration
temp <- dissemination_area_cleaned %>%
  select(Topic, Characteristic) %>%
  distinct()

# select columns
dissemination_area_cleaned <- dissemination_area_cleaned %>%
  select(Geo_Code, Prov_name, Geo_nom, Topic, Characteristic, Total) %>%
  mutate(Total = as.numeric(Total)
         # not a good idea as shapefile seems to contain the decimal places as well
         # Geo_Code = as.integer(Geo_Code), 
         # CT_Name = as.integer(CT_Name)
         )


# Selected Topics
# Age characteristics
# Household and dwelling characteristics
# Detailed mother tongue
  


#### Export procedures ####

# export all census tracts data
export_flag = TRUE
if (export_flag==TRUE) {
  
  # check if directory exists and if not, then create it
  subDir <- "Census_data"
  
  ifelse(!dir.exists(file.path(output_location, subDir)), 
         dir.create(file.path(output_location, subDir)), 
         FALSE)
  
  # output to location
  # temp_path = paste0(output_location, '\\', subDir, '\\Master_linkage.csv')
  temp_path = paste0(output_location, '\\', subDir, '\\Dissemination_Area.csv')
  write.csv(file = temp_path, x = dissemination_area_cleaned, row.names = FALSE, na = '')}
