#### Introduction ####

# This script is to build clean census data at the census tracts level



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
if (!exists('census_tracts')){
  temp_path <- paste0(input_location, 'Census Data\\Census Tract\\98-316-XWE2011001-401.csv')
  census_tracts <- read.csv(file = temp_path, header = FALSE, stringsAsFactors = FALSE)}



#### Clean data ####

# set default
census_tracts_cleaned <- census_tracts

# set correct header
colnames(census_tracts_cleaned) <- census_tracts_cleaned[2, ] 

# remove first 2 (non-data) rows
census_tracts_cleaned <- census_tracts_cleaned[-(1:2), ]

# exploration
temp <- census_tracts_cleaned %>%
  select(Topic, Characteristic) %>%
  distinct()

# select columns
census_tracts_cleaned <- census_tracts_cleaned %>%
  select(Geo_Code, Prov_Name, CMACA_Name, CT_Name, Topic, Characteristic, Total) %>%
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
if (export_flag==TRUE) {
  
  # check if directory exists and if not, then create it
  subDir <- "Census_data"
  
  ifelse(!dir.exists(file.path(output_location, subDir)), 
         dir.create(file.path(output_location, subDir)), 
         FALSE)
  
  # output to location
  # temp_path = paste0(output_location, '\\', subDir, '\\Master_linkage.csv')
  temp_path = paste0(output_location, '\\', subDir, '\\Census_Tracts.csv')
  write.csv(file = temp_path, x = census_tracts_cleaned, row.names = FALSE, na = '')}
