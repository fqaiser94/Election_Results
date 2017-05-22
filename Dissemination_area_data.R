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

# read in master linkage
if (!exists('geo_linkage')){
  temp_path <- paste0(output_location, 'Shapefiles\\Linking_Shapefiles\\Master_linkage.csv')
  geo_linkage <- read.csv(file = temp_path, header = TRUE, stringsAsFactors = FALSE)}



#### Clean data ####

# set default
dissemination_area_cleaned <- dissemination_area

# select columns
dissemination_area_cleaned <- dissemination_area_cleaned %>%
  # some cleaning up
  mutate(Total = as.numeric(Total)
         , Topic = gsub(x = Topic, pattern = "^\\s+|\\s+$", replacement = "")
         , Characteristic = gsub(x = Characteristic, pattern = "^\\s+|\\s+$", replacement = "")
         ) %>%
  # remove rows like this, only want to keep aggregate rows to make things easy for end-user
  filter(!grepl(x = Characteristic, pattern = '^[0-9][0-9] years$')) %>%
  # Add Subdivision
  # left_join(select(geo_linkage, DAUID, AREA_NAME), 
  #           c('Geo_Code'='DAUID')) %>%
  # select final columns
  select(
         # AREA_NAME, 
         Geo_Code, Prov_name, Geo_nom, Topic, Characteristic, Total)

# exploration
temp <- dissemination_area_cleaned %>%
  select(Topic, Characteristic) %>%
  distinct()


#### Aggregate to Subdivision Level #### 

# Selected Topics
# Age characteristics
# Household and dwelling characteristics
# Detailed mother tongue

subdivision <- dissemination_area_cleaned %>%
  # Topics to completely remove
  filter(!(Topic %in% c('Marital status'
                        , 'Detailed mother tongue'
                        , 'Knowledge of official languages'
                        , 'First official language spoken'
                        , 'Detailed other language spoken regularly at home'
                        ))) %>%
  # Characteristics in specific topics to remove
  filter(!(Characteristic %in% c(# Population and dwelling counts
                                 'Population density per square kilometre'
                                 , 'Land area (square km)'
                                 , '2006 to 2011 population change (%)'
                                 # Age characteristics
                                 , 'Total population by age groups'
                                 , 'Median age of the population'
                                 , '% of the population aged 15 and over'
                                 # Family characteristics
                                 , 'Total number of census families in private households'
                                 , 'Size of census family: 2 persons'
                                 , 'Size of census family: 3 persons'
                                 , 'Size of census family: 4 persons'
                                 , 'Size of census family: 5 or more persons'
                                 , 'Total couple families by family structure and number of children'
                                 , 'Married couples'
                                 #, 'Without children at home'
                                 #, 'With children at home'
                                 , '1 child'
                                 , '2 children'
                                 , '3 or more children'
                                 , 'Common-law couples'
                                 , 'Total lone-parent families by sex of parent and number of children'
                                 , 'Female parent'
                                 , 'Male parent'
                                 , 'Total children in census families in private households'
                                 , 'Under six years of age'
                                 , '6 to 14 years'
                                 , '15 to 17 years'
                                 , '18 to 24 years'
                                 , '25 years and over'
                                 , 'Average number of children at home per census family'
                                 # Household and dwelling characteristics
                                 , 'Total number of persons in private households'
                                 , 'Number of persons not in census families'
                                 , 'Living with relatives'
                                 , 'Living with non-relatives only'
                                 , 'Living alone'
                                 , 'Number of census family persons'
                                 , 'Average number of persons per census family'
                                 , 'Total number of persons aged 65 years and over in private households'
                                 , 'Number of persons not in census families aged 65 years and over'
                                 , 'Number of census family persons aged 65 years and over'
                                 , 'Total number of private households by household type'
                                 , 'Census-family households'
                                 , 'One-family-only households'
                                 , 'Couple-family households'
                                 , 'Without children'
                                 , 'With children'
                                 , 'Lone-parent-family households'
                                 , 'Other family households'
                                 , 'One-family households with persons not in a census family'
                                 , 'Two-or-more-family households'
                                 , 'Non-census-family households'
                                 , 'One-person households'
                                 , 'Two-or-more-person households'
                                 , 'Total number of occupied private dwellings by structural type of dwelling'
                                 #, 'Single-detached house'
                                 #, 'Apartment, building that has five or more storeys'
                                 #, 'Movable dwelling'
                                 #, 'Other dwelling'
                                 #, 'Semi-detached house'
                                 #, 'Row house'
                                 #, 'Apartment, duplex'
                                 #, 'Apartment, building that has fewer than five storeys'
                                 #, 'Other single-attached house'
                                 , 'Total number of private households by household size'
                                 , '1 person'
                                 , '2 persons'
                                 , '3 persons'
                                 , '4 persons'
                                 , '5 persons'
                                 , '6 or more persons'
                                 , 'Number of persons in private households'
                                 , 'Average number of persons in private households'
                                 # Detailed language spoken most often at home
                                 , 'Detailed language spoken most often at home - Total population excluding institutional residents'
                                 , 'Single responses'
                                 , 'Multiple responses'
                                 , 'Selected Aboriginal languages'
                                 , 'Selected non-Aboriginal languages'
                                 , 'Non-official languages'
                                 ))) %>%
  filter(!is.na(Total)) %>%
  # Add Subdivision
  left_join(select(geo_linkage, DAUID, AREA_ID), 
            c('Geo_Code'='DAUID')) %>%
  # filter for only those areas we have a shapefile for
  filter(!is.na(AREA_ID)) %>%
  # aggregate data at the Subdivision level
  group_by(AREA_ID, Topic, Characteristic) %>%
    summarise(Total = sum(Total, na.rm = TRUE)) %>%
  ungroup() 
  # Final columns
  # select(Geo_code, Geo_nom, Topic, Characteristic, Total)
  

# exploration
temp <- subdivision %>%
  select(Topic, Characteristic) %>%
  distinct()



#### Export procedures ####

# export all dissemination area level data
if (export_flag==TRUE) {
  
  # check if directory exists and if not, then create it
  subDir <- "Census_data"
  
  ifelse(!dir.exists(file.path(output_location, subDir)), 
         dir.create(file.path(output_location, subDir)), 
         FALSE)
  
  # output to location
  temp_path = paste0(output_location, '\\', subDir, '\\Dissemination_Area.csv')
  write.csv(x = dissemination_area_cleaned, file = temp_path, row.names = FALSE, na = '')}


# export all subdivision level data
if (export_flag==TRUE) {
  
  # check if directory exists and if not, then create it
  subDir <- "Census_data"
  
  ifelse(!dir.exists(file.path(output_location, subDir)), 
         dir.create(file.path(output_location, subDir)), 
         FALSE)
  
  # output to location
  temp_path = paste0(output_location, '\\', subDir, '\\Subdivision.csv')
  write.csv(x = subdivision, file = temp_path, row.names = FALSE, na = '')}
