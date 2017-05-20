#### Introduction ####

# This script is to build clean census data at the neighbourhood level



#### Hyperparamaters ####

# download hyperparameters
source('Hyper_parameters.R')




#### Packages ####

# reading data packages
library(XLConnect)

# data manipulation packages
library(dplyr)
library(tidyr)




#### Custom functions ####

source('Custom_functions.R')


#### Load data ####

# Considering whether its wise to use data at the census tracts level or the FSA level
temp_path <- paste0(input_location, 'Census Data\\Census Tract\\98-316-XWE2011001-401.csv')

temp <- read.csv(file = temp_path, stringsAsFactors = FALSE)

