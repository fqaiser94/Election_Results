# yes
#### Introduction ####

# This script is to set up the workspace for this project
# Download necessary files
# Set up folder structure



#### Download data from source ####

# Download all federal election results for Ontario

temp <- paste0(input_location, '\\Election Results\\Federal Election results\\35001.csv')

if(!file.exists(temp)) {
  
  for (i in 35001:35121) {
    
    # Example url
    #http://www.elections.ca/res/rep/off/ovr2015app/41/data_donnees/pollresults_resultatsbureau35001.csv
    
    # create url to download from
    temp_url_beg <- 'http://www.elections.ca/res/rep/off/ovr2015app/41/data_donnees/pollresults_resultatsbureau'
    temp_url_end <- '.csv'
    temp_url <- paste0(temp_url_beg, i, temp_url_end)
    
    # create destination file to download to
    temp_path = paste0(input_location, '\\Election Results\\Federal Election results\\')
    temp_dest_file <- paste0(temp_path, i, ".csv")
    
    # download file from url to specified location
    download.file(temp_url, temp_dest_file, mode="wb")
    
    # remove temporary variables
    rm(list = ls(patter = 'temp.*'))
  }
}