#### Introduction ####

# This script is to build the core datasets 



#### Hyperparamaters ####

# download hyperparameters
source('C:\\Users\\fmqai\\Documents\\Election_Results\\Election_Results\\Hyper_parameters.R')



#### Packages ####

# reading data packages
library(xlsx) # if you get an rJava error, use the following link to download and install the right version of Java: https://www.java.com/en/download/manual.jsp

# data manipulation packages
library(dplyr)
library(tidyr)

# mapping packages
library(rgdal)
library(sp)



#### Custom Functions ####

pathPrep <- function(path) {
  new_path <- gsub(x = path, pattern = "/", replacement= "\\\\")
  return(new_path)
}


extract_file_paths <- function(filename_like_string, ...) {
  
  # Given a filename_like_string, returns all similar files in input location
  
  # get all file paths
  all_file_paths <- list.files(path = input_location, pattern = '*', recursive = TRUE)
  
  # filter for only those that match our requirements (i.e. that match filename_like)
  all_file_paths <- tolower(all_file_paths)
  filename_like_string <- tolower(filename_like_string)
  
  all_relevant_file_paths <- all_file_paths[grepl(x = all_file_paths, pattern = filename_like_string)]
  
  # clean up file path for Windows
  all_relevant_file_paths <- gsub(x = all_relevant_file_paths, pattern = '/', replacement = '\\\\')
  
  # create full length filepath
  all_relevant_full_length_file_paths = paste0(input_location, all_relevant_file_paths)
  
  return(all_relevant_full_length_file_paths)
}

# extract_file_paths(filename_like_string = ".*councillor.*")



extract_sheets_as_list_of_dataframes <- function(temp_path) {
  
  # load workbook
  wb <- loadWorkbook(temp_path)
  
  # get the names of all the sheets in workbook
  sheets <- names(getSheets(wb)) 
  
  # initialize a new list
  list_of_dataframes <- list()
  
  # for each sheet in workbook
  for (i in 1:length(sheets)) {
    list_of_dataframes[[sheets[i]]] <- read.xlsx(file = temp_path, sheetName = sheets[i], header = FALSE, stringsAsFactors = FALSE)
  }
  
  return(list_of_dataframes)
}



remove_empty_columns_and_rows <- function(df) {
  
  # Given a dataframe, returns a dataframe without any empty columns and rows
  
  # set default 
  df_cleaned <- df
  
  # remove empty rows
  df_cleaned <- df_cleaned[rowSums(is.na(df_cleaned)) != ncol(df_cleaned), ]
  
  # remove empty columns
  df_cleaned <- df_cleaned[ , colSums(is.na(df_cleaned)) != nrow(df_cleaned)]
  
  return(df_cleaned)}




##### Councillor Election results ####

# start with councillor race

# read in data 
if (!exists('councillors_2003')) {
  
  # extract all filepaths for councillor workbook
  temp_all_councillor_file_paths <- extract_file_paths(filename_like = ".*(councillor).*")
  
  # create appropriate name for each list of dataframes we create in the next FOR loop
  temp_year <- sub(pattern = ".*?(\\d+).*", replacement = "\\1", x = temp_all_councillor_file_paths)
  temp_listname <- paste0('councillors_', temp_year)
  
  for (i in 1:length(temp_all_councillor_file_paths)) {
    
    # extract sheets from the relevant workbook
    temp <- extract_sheets_as_list_of_dataframes(temp_path = temp_all_councillor_file_paths[i])
    
    # assign appropriate name to list 
    assign(x = temp_listname[i], value = temp)
  }
  
  rm(list = ls(patter = 'temp.*'))}

# transformed data

councillors_2003_cleaned <- councillors_2003
councillors_2006_cleaned <- councillors_2006
councillors_2010_cleaned <- councillors_2010
councillors_2014_cleaned <- councillors_2014



# write tests for input dataframes

# temp <- sample(x = 1:length(councillors_2003_cleaned), size = 1)
# print(temp)
# temp_2003 <- councillors_2003_cleaned[[temp]]
# 
# temp <- sample(x = 1:length(councillors_2006), size = 1)
# print(temp)
# temp_2006 <- councillors_2006[[temp]]
# 
# temp <- sample(x = 1:length(councillors_2010), size = 1)
# print(temp)
# temp_2010 <- councillors_2010[[temp]]
# 
# temp <- sample(x = 1:length(councillors_2014), size = 1)
# print(temp)
# temp_2014 <- councillors_2014[[temp]]




# Give dataframes in each of the lists-of-dataframes the right structure before we move to transformation steps

# make 2003 dataframes similar to other dataframes 
temp_function <- function(df) {
  
  # set default
  df_cleaned <- df
  
  # remove empty columns and rows
  df_cleaned <- remove_empty_columns_and_rows(df = df_cleaned)
  
  # extract ward number for dataframe 
  ward_number = gsub(x = df_cleaned[1,], pattern = '[^0-9]', replacement = '')
  ward_number = as.numeric(ward_number)
  ward_number = ward_number[!is.na(ward_number)]
  # print(ward_number)
  
  # replace cell[1,1] with a similar value to what we see in later years reports
  df_cleaned[1,1] <- paste0('Poll By Poll report for: COUNCILLOR Ward: ', ward_number)
  df_cleaned[1, 2:ncol(df_cleaned)] <- NA
  
  # replace any references to 'Name' with Subdivision
  temp_function <- function(x) {gsub(x = x, pattern = "^Name$", replacement = "Subdivision")}
  df_cleaned <- as.data.frame(lapply(df_cleaned, temp_function), stringsAsFactors = FALSE)
  
  return(df_cleaned)
}

councillors_2003_cleaned <- lapply(councillors_2003_cleaned, temp_function)

# temp <- councillors_2003_cleaned[[34]]



# clean up data 

temp_function <- function(df, year) {
  
  # Given a dataframe of a fairly defined structured (see below), returns a cleaned dataframe
  # The dataframe should contain the header in the second row
  
  # set default
  df_cleaned <- df
  
  # set column names
  colnames(df_cleaned) <- df_cleaned[2, ]
  colnames(df_cleaned) <- paste0('Subdivision', colnames(df_cleaned))
  colnames(df_cleaned)[1] <- "Candidate"
  
  # extract ward number for later step
  ward_number = as.numeric(gsub(x = df_cleaned[1,1], pattern = '[^0-9]', replacement = ''))
  
  # remove any columns that contain "subdivision" after the first column as these are duplicates of the first
  remove <- grepl(pattern = ".*subdivision.*", x = tolower(df_cleaned[1:2, ]))
  remove[1] <- FALSE
  df_cleaned <- df_cleaned[, !remove]
  
  # remove columns containing the word 'total' in the top 2 (header) rows
  remove <- grepl(pattern = ".*total.*", x = tolower(df_cleaned[1:2, ]))
  df_cleaned <- df_cleaned[ , !remove]
  
  # remove rows containing the word 'total' in the bottom (summary) row
  remove <- grepl(pattern = ".*total.*", x = tolower(df_cleaned[ , 1]))
  df_cleaned <- df_cleaned[!remove, ]
  
  # remove first 2 rows (header + messy column names)
  df_cleaned <- df_cleaned[-(1:2), ]
  
  # gather columns
  df_cleaned <- df_cleaned %>%
    gather(key = Subdivision, value = Votes, 2:ncol(df_cleaned)) %>%
    mutate(Ward = ward_number,
           Year = year,
           Subdivision = as.numeric(gsub(x = Subdivision, pattern = '[^0-9]', replacement ='')),
           Votes = as.numeric(Votes)
    ) %>%
    select(Year, Ward, Candidate, Subdivision, Votes)
  
  return(df_cleaned)
  
}

councillors_2003_cleaned <- lapply(councillors_2003_cleaned, temp_function, year = 2003)
councillors_2006_cleaned <- lapply(councillors_2006_cleaned, temp_function, year = 2006)
councillors_2010_cleaned <- lapply(councillors_2010_cleaned, temp_function, year = 2010)
councillors_2014_cleaned <- lapply(councillors_2014_cleaned, temp_function, year = 2014)



# one table to rule them all! LOTR
all_councillor_data <- data.frame(stringsAsFactors = FALSE)

all_councillor_data <-  all_councillor_data %>%
  bind_rows(plyr::ldply(councillors_2003_cleaned, rbind)) %>%
  bind_rows(plyr::ldply(councillors_2006_cleaned, rbind)) %>%
  bind_rows(plyr::ldply(councillors_2010_cleaned, rbind)) %>%
  bind_rows(plyr::ldply(councillors_2014_cleaned, rbind)) %>%
  rename(SourceExcelSheet = .id) %>%
  # AreaName will be our joining ID
  mutate(AreaName = Ward*1000 + Subdivision, 
         AreaName = as.character(AreaName), 
         AreaName = ifelse(nchar(AreaName)<5, paste0('0', AreaName), AreaName)
         ) %>%
  # Clean up names
  mutate(Candidate = gsub(x = Candidate, pattern = ',', replacement = ''), 
         Candidate = tolower(Candidate), 
         Candidate = gsub(x = Candidate,  pattern = "(?<=\\b)([a-z])", replacement = "\\U\\1", perl=TRUE)
         ) %>%
  # final columns
  select(Year, Ward, Candidate, Subdivision, Votes, AreaName, SourceExcelSheet)


data_dictionary <- data.frame(a = c('Year refers to the election year',
                                    'Ward refers to the ward the Candidate was running for',
                                    'Subdivision refers to the subdivisions of a ward candidate received votes in ???',
                                    'Votes refers to the number of votes the candidates received in a given subdivision of a ward', 
                                    'AreaName is a calculate field based on Ward and Subdivision to be used as a joining field for shapefiles',
                                    'SourceExcelSheet refers to the Excel sheet from where the data originated for each record', 
                                    '',
                                    paste0('Prepared by Farooq Qaiser on ', Sys.Date())), 
                              stringsAsFactors = FALSE)




#### Federal Election results ####


# read in all files as list 

temp_federal_elections_2014 <- data.frame(stringsAsFactors = FALSE)

if (!exists('federal_elections_2014')) {
  
  # extract all filepaths for councillor workbook
  temp_all_federal_file_paths <- extract_file_paths(filename_like = ".*(Federal Election results).*")
  
  for (i in 1:length(temp_all_federal_file_paths)) {
    
    # for debugging
    # print(temp_all_federal_file_paths[i])
    
    # extract sheets from the relevant workbook
    temp_federal_elections_2014 <- bind_rows(temp_federal_elections_2014, read.csv(file = temp_all_federal_file_paths[i], header = TRUE, stringsAsFactors = FALSE, colClasses = 'character'))
    
  }
  
}

# assign to final federal_elections_2014 dataframe
federal_elections_2014 <- temp_federal_elections_2014




# set default column name
temp_colnames <- colnames(federal_elections_2014)
# remove french parts
temp_colnames <- gsub(x = temp_colnames, pattern = 'Num.\\..*|Nom.*|Indicateur.*|Fusionn.*|Bulletins.*|Second.*|Pr.*|Appartenance.*|Votes\\.du.*|lecteurs.*', replacement = '')
# remove non-ASCII characters
temp_colnames <- iconv(temp_colnames, "latin1", "ASCII", sub="")
# remove '.s'
temp_colnames <- gsub(x = temp_colnames, pattern = '\\.s', replacement = '')
# remove all '_' 
temp_colnames <- gsub(x = temp_colnames, pattern = '_', replacement = '\\.')
# replace '..' with '.'
temp_colnames <- gsub(x = temp_colnames, pattern = '\\.\\.', replacement = '\\.')
# remove trailing and leading '.'
temp_colnames <- gsub(x = temp_colnames, pattern = '^\\.|\\.$', replacement = '')
# replace dots with '_'
temp_colnames <- gsub(x = temp_colnames, pattern = '\\.', replacement = '_')
# set new (clean) column names
colnames(federal_elections_2014) <- temp_colnames

# set as numeric column
federal_elections_2014$Electoral_District_Number <- as.numeric(federal_elections_2014$Electoral_District_Number)
federal_elections_2014$Rejected_Ballots_for_Polling_Station <- as.numeric(federal_elections_2014$Rejected_Ballots_for_Polling_Station)
federal_elections_2014$Electors_for_Polling_Station <- as.numeric(federal_elections_2014$Electors_for_Polling_Station)
federal_elections_2014$Candidate_Poll_Votes_Count <- as.numeric(federal_elections_2014$Candidate_Poll_Votes_Count)

# one table to rule them all! LOTR
all_federal_elections_data <- data.frame(stringsAsFactors = FALSE)

all_federal_elections_data <-  all_federal_elections_data %>%
  bind_rows(federal_elections_2014)



#### Export procedures ####

# export all Councillor Elections data
if (export_flag==TRUE) {
  temp_path = paste0(output_location, 'Councillor_Elections_results.csv')
  write.csv(file = temp_path, x = all_councillor_data, row.names = FALSE, na = '')}


# export all Federal Elections data
if (export_flag==TRUE) {
  temp_path = paste0(output_location, 'Federal_Elections_results.csv')
  write.csv(file = temp_path, x = all_federal_elections_data, row.names = FALSE, na = '')}