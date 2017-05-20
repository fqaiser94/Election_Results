#### Introduction ####

# This script contains some commonly used custom functions
# A bit like stored procedures


#### Extract file paths ####

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


