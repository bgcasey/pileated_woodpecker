

#Specify the full path to the directory containing the RData files
directory_path <- "3_output/model_results/WT"

# List all RData files in the directory that include the string "varimp"
file_list <- list.files(path = directory_path, pattern = "varimp.*\\.RData", full.names = TRUE, ignore.case = TRUE)

# Load each RData file into the R environment
for (file in file_list) {
  load(file)
}



# List all RData files in the directory that include the string "varimp"
file_list <- list.files(path = directory_path, pattern = "varimp.*\\.RData", full.names = TRUE, ignore.case = TRUE)

# Initialize an empty data frame to store the combined data
data_frames <- list()
# Check if there are files in the list
if (length(file_list) > 0) {
  # Load each RData file into the R environment
  for (file in file_list) {
    loaded_data <- load(file)  # Use readRDS to load RData files
    data_frames <- c(data_frames, list(loaded_data))
  }
  
  # Check if all elements in data_frames are data frames
  if(all(sapply(data_frames, inherits, "data.frame"))) {
    # Combine all data frames into a single data frame using dplyr's bind_rows
    combined_data <- bind_rows(data_frames, .id = "source_file")
  } else {
    stop("Not all loaded objects are data frames.")
  }
} else {
  cat("No matching files found.\n")
}



  # List all RData files in the directory that include the string "varimp"
file_list <- list.files(path = directory_path, pattern = "cvstat.*\\.RData", full.names = TRUE, ignore.case = TRUE)

# Load each RData file into the R environment
for (file in file_list) {
  load(file)
}



cbind()


