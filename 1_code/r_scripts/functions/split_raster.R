#' Split Raster into Smaller Chunks
#'
#' This function splits a given raster into smaller chunks based on the
#' specified number of rows and columns.
#'
#' @param raster SpatRaster. The input raster to be split.
#' @param n_rows Integer. The number of rows to split the raster into.
#' @param n_cols Integer. The number of columns to split the raster into.
#' @return List of SpatRaster objects. A list containing the smaller raster
#' chunks.
#' 
#' @example 
#' # Example usage of the function
#' library(terra)
#' pred_raster <- rast("path/to/your/raster.tif")
#' chunks <- split_raster(pred_raster, 3, 2)
#' print(chunks)
split_raster <- function(raster, n_rows, n_cols) {
  # Step 1: Get the extent of the raster
  ext <- ext(raster)
  
  # Step 2: Calculate the width and height of each tile
  x_step <- (ext[2] - ext[1]) / n_cols
  y_step <- (ext[4] - ext[3]) / n_rows
  
  # Step 3: Initialize a list to store the chunks
  chunks <- list()
  
  # Step 4: Loop through rows and columns to create the chunks
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      # Define the extent for the current tile
      x_min <- ext[1] + (j-1) * x_step
      x_max <- ext[1] + j * x_step
      y_min <- ext[3] + (i-1) * y_step
      y_max <- ext[3] + i * y_step
      
      # Create the extent for the current chunk
      chunk_ext <- ext(x_min, x_max, y_min, y_max)
      
      # Crop the raster to the current extent
      chunk <- crop(raster, chunk_ext)
      
      # Store the chunk in the list
      chunks[[length(chunks) + 1]] <- chunk
    }
  }
  
  # Step 5: Return the list of chunks
  return(chunks)
}