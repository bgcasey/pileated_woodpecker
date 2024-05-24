# ---
# title: "misc_functions"
# author: "Brendan Casey"
# created: "2024-05-24"
# description: " Miscellaneous R functions."
# ---

## ////////////////////////////////////////////////////////////////

## points_to_squares----
# Function to create a square from a center point and radius
create_square <- function(center, radius) {
  # Calculate the half side length of the square (which is the radius 
  # of the circle)
  half_side <- radius
  
  # Coordinates of the center
  x <- center[1]
  y <- center[2]
  
  # Calculate the vertices of the square
  vertices <- matrix(nrow = 5, ncol = 2,
                     data = c(x - half_side, y - half_side,
                              x + half_side, y - half_side,
                              x + half_side, y + half_side,
                              x - half_side, y + half_side,
                              x - half_side, y - half_side), 
                     byrow = TRUE)
  
  # Create an sf polygon
  square <- st_polygon(list(vertices))
  return(square)
}

# Function to convert sf points to sf squares
points_to_squares <- function(points, radius) {
  # Extract coordinates from points
  coords <- st_coordinates(points)
  
  # Create a list to store squares
  squares <- list()
  
  # Loop over each point and create a square
  for (i in 1:nrow(coords)) {
    center <- coords[i, ]
    square <- create_square(center, radius)
    squares[[i]] <- square
  }
  
  # Convert list of squares to sf object
  squares_sf <- st_sfc(squares)
  
  # Create a new sf object that includes the fields from the points
  squares_sf <- st_sf(data = st_drop_geometry(points), 
                      geometry = squares_sf)
  
  # Remove 'data.' prefix from field names
  names(squares_sf) <- gsub("^data\\.", "", names(squares_sf))
  
  # Return sf object of squares
  return(squares_sf)
}

# Example usage
# Load the sf package
# library(sf)

## Create some example points
# points <- st_as_sf(data.frame(x = c(1, 2, 3), y = c(1, 2, 3)), 
#                    coords = c("x", "y"), crs = 4326)
# points <- st_transform(points, crs=3348) #convert to meters

## Define the radius
# radius <- 50

## Convert the points to squares
# squares <- points_to_squares(points, radius)

## Check the result
# plot(squares)

## ////////////////////////////////////////////////////////////////


