library(sf)
library(terra)
library(tidyverse)

# read in points
ss_xy<- read_csv("3_output/data/Anzac_region.csv")

# convert to spatial object
ss_xy_4326<-ss_xy%>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

class<-rast("class.tif")
class<-terra::extract(reclassified_raster, ss_xy_4326, fun=max)

moved_classes<-cbind(ss_xy_4326, class)%>%
  as.data.frame()%>%
  dplyr::select(-geometry)

write_csv(moved_classes, "moved_classes.csv")



##/////
# Install and load the necessary packages
install.packages("readtext")
library(readtext)
library(tidyverse)

# Read the Word document
doc <- readtext(paste0("0_data/external/Simran_Bains/fieldwork_2024/",
                       "field_notes_2024.docx"))$text

# Split the text into lines
lines <- strsplit(doc, "\n")[[1]]

# Split each line at the colon, with or without a space
split_lines <- strsplit(lines, ": ?")

# Convert the list to a data frame
df <- data.frame(
  ss = sapply(split_lines, `[`, 1),
  notes = sapply(split_lines, `[`, 2)
)

# Add the "WOOD-GT-" prefix if the entry is a lone number
df$ss[grepl("^[0-9]+$", df$ss)] <- paste0("WOOD-GT-", 
                                          df$ss[grepl("^[0-9]+$", 
                                                      df$ss)])
field_notes <- df
save(field_notes, file = "0_data/manual/wood_field_notes_2024.RData")



library(terra)
ua<-rast("~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/My Drive/3_Resources/images/logos/UA_Logo_Green_RGB.tiff")
# Assign a CRS
crs(ua) <- "EPSG:4326"  # WGS 84

# Write the raster to a new GeoTIFF file
writeRaster(ua, "~/path/to/output/UA_Logo_Green_RGB_GeoTIFF.tiff", format = "GTiff")



#-------
# dummy data ----
# Adjust the number of rows to 5000 for the dummy dataframe
set.seed(123) # For reproducibility

# Generate covariates
cov_1 = runif(500, 0, 100) # Continuous covariate
cov_2 = rnorm(500, 50, 10) # Continuous covariate
cov_3 = sample(1:10, 500, replace = TRUE) # Discrete covariate
cov_4 = rnorm(500, 0, 1) # Continuous covariate
cov_5 = runif(500, -50, 50) # Continuous covariate

# Calculate a linear combination of covariates
linear_combination = -2 + 0.05 * cov_1 - 0.04 * cov_2 + 0.5 * cov_3 + 
  0.1 * cov_4 - 0.02 * cov_5

# Apply logistic function to get probabilities
probabilities = 1 / (1 + exp(-linear_combination))

# Generate PIWO based on probabilities
PIWO = rbinom(500, 1, probabilities)

# Create the dataframe
dummy_df <- data.frame(
  PIWO = PIWO,
  PIWO_offset = rnorm(500, 0, 1),
  cov_1 = cov_1,
  cov_2 = cov_2,
  cov_3 = cov_3,
  cov_4 = cov_4,
  cov_5 = cov_5
)

cvstats <- as.data.frame(brt_1$cv.statistics[c(1, 3, 5)])
cvstats$deviance.null <- brt_1$self.statistics$mean.null
cvstats$deviance.explained <- 
  (cvstats$deviance.null - cvstats$deviance.mean) / 
  cvstats$deviance.null
cvstats[[model_name]] <- rep(model_name, nrow(cvstats))
colnames(cvstats)[colnames(cvstats) == "discrimination.mean"] <- "AUC"


