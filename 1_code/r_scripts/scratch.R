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


