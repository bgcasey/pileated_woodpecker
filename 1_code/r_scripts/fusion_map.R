aoi <- st_read("0_data/external/Alberta/alberta.shp")
aoi_s <- st_simplify(aoi)
aoi_t <- st_transform(aoi_s, crs = "EPSG:4326")
buffer_distance <- 2000 # Adjust the buffer distance as needed
aoi_buff <- st_buffer(aoi_t, dist = buffer_distance)


ls_pred_raster_mean <- rast(
  "3_output/models/ls_noOff_noYear/spatial_pred/mean_raster.tif"
)

s2_pred_raster_mean <- rast(
  "3_output/models/s2_noOff_noYear/spatial_pred/mean_raster.tif"
)


# Check if the CRS match
ls_transformed <- project(ls_pred_raster_mean, "EPSG:4326")
ls_crop <- crop(ls_transformed, aoi_buff)

s2_transformed <- project(s2_pred_raster_mean, "EPSG:4326")
s2_crop <- crop(s2_transformed, aoi_buff)

# Align the extents and resolutions
s2_resampled <- resample(s2_crop, ls_crop, method = "bilinear")

# Calculate the difference
raster_difference <- ls_crop - s2_resampled

# Calculate the mean
raster_mean <- (ls_crop + s2_resampled) / 2


# # Check if the CRS match
# if (!identical(crs(ls_pred_raster_mean), crs(s2_pred_raster_mean))) {
#   # If CRS do not match, reproject s2_pred_raster_mean to match ls_pred_raster_mean
#   s2_pred_raster_mean <- project(s2_pred_raster_mean, crs(ls_pred_raster_mean))
# }
# 
# # Align the extent and resolution of s2_pred_raster_mean to match ls_pred_raster_mean
# s2_pred_raster_mean_aligned <- resample(s2_pred_raster_mean, ls_pred_raster_mean, method = "bilinear")
# 
# # Calculate the difference
# raster_difference <- ls_pred_raster_mean - s2_pred_raster_mean_aligned
# 
# # Calculate the mean
# raster_mean <- (ls_pred_raster_mean + s2_pred_raster_mean_aligned) / 2
# Save
writeRaster(raster_difference, "3_output/fusion/noOff_noYear/difference_raster.tif", overwrite = TRUE)
writeRaster(raster_mean, "3_output/fusion/noOff_noYear/mean_raster.tif", overwrite = TRUE)

# Set up save
png(
  file = "3_output/fusion/noOff_noYear/prediction_map_2.png",
  width = 6, height = 8, units = "in", res = 300
)

## 6.2 Set plot parameters ----
plg <- list(
  cex = 0.7,
  shrink = 1
)
pax <- list(retro = TRUE)

# Set up a 2x2 plotting area
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2), oma = c(1, 1, 1, 1))

# Plot the data
plot(ls_crop,
     main = "",
     legend = TRUE,
     plg = plg,
     pax = pax
)
mtext("A", side = 3, line = -1, adj = -0.15, cex = .75, font = 2)
plot(aoi_t, col = adjustcolor("blue", alpha.f = 0.0), add = TRUE)

plot(s2_crop,
     main = "",
     legend = TRUE,
     plg = plg,
     pax = pax
)
mtext("B", side = 3, line = -1, adj = -0.15, cex = .75, font = 2)
plot(aoi_t, col = adjustcolor("blue", alpha.f = 0.0), add = TRUE)

plot(raster_difference,
     main = "",
     legend = TRUE,
     plg = plg,
     pax = pax
)
mtext("C", side = 3, line = -1, adj = -0.15, cex = .75, font = 2)
plot(aoi_t, col = adjustcolor("blue", alpha.f = 0.0), add = TRUE)

plot(raster_mean,
     main = "",
     legend = TRUE,
     plg = plg,
     pax = pax
)
mtext("D", side = 3, line = -1, adj = -0.15, cex = .75, font = 2)
plot(aoi_t, col = adjustcolor("blue", alpha.f = 0.0), add = TRUE)
# Close the device
dev.off()


# Extract values from the raster objects
ls_values <- values(ls_crop)
s2_values <- values(s2_crop)

# Remove NA values
valid_indices <- !is.na(ls_values) & !is.na(s2_values)
ls_values <- ls_values[valid_indices]
s2_values <- s2_values[valid_indices]

# Compute the correlation
correlation <- cor(ls_values, s2_values)
# Print the correlation
print(correlation)