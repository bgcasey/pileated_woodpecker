/*
 * ---
 * title: "Extract spatial data"
 * author: "Brendan Casey"
 * created: "2024-07-01"
 * description: "Processes satellite imagery and environmental data
 * layers for spatial analysis. It includes data preparation, image
 * processing, summarizing metrics to point locations, and
 * focal statistics. The code outputs several CSV files with metric 
 * summaries and a multiband focal raster."
 * ---
 */

/* 
 * 1. Setup
 * Initializes the environment and prepares data for analysis.
 * Includes loading point counts, ARU locations, station locations,
 * defining the AOI, loading helper functions, and creating a list
 * of dates for time series data.
 */

/* Import station locations */
var ss_xy = ee.FeatureCollection(
  "projects/ee-bgcasey-piwomodels/assets/ss_xy_ab"
);
// print(ss_xy.limit(5));

/* Define area of interest */
var aoi = ee.FeatureCollection('FAO/GAUL_SIMPLIFIED_500m/2015/level1')
  .filter(ee.Filter.eq('ADM0_NAME', 'Canada')) // Filter for Canada
  .filter(ee.Filter.eq('ADM1_NAME', 'Alberta')) // Filter for Alberta
  .geometry();

// var aoi = ee.FeatureCollection("users/bgcasey/old/studyarea_CL")
//           .geometry();

Map.centerObject(aoi);  

// Convert geometry to a feature for batch.Download
var aoi1 = ee.FeatureCollection(aoi);

/* Load helper functions */
var landsatTimeSeries = require(
  "users/bgcasey/functions:landsat_time_series"
);
var landsatIndicesAndMasks = require(
  "users/bgcasey/functions:landsat_indices_and_masks"
);
var sentinelIndicesAndMasks = require(
  "users/bgcasey/functions:sentinel_indices_and_masks"
);
var utils = require("users/bgcasey/functions:utils");
var sentinelTimeSeries = require(
  "users/bgcasey/functions:sentinel_time_series"
);
var getCanopy = require("users/bgcasey/functions:canopy_height");
var twi = require("users/bgcasey/functions:calculate_TWI");
var geomorpho = require("users/bgcasey/functions:geomorpho90m");

/* Create a date list */
var dateList = utils.createDateList(
  ee.Date('2010-05-01'), ee.Date('2023-05-01'), 1, 'years'
);
// print('Generated list of dates:', dateList);

/* 
 * 2. Get image collections
 * Obtains image collections and calculates spectral indices using
 * helper functions and modules from section 1.
 */
 
/* Landsat time series 
 * A time series of harmonized Landsat 5, 7, and 8 data. 
 */
var ls = landsatTimeSeries.ls_fn(
  dateList, 4, 'months', aoi,
  ['DRS', 'DSWI', 'EVI', 'GNDVI', 'LAI', 'NBR', 'NDVI', 'NDWI'])
  .map(landsatIndicesAndMasks.addNDRS)
  // add prefix to band names
  .map(function(image) {
    // Get the list of band names from the image
    var bandNames = image.bandNames();
    // Map over the band names to append 's2_' prefix
    var newBandNames = bandNames.map(function(bandName) {
      return ee.String('ls_').cat(bandName);
    });
    // Rename the bands in the image
    return image.rename(newBandNames);
  });
// print("ls", ls);

/* Sentinel-2 time series */
var s2 = sentinelTimeSeries.s2_fn(
  dateList, 4, 'months', aoi,
  ['CRE', 'DRS', 'DSWI', 'EVI', 'GNDVI', 'LAI', 'NBR', 
   'NDRE1', 'NDRE2', 'NDRE3', 'NDVI', 'NDWI', 'RDI']);

// Filter and add NDRS
var s2 = utils.filterCollectionByBands(s2, ['DRS'])
  .map(sentinelIndicesAndMasks.addNDRS)
  // add prefix to band names
  .map(function(image) {
    // Get the list of band names from the image
    var bandNames = image.bandNames();
    // Map over the band names to append 's2_' prefix
    var newBandNames = bandNames.map(function(bandName) {
      return ee.String('s2_').cat(bandName);
    });
    // Rename the bands in the image
    return image.rename(newBandNames);
  });
  
// print("s2", s2);

/* Non time-series data */
var elevation = ee.Image('MERIT/DEM/v1_0_3').clip(aoi);
var geomorpho90m = geomorpho.getGeomorpho90m(aoi).select([
  'elev', 'elev-stdev', 'eastness', 'northness', 'cti', 'vrm', 
  'roughness', 'tpi', 'tri'
]);
// print(geomorpho90m, "geo")


var twi = twi.calculateTWI(aoi);
var canopy = getCanopy.get_canopy_data(aoi);

/* 3. Summarize metrics to points 
 * This section summarizes environmental metrics to points. It
 * aggregates metrics over buffer zones around points of interest
 * using Earth Engine's functionality. Metrics include terrain
 * features, canopy cover, and vegetation indices from Landsat and
 * Sentinel-2 imagery. Results are stored as feature collections
 * and exported as CSV files.
 */

// Set global reducer parameters
var xyPoints = ss_xy; // Points of interest
var crs = 'EPSG:3348'; // Coordinate Reference System
var scale = 30; // Spatial resolution in meters
var tileScale = 8; // Level of parallel processing

// Terrain
var terrain = geomorpho90m.addBands(twi); // Combine terrain and TWI
// print(terrain);

var bufferSize = 0; // No buffer, exact point location
var reducer = ee.Reducer.first(); // Use first value encountered
var image = terrain; // Image to summarize
var fileName = "ss_terrain_first_00"; // Output file name

var ssTerrainFirst00 = utils.image_to_points(
  bufferSize, reducer, xyPoints, aoi, image,
  crs, scale, tileScale, fileName
);
// print("ss_terrain_first_00", ssTerrainFirst00);

// Canopy
bufferSize = 500; // Buffer zone radius in meters
reducer = ee.Reducer.mean(); // Calculate mean value within buffer
image = canopy; // Canopy cover image
fileName = "ss_canopy_mean_500"; // Output file name

var ssCanopyMean500 = utils.image_to_points(
  bufferSize, reducer, xyPoints, aoi, image,
  crs, scale, tileScale, fileName
);
// print("ss_canopy_mean_500", ssCanopyMean500);

// Sentinel-2
fileName = "ss_s2_mean_500"; // Output file name for Sentinel-2
var ssS2Mean500 = utils.imageCollectionToPoints(
  bufferSize, reducer, xyPoints, aoi, 
  ee.ImageCollection(s2), crs, scale, tileScale, fileName
);
// print("ssS2Mean500", ssS2Mean500.limit(10));

// Landsat
fileName = "ss_ls_mean_500"; // Output file name for Landsat
var sslsMean500 = utils.imageCollectionToPoints(
  bufferSize, reducer, xyPoints, aoi, 
  ee.ImageCollection(ls), crs, scale, tileScale, fileName
);
// print("sslsMean500", sslsMean500.limit(10));


/* 4. Focal Statistics
 * This section applies focal statistics to a combined multiband image
 * for the year 2023. It first combines Sentinel-2 and Landsat images
 * with additional environmental layers (canopy, TWI, elevation, and
 * geomorpho90m) into a single multiband image. Then, it performs a
 * focal mean reduction using a specified kernel size to smooth the
 * data. Finally, it renames the bands to include the kernel size in
 * their names and exports the resulting image to Google Drive.
 */

// Combine all images into a single multiband image for 2023
var s2_2023 = s2.filter(ee.Filter.eq('year', 2023)).first();
// print(s2_2023);
var ls_2023 = ls.filter(ee.Filter.eq('year', 2023)).first();

// Add bands from other sources to create a combined image
var combinedImage = s2_2023.addBands([
  ls_2023,
  canopy,
  twi,
  elevation,
  geomorpho90m
]);
// print('Combined Multiband Image', combinedImage);

// Define kernel size for focal operation
var kernelSize = 500;

// Apply focal mean reduction with specified kernel size
var focal_image = combinedImage.reduceNeighborhood({
  reducer: ee.Reducer.mean(),
  kernel: ee.Kernel.circle(kernelSize, "meters"),
});

// Get band names from the focal reduced image
var bandNames = focal_image.bandNames();

// Function to append kernel size to band names
var addKernelSize = function(bandName) {
  return ee.String(bandName).cat("_").cat(ee.Number(kernelSize).format());
};

// Rename bands with kernel size appended
var focal_image = focal_image.rename(bandNames.map(addKernelSize));

// // print renamed band names to check
// print(focal_image.bandNames());

// Export the focal mean image to Google Drive
Export.image.toDrive({ 
  image: focal_image.toFloat(),
  description: 'focal_image_500',
  folder: "gee_exports",
  crs: 'EPSG:3348',
  scale: 100,
  region: aoi,
  maxPixels: 6000000000
});