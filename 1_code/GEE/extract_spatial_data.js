/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var geometry = /* color: #d63000 */ee.Geometry.MultiPoint();
/***** End of imports. If edited, may not auto-convert in the playground. *****/
//########################################################################################################
//##### User defined inputs ##### 
//########################################################################################################
   
// import station locations
// var ss_xy = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/ss_xy_az_1");
var ss_xy = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/ss_xy_ab");

// define a buffer size around point locations (for zonal stats)
var buf=500


// 4 seasons (start date should correspond to the first day of a season)
// Date range
var Date_Start = ee.Date('1994-12-01');
var Date_End = ee.Date('2021-11-30');
 
 
 //set number of months in time interval
var interval=3;

// import shapefile of study area
// var study_area = ee.FeatureCollection("projects/ee-bgcasey/assets/calling_lake_study_area");
var study_area = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/Alberta_boundary");


//########################################################################################################
//##### Helper functions ##### 
//########################################################################################################

var ts = require("users/bgcasey/functions:timeseries_functions");
var tpi = require("users/bgcasey/functions:TPI");
var hli = require('users/bgcasey/functions:HLI');
var sentinel_indices = require("users/bgcasey/functions:sentinel_indices");
var landsat=require("users/bgcasey/functions:landsat_functions");

//########################################################################################################
//##### Setup ##### 
//########################################################################################################

// for zonal stats create buffer around points
var ss_xy_buff= ss_xy.map(function(pt){
    return pt.buffer(buf);
  });

var aoi = study_area.geometry().bounds().buffer(10000).bounds();
// var aoi = study_area.geometry()

// convert the geometry to a feature to get the batch.Download.ImageCollection.toDrive function to work
var aoi1=ee.FeatureCollection(aoi)

// Create list of dates for time series. It start at the first of each month in the date range and progress by num_months_in_interval
var n_months = Date_End.difference(Date_Start,'month').round();
var dates = ee.List.sequence(0, n_months, interval);
var make_datelist = function(n) {
  return Date_Start.advance(n,'month');
};
dates = dates.map(make_datelist);

print('list of dates for time series', dates)

//########################################################################################################
//##### Get image collections
//########################################################################################################

// ////////////////////////////////////////
// // Landsat Indices
// ////////////////////////////////////////
var leo7=ts.leo7_fn(dates, interval, aoi)
    .map(landsat.addNDRS)
    .map(landsat.createBinaryMask);
print("leo7", leo7)

// // var leo7_snow=ts.leo7_snow_fn(dates, interval, aoi);
// // // print("leo7_snow", leo7_snow)

////////////////////////////////////////
// Terrain standard
////////////////////////////////////////

var dem = ee.ImageCollection('NRCan/CDEM')
  .mosaic()//combine the tiled image collection into a single image
  .clip(aoi)
  .setDefaultProjection('EPSG:3348')

//print("dem", dem)

// Slope. Units are degrees, range is [0,90).
var slope = ee.Terrain.slope(dem);

// Aspect. Units are degrees where 0=N, 90=E, 180=S, 270=W.
var aspect = ee.Terrain.aspect(dem);

// calcuate northness variable. Convert aspect degrees to radians and take the cosine. 
var northness = aspect.multiply(Math.PI).divide(180).cos().rename('northness');

var terrain = dem.addBands(slope)
                  .addBands(northness)
                  .addBands(aspect)

////////////////////////////////////////
// TPI
////////////////////////////////////////

// Set TPI window parameters. These have a significant impact on output results.
var radius = 5000;
var shape = "circle";
var units = "meters";

// Calculate a TPI image
var tpi_5000 = tpi.tpi(dem, radius, shape, units).rename("tpi_5000").clip(aoi);
//prin("tpi_5000", tpi_5000)

// Set TPI window parameters. These have a significant impact on output results.
var radius = 1000;
var shape = "circle";
var units = "meters";

// Calculate a TPI image
var tpi_1000 = tpi.tpi(dem, radius, shape, units).rename("tpi_1000").clip(aoi);
//prin("tpi_1000", tpi_1000)

// Set TPI window parameters.
var radius = 500;
var shape = "circle";
var units = "meters";

// Calculate a TPI image
var tpi_500 = tpi.tpi(dem, radius, shape, units).rename("tpi_500").clip(aoi);
//print("tpi_500", tpi_500)

// Set TPI window parameters.
var radius = 300;
var shape = "circle";
var units = "meters";

// Calculate a TPI image
var tpi_300 = tpi.tpi(dem, radius, shape, units).rename("tpi_300").clip(aoi);
//print("tpi_300", tpi_300)


// Set TPI window parameters.
var radius = 150;
var shape = "circle";
var units = "meters";

// Calculate a TPI image
var tpi_150 = tpi.tpi(dem, radius, shape, units).rename("tpi_150").clip(aoi);
//print("tpi150", tpi_150)




////////////////////////////////////////
// Canopy height
////////////////////////////////////////

var canopy_height = ee.Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1')
      .rename("canopy_height")
      .clip(aoi)
      //.setDefaultProjection(L7proj);
//prin('canopy_height metadata:', canopy_height);

var canopy_standard_deviation = ee.Image('users/nlang/ETH_GlobalCanopyHeightSD_2020_10m_v1')
      .rename('canopy_standard_deviation')
      .clip(aoi)
      //.setDefaultProjection(L7proj);
//prin('standard_deviation metadata:', canopy_standard_deviation);

//combine bands into single image
var canopy = canopy_height.addBands([canopy_standard_deviation])
//prin("canopy", canopy)


////////////////////////////////////////
// TWI
////////////////////////////////////////

// # Calculate Topographic wetness index and extract points
var upslopeArea = (ee.Image("MERIT/Hydro/v1_0_1")
    .clip(aoi)
    //.setDefaultProjection(L7proj)
    .select('upa')); //flow accumulation area

var elv = (ee.Image("MERIT/Hydro/v1_0_1")
    .clip(aoi)
    //.setDefaultProjection(L7proj)
    .select('elv'));

// TWI equation is ln(α/tanβ)) where α=cumulative upslope drainage area and β is slope 
var slope = ee.Terrain.slope(elv)
var upslopeArea = upslopeArea.multiply(1000000).rename('UpslopeArea') //multiply to conver km^2 to m^2
var slopeRad = slope.divide(180).multiply(Math.PI).rename('slopeRad') //convert degrees to radians
var TWI = (upslopeArea.divide(slopeRad.tan())).log().rename('TWI')
//prin("TWI", TWI)

////////////////////////////////////////
// Heat load index
////////////////////////////////////////

var HLI = hli.hli(dem);
//print("HLI", HLI)

var CHILI = ee.Image('CSP/ERGo/1_0/Global/ALOS_CHILI').rename("CHILI")
  .clip(aoi)
//print("CHILI", CHILI)


////////////////////////////////////////
// TERRA climate variables
////////////////////////////////////////

var terra=ts.terra_fn(dates, interval, aoi);
//print("terra", terra)

////////////////////////////////////////
// NOAA climate variables
////////////////////////////////////////

var noaa=ts.noaa_fn(dates, interval, aoi);
print("noaa", noaa)

// ////////////////////////////////////////
// // Sentinel spectral indices
// ////////////////////////////////////////

// var S2_collection=ts.S2_DRS_fn(dates, interval, aoi)
//     .map(sentinel_indices.addNDRS)
//     .map(sentinel_indices.createBinaryMask);
// print("S2_collection", S2_collection)


//########################################################################################################
// // ### Merge image collections ###
//########################################################################################################

// image collection of fixed variables
var all_fixed = terrain.addBands(CHILI)
              .addBands(TWI)
              .addBands(HLI)
              .addBands(tpi_150)
              .addBands(tpi_300)
              .addBands(tpi_500)
              .addBands(tpi_1000)
              .addBands(tpi_5000)
              .addBands(canopy)
              // .addBands(lidar_abmi)
              ;
//print("all_fixed", all_fixed)


// // image collection of time series variables
var all_ts = noaa.combine(terra)
              .combine(leo7)
              // .combine(leo7_snow)
              // .combine(S2_collection)
              .filter(ee.Filter.eq('month', 6))//filter to the summer season
              ;
//print("all_ts", all_ts.limit(2))

//###############################################
// // Focal statistics via reduceNeighborhood ###
//###############################################


//###############################################
////////////////////////////////////////
// Fixed variables
////////////////////////////////////////

////////////////////////////////////////
// Kernal 150
////////////////////////////////////////
// Define the kernel size
var kernelSize = 150;

var all_fixed_150= all_fixed.reduceNeighborhood({
    reducer: ee.Reducer.mean(), // set the names of output properties to the corresponding band names
    kernel: ee.Kernel.circle(kernelSize, "meters"),
    })

// Get the band names of the reduced image
var bandNames = all_fixed_150.bandNames();

// Define a function to add the kernel size to band names
var addKernelSize = function(bandName) {
  return ee.String(bandName).cat("_").cat(ee.Number(kernelSize).format());
};

// Rename the bands with the kernel size appended
var all_fixed_150 = all_fixed_150.rename(bandNames.map(addKernelSize));

// Print the band names of the renamed image
print(all_fixed_150.bandNames());


////////////////////////////////////////
// Kernal 565   
////////////////////////////////////////
// Define the kernel size
var kernelSize = 565;

var all_fixed_565= all_fixed.reduceNeighborhood({
    reducer: ee.Reducer.mean(), // set the names of output properties to the corresponding band names
    kernel: ee.Kernel.circle(kernelSize, "meters"),
    })

// Get the band names of the reduced image
var bandNames = all_fixed_565.bandNames();

// Define a function to add the kernel size to band names
var addKernelSize = function(bandName) {
  return ee.String(bandName).cat("_").cat(ee.Number(kernelSize).format());
};

// Rename the bands with the kernel size appended
var all_fixed_565 = all_fixed_565.rename(bandNames.map(addKernelSize));

// Print the band names of the renamed image
//print(all_fixed_565.bandNames());


////////////////////////////////////////
// Kernal 1000
////////////////////////////////////////
// Define the kernel size
var kernelSize = 1000;

var all_fixed_1000= all_fixed.reduceNeighborhood({
    reducer: ee.Reducer.mean(), // set the names of output properties to the corresponding band names
    kernel: ee.Kernel.circle(kernelSize, "meters"),
    })

// Get the band names of the reduced image
var bandNames = all_fixed_1000.bandNames();

// Define a function to add the kernel size to band names
var addKernelSize = function(bandName) {
  return ee.String(bandName).cat("_").cat(ee.Number(kernelSize).format());
};

// Rename the bands with the kernel size appended
var all_fixed_1000 = all_fixed_1000.rename(bandNames.map(addKernelSize));

// Print the band names of the renamed image
//print(all_fixed_1000.bandNames());





////////////////////////////////////////
// Combine all neighborhood rasters into a single image
////////////////////////////////////////

// Use addBands to add the bands from the source image to the destination image.
var all_fixed_focalAll = all_fixed_150.addBands(all_fixed_565).addBands(all_fixed_1000);

// Load your image with bands of different resolutions.
var image = all_fixed_focalAll;

// Specify the target scale (resolution) in meters (e.g., 100 meters).
var targetScale = 150;

// Define a function to resample a band to the target scale.
var resampleToTargetScale = function (bandName) {
  var band = image.select([bandName]);
  return band.reproject({
    crs: band.projection(),
    scale: targetScale
  }).set('system:band_name', bandName); // Preserve band name.
};

// Get the band names from the image.
var bandNames = image.bandNames();

// Initialize an image with the first resampled band.
var resampledImage = ee.Image(resampleToTargetScale(bandNames.get(0)));

// Iterate through the band names and add the resampled bands to the image.
for (var i = 1; i < bandNames.size().getInfo(); i++) {
  var bandName = bandNames.get(i);
  var resampledBand = resampleToTargetScale(bandName);
  resampledImage = resampledImage.addBands(resampledBand);
}

var all_fixed_focalAll_resampled=resampledImage;
//print(all_fixed_focalAll_resampled, "all_fixed_focalAll_resampled")

//###############################################
////////////////////////////////////////
// Time series variables
////////////////////////////////////////

// Define the kernel size
var kernelSize = 150;

// Define a function to add the kernel size to band names
var addKernelSize = function(bandName) {
  return ee.String(bandName).cat("_").cat(ee.Number(kernelSize).format());
};

// Create a function to process a single image and rename its bands
var processImage = function(image) {
  var all_ts_150 = image.reduceNeighborhood({
    reducer: ee.Reducer.mean(),
    kernel: ee.Kernel.circle(kernelSize, "meters"),
  });
  
  // Get the band names of the reduced image
  var bandNames = all_ts_150.bandNames();
  
  // Rename the bands with the kernel size appended
  all_ts_150 = all_ts_150.rename(bandNames.map(addKernelSize));
  
  // Copy the properties from the original image to the processed image
  all_ts_150 = all_ts_150.copyProperties(image, image.propertyNames());
  
  return all_ts_150;
};

// Apply the processing function to the image collection
var all_ts_150 = all_ts.map(processImage);

// Get the band names of the processed image in the collection (you can choose any image in the collection)
var bandNames = all_ts_150.first().bandNames();

// Print the band names to the console
print("Band Names: ", bandNames);

//print(all_ts_150, "all_ts_150")


////////////////////////////////////////
// Kernal 500
////////////////////////////////////////
// Define the kernel size
var kernelSize = 565;

// Define a function to add the kernel size to band names
var addKernelSize = function(bandName) {
  return ee.String(bandName).cat("_").cat(ee.Number(kernelSize).format());
};

// Create a function to process a single image and rename its bands
var processImage = function(image) {
  var all_ts_565 = image.reduceNeighborhood({
    reducer: ee.Reducer.mean(),
    kernel: ee.Kernel.circle(kernelSize, "meters"),
  });
  
  // Get the band names of the reduced image
  var bandNames = all_ts_565.bandNames();
  
  // Rename the bands with the kernel size appended
  all_ts_565 = all_ts_565.rename(bandNames.map(addKernelSize));
  
  // Copy the properties from the original image to the processed image
  all_ts_565 = all_ts_565.copyProperties(image, image.propertyNames());
  
  return all_ts_565;
};

// Apply the processing function to the image collection
var all_ts_565 = all_ts.map(processImage);

// Get the band names of the processed image in the collection (you can choose any image in the collection)
var bandNames = all_ts_565.first().bandNames();

// Print the band names to the console
print("Band Names: ", bandNames);

//print(all_ts_565, "all_ts_565")



////////////////////////////////////////
// Kernal 1000
////////////////////////////////////////
// Define the kernel size
var kernelSize = 1000;

// Define a function to add the kernel size to band names
var addKernelSize = function(bandName) {
  return ee.String(bandName).cat("_").cat(ee.Number(kernelSize).format());
};

// Create a function to process a single image and rename its bands
var processImage = function(image) {
  var all_ts_1000 = image.reduceNeighborhood({
    reducer: ee.Reducer.mean(),
    kernel: ee.Kernel.circle(kernelSize, "meters"),
  });
  
  // Get the band names of the reduced image
  var bandNames = all_ts_1000.bandNames();
  
  // Rename the bands with the kernel size appended
  all_ts_1000 = all_ts_1000.rename(bandNames.map(addKernelSize));
  
  // Copy the properties from the original image to the processed image
  all_ts_1000 = all_ts_1000.copyProperties(image, image.propertyNames());
  
  return all_ts_1000;
};

// Apply the processing function to the image collection
var all_ts_1000 = all_ts.map(processImage);

// Get the band names of the processed image in the collection (you can choose any image in the collection)
var bandNames = all_ts_1000.first().bandNames();

// Print the band names to the console
print("Band Names: ", bandNames);

//print(all_ts_1000, "all_ts_1000")



////////////////////////////////////////
// Resample
////////////////////////////////////////

// Define a function to resample an image to the target scale.
var resampleImageToTargetScale = function (image) {
  // Reproject the image to the target scale.
  var resampledImage = image
    .reproject({
      crs: image.select(0).projection(),
      scale: targetScale
    });
  return resampledImage;
};

// Specify the target scale (resolution) in meters (e.g., 150 meters).
var targetScale = 150;

/// 565 to 150

// Get the band names from the first image in the collection.
var firstImage = all_ts_565.first();
var bandNames = firstImage.bandNames();

// Map the resampling function over the entire image collection.
var all_ts_565_rs = all_ts_565.map(resampleImageToTargetScale);


// Print the resampled image collection.
//print(all_ts_565_rs, "all_ts_565_rs");


/// 1000 to 150

// Get the band names from the first image in the collection.
var firstImage = all_ts_1000.first();
var bandNames = firstImage.bandNames();

// Map the resampling function over the entire image collection.
var all_ts_1000_rs = all_ts_1000.map(resampleImageToTargetScale);


// // //########################################################################################################
// // // // ### Extract data to points ###
// // //########################################################################################################

////////////////////////////////////////
// Fixed variables
////////////////////////////////////////
var ss_xy_fixed_pointLevel = all_fixed.reduceRegions({
  collection: ss_xy,
  reducer: ee.Reducer.first(),
  crs:'EPSG:3348',
  scale: 24
});
// print(ev_fixed.limit(10), "ev")

// var ss_xy_fixed_count = ss_xy_fixed.size();
//print('ss_xy_fixed count', ss_xy_fixed_count);

// Export data to a csv
Export.table.toDrive({
  folder: 'google_earth_engine_tables',
  collection: ss_xy_fixed_pointLevel,
  description:'ss_xy_fixed_pointLevel',
  fileFormat: 'csv',
     selectors: [ // choose properties to include in export table. Should add the fields represnenting station id
                  'location',
                  'slope',
                  'elevation',
                  'aspect',
                  'northness',
                  'tpi_150',
                  'tpi_300',
                  'tpi_500',
                  'tpi_1000',
                  'tpi_5000',
                  'TWI',
                  'HLI'
                  ] 
});




var ss_xy_fixed = all_fixed_focalAll_resampled.reduceRegions({
  collection: ss_xy,
  reducer: ee.Reducer.first(),
  crs:'EPSG:3348',
  scale: 150
});
// print(ev_fixed.limit(10), "ev")

var ss_xy_fixed_count = ss_xy_fixed.size();
//print('ss_xy_fixed count', ss_xy_fixed_count);

// Export data to a csv
Export.table.toDrive({
  folder: 'google_earth_engine_tables',
  collection: ss_xy_fixed,
  description:'ss_xy_fixed',
  fileFormat: 'csv',
    selectors: [ // choose properties to include in export table. Should add the fields represnenting station id
                  'location',
                  'slope_mean_150',
                  'slope_mean_565',
                  'slope_mean_1000',
                  'elevation_mean_150',
                  'elevation_mean_565',
                  'elevation_mean_1000',
                  'aspect_mean_150',
                  'aspect_mean_565',
                  'aspect_mean_1000',
                  'northness_mean_150',
                  'northness_mean_565',
                  'northness_mean_1000',
                  'tpi_150_mean_150',
                  'tpi_150_mean_565',
                  'tpi_150_mean_1000',
                  'tpi_300_mean_150',
                  'tpi_300_mean_565',
                  'tpi_300_mean_1000',
                  'tpi_500_mean_150',
                  'tpi_500_mean_565',
                  'tpi_500_mean_500',
                  'tpi_1000_mean_150',
                  'tpi_1000_mean_565',
                  'tpi_1000_mean_500',   
                  'tpi_5000_mean_150',
                  'tpi_5000_mean_565',
                  'tpi_5000_mean_500',
                  'TWI_mean_150',
                  'TWI_mean_565',
                  'TWI_mean_500',
                  'HLI_mean_150',
                  'HLI_mean_565',
                  'HLI_mean_500',
                  'canopy_height_mean_150',
                  'canopy_height_mean_565',
                  'canopy_height_mean_500',
                  'canopy_standard_deviation_mean_150',
                  'canopy_standard_deviation_mean_565',
                  'canopy_standard_deviation_mean_500'
                  ] 
});

// ////////////////////////////////////////
// // all_ts_150
// ////////////////////////////////////////

var ss_xy_ts_150 = all_ts_150.map(function(img) {
  return img.reduceRegions({
    collection: ss_xy,
    crs:'EPSG:3348',
    reducer: ee.Reducer.first(), // set the names of output properties to the corresponding band names
    scale: 150, 
    tileScale: 8
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(img); //to get year and month properties from the stack
  });
}).flatten(); //  Flattens collections

var ss_xy_ts_150_count = ss_xy_ts_150.size();
//print('ss_xy_ts_150 count', ss_xy_ts_150_count);
// print(ss_xy_ts_150.limit(2), "ss_xy_ts")


// Export data to a csv
Export.table.toDrive({
  folder: 'google_earth_engine_tables',
  collection: ss_xy_ts_150,
  description:'ss_xy_ts_150',
  fileFormat: 'csv',
    selectors: [ // choose properties to include in export table. Should add the fields represnenting station id
                'location',
                  'year',
                  'NDVI_mean_150',
                  'NDMI_mean_150',
                  'LAI_mean_150',
                  'DSWI_mean_150',   
                  'DRS_mean_150',
                  'NDRS_mean_150',
                  'NDRS_stressed_mean_150',       
                  'pr_mean_150',
                  'tmmn_mean_150',
                  'tmmx_mean_150'
                  ]
});

// ////////////////////////////////////////
// // all_ts_565_rs
// ////////////////////////////////////////

var ss_xy_ts_565 = all_ts_565.map(function(img) {
  return img.reduceRegions({
    collection: ss_xy,
    crs:'EPSG:3348',
    reducer: ee.Reducer.first(), // set the names of output properties to the corresponding band names
    scale: 565, 
    tileScale: 8
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(img); //to get year and month properties from the stack
  });
}).flatten(); //  Flattens collections

var ss_xy_ts_565_count = ss_xy_ts_565.size();
//print('ss_xy_ts_565 count', ss_xy_ts_565_count);
// print("ss_xy_ts_565", ss_xy_ts_565.limit(2))

// Export data to a csv
Export.table.toDrive({
  folder: 'google_earth_engine_tables',
  collection: ss_xy_ts_565,
  description:'ss_xy_ts_565',
  fileFormat: 'csv',
       selectors: [ // choose properties to include in export table. Should add the fields represnenting station id
                  'location',
                  'year',
                  'NDVI_mean_565',
                  'NDMI_mean_565',
                  'LAI_mean_565',
                  'DSWI_mean_565',   
                  'DRS_mean_565',
                  'NDRS_mean_565',
                  'NDRS_stressed_mean_565',       
                  'pr_mean_565',
                  'tmmn_mean_565',
                  'tmmx_mean_565'
                  ]
});


var ss_xy_ts_1000 = all_ts_1000.map(function(img) {
  return img.reduceRegions({
    collection: ss_xy,
    crs:'EPSG:3348',
    reducer: ee.Reducer.first(), // set the names of output properties to the corresponding band names
    scale: 1000, 
    tileScale: 8
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(img); //to get year and month properties from the stack
  });
}).flatten(); //  Flattens collections

// var ss_xy_ts_1000_count = ss_xy_ts_1000.size();
//print('ss_xy_ts_1000 count', ss_xy_ts_1000_count);
// print("ss_xy_ts_1000", ss_xy_ts_1000.limit(2))


// Export data to a csv
Export.table.toDrive({
  folder: 'google_earth_engine_tables',
  collection: ss_xy_ts_1000,
  description:'ss_xy_ts_1000',
  fileFormat: 'csv',
  selectors: [ // choose properties to include in export table. Should add the fields represnenting station id
                  'location',
                  'year',
                  'NDVI_mean_1000',
                  'NDMI_mean_1000',
                  'LAI_mean_1000',
                  'DSWI_mean_1000',   
                  'DRS_mean_1000',
                  'NDRS_mean_1000',
                  'NDRS_stressed_mean_1000',       
                  'pr_mean_1000',
                  'tmmn_mean_1000',
                  'tmmx_mean_1000'
                  ]
});

// // //////////////////////////////////////////

// // // // //########################################################################################################
// // // // // // ### Download rasters ###
// // // // //########################################################################################################

//Get a single year image from the time series image collection
// Filter the collection to get the image corresponding to the year 2020

var all_ts_1000_rs_2019 = all_ts_1000_rs.filter(ee.Filter.equals('year', 2019)).first();
Export.image.toDrive({ 
  image: all_ts_1000_rs_2019.toFloat(),
  description: 'all_ts_1000_rs_2019',
  folder: 'piwo_raster_neighborhood',
  crs:'EPSG:3348',
  scale: 150,
  region: aoi,
  maxPixels: 6000000000
});

var all_ts_565_rs_2019 = all_ts_565_rs.filter(ee.Filter.equals('year', 2019)).first();
Export.image.toDrive({ 
  image: all_ts_565_rs_2019.toFloat(),
  description: 'all_ts_565_rs_2019',
  folder: 'piwo_raster_neighborhood',
  crs:'EPSG:3348',
  scale: 150,
  region: aoi,
  maxPixels: 6000000000
});

var all_ts_150_2019 = all_ts_150.filter(ee.Filter.equals('year', 2019)).first();
Export.image.toDrive({ 
  image: all_ts_150_2019.toFloat(),
  description: 'all_ts_150_2019',
  folder: 'piwo_raster_neighborhood',
  crs:'EPSG:3348',
  scale: 150,
  region: aoi,
  maxPixels: 6000000000
});


// // /////////////

Export.image.toDrive({ 
  image: all_fixed_focalAll_resampled.toFloat(),
  description: 'all_fixed_focalAll_resampled',
  folder: 'piwo_raster_neighborhood',
  crs:'EPSG:3348',
  scale: 150,
  region: aoi,
  maxPixels: 6000000000
});

// // /////////////
Export.image.toDrive({ 
  image: terrain.toFloat(),
  description: 'terrain',
  folder: 'piwo_raster_neighborhood',
  crs:'EPSG:3348',
  scale: 150,
  region: aoi,
  maxPixels: 6000000000
});

// // /////////////

var all_fixed_2 = all_fixed.select(['slope',
                  'elevation',
                  'aspect',
                  'northness',
                  'tpi_150',
                  'tpi_300',
                  'tpi_500',
                  'tpi_1000',
                  'tpi_5000',
                  'TWI',
                  'HLI']);

Export.image.toDrive({ 
  image: all_fixed_2.toFloat(),
  description: 'all_fixed_2',
  folder: 'piwo_raster_neighborhood',
  crs:'EPSG:3348',
  scale: 150,
  region: aoi,
  maxPixels: 6000000000
});