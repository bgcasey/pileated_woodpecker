//########################################################################################################
//##### User defined inputs ##### 
//########################################################################################################
   
// import station locations
// var ss_xy = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/ss_xy_az_1");
var ss_xy = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/ss_xy_ab");

// 4 seasons (start date should correspond to the first day of a season)
// Date range
var Date_Start = ee.Date('1995-01-01');
var Date_End = ee.Date('2018-12-31');

 
 //set number of months in time interval
var interval=12;

// import shapefile of study area
// var study_area = ee.FeatureCollection("projects/ee-bgcasey/assets/calling_lake_study_area");
var study_area = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/Alberta_boundary");


//########################################################################################################
//##### Helper functions ##### 
//########################################################################################################

var lc_prop = require("users/bgcasey/functions:landcover_proportions");

//########################################################################################################
//##### Setup ##### 
//########################################################################################################

var aoi = study_area.geometry().bounds().buffer(10000).bounds();
// var aoi = study_area.geometry()

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

var LC=ee.ImageCollection('projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2').first()

var kernal_size=150
var lc_150= lc_prop.lc_focal_ts(dates, interval, kernal_size, aoi);
print("lc_150",lc_150)

var kernal_size=565
var lc_565= lc_prop.lc_focal_ts(dates, interval, kernal_size, aoi);
// print("lc_565",lc_565)

var kernal_size=1000
var lc_1000= lc_prop.lc_focal_ts(dates, interval, kernal_size, aoi);
// print("lc_1000",lc_1000.limit(2))

var palettes = require('users/gena/packages:palettes');
Map.addLayer(lc_150.filter(ee.Filter.equals('year', 2019)).first(), {bands:"Broadleaf_150", min:0, max:1, palette:palettes.cmocean.Speed[7]}, 'DRS');
//########################################################################################################
//##### Extract to points
//########################################################################################################

/////////////////
////Focal 150 m
/////////////////

var ss_xy_lc_150 = lc_150.map(function(img) {
  return img.reduceRegions({
    collection: ss_xy,
    crs:'EPSG:3348',
    reducer: ee.Reducer.first(), // set the names of output properties to the corresponding band names
    scale: 150, 
    tileScale: 4
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(img); //to get year and month properties from the stack
  });
}).flatten(); //  Flattens collections

var ss_xy_lc_150_count = ss_xy_lc_150.size();
//print('ss_xy_lc_150 count', ss_xy_lc_150_count);
// print(ss_xy_lc_150.limit(2), "ss_xy_ts")


// Export data to a csv
Export.table.toDrive({
  folder: 'google_earth_engine_tables',
  collection: ss_xy_lc_150,
  description:'ss_xy_lc_150',
  fileFormat: 'csv',
    selectors: [ // choose properties to include in export table. Should add the fields represnenting station id
                 'location',
                 'year',
                  'Broadleaf_150',
                  'Coniferous_150',
                  'Exposed/Barren land_150',
                  'Herbs_150',
                  'Mixedwood_150',
                  'Shrubs_150',
                  'Water_150',
                  'Wetland-treed_150',
                  'Wetland_150',
                  ]
});

/////////////////
////Focal 575 m
/////////////////

var ss_xy_lc_565 = lc_565.map(function(img) {
  return img.reduceRegions({
    collection: ss_xy,
    crs:'EPSG:3348',
    reducer: ee.Reducer.first(), // set the names of output properties to the corresponding band names
    scale: 565, 
    tileScale: 4
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(img); //to get year and month properties from the stack
  });
}).flatten(); //  Flattens collections

var ss_xy_lc_565_count = ss_xy_lc_565.size();
//print('ss_xy_lc_565 count', ss_xy_lc_565_count);
// print(ss_xy_lc_565.limit(2), "ss_xy_ts")


// Export data to a csv
Export.table.toDrive({
  folder: 'google_earth_engine_tables',
  collection: ss_xy_lc_565,
  description:'ss_xy_lc_565',
  fileFormat: 'csv',
    selectors: [ // choose properties to include in export table. Should add the fields represnenting station id
                 'location',
                 'year',
                  'Broadleaf_565',
                  'Coniferous_565',
                  'Exposed/Barren land_565',
                  'Herbs_565',
                  'Mixedwood_565',
                  'Shrubs_565',
                  'Water_565',
                  'Wetland-treed_565',
                  'Wetland_565',
                  ]
});

/////////////////
////Focal 1000 m
/////////////////

var ss_xy_lc_1000 = lc_1000.map(function(img) {
  return img.reduceRegions({
    collection: ss_xy,
    crs:'EPSG:3348',
    reducer: ee.Reducer.first(), // set the names of output properties to the corresponding band names
    scale: 1000, 
    tileScale: 4
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(img); //to get year and month properties from the stack
  });
}).flatten(); //  Flattens collections

var ss_xy_lc_1000_count = ss_xy_lc_1000.size();
//print('ss_xy_lc_1000 count', ss_xy_lc_1000_count);
// print(ss_xy_lc_1000.limit(2), "ss_xy_ts")


// Export data to a csv
Export.table.toDrive({
  folder: 'google_earth_engine_tables',
  collection: ss_xy_lc_1000,
  description:'ss_xy_lc_1000',
  fileFormat: 'csv',
    selectors: [ // choose properties to include in export table. Should add the fields represnenting station id
                 'location',
                 'year',
                  'Broadleaf_1000',
                  'Coniferous_1000',
                  'Exposed/Barren land_1000',
                  'Herbs_1000',
                  'Mixedwood_1000',
                  'Shrubs_1000',
                  'Water_1000',
                  'Wetland-treed_1000',
                  'Wetland_1000',
                  ]
});



// // //########################################################################################################
// // // // ### Download rasters ###
// // //########################################################################################################


//Get a single year image from the time series image collection
// Filter the collection to get the image corresponding to the year 2020
var lc_150_2019 = lc_150.filter(ee.Filter.equals('year', 2019)).first();

// Print the image to the console
print('lc_150_2019', lc_150_2019);

Export.image.toDrive({ 
  image: lc_150_2019.toFloat(),
  description: 'lc_150_2019',
  folder: 'piwo_raster_neighborhood',
  crs:'EPSG:3348',
  scale: 150,
  region: aoi,
  maxPixels: 6000000000
});



//Get a single year image from the time series image collection
// Filter the collection to get the image corresponding to the year 2020
var lc_565_2019 = lc_565.filter(ee.Filter.equals('year', 2019)).first();

// Print the image to the console
print('lc_565_2019', lc_565_2019);



Export.image.toDrive({ 
  image: lc_565_2019.toFloat(),
  description: 'lc_565_2019',
  folder: 'piwo_raster_neighborhood',
  crs:'EPSG:3348',
  scale: 150,
  region: aoi,
  maxPixels: 6000000000
});


//Get a single year image from the time series image collection
// Filter the collection to get the image corresponding to the year 2020
var lc_1000_2019 = lc_1000.filter(ee.Filter.equals('year', 2019)).first();

// Print the image to the console
print('lc_1000_2019', lc_1000_2019);



Export.image.toDrive({ 
  image: lc_1000_2019.toFloat(),
  description: 'lc_1000_2019',
  folder: 'piwo_raster_neighborhood',
  crs:'EPSG:3348',
  scale: 150,
  region: aoi,
  maxPixels: 6000000000
});







