
// Run this script using the Earth Engine code editor at code.earthengine.google.com

//########################################################################################################
//##### User defined inputs ##### 
//########################################################################################################

// import point count xy locations
var ss_xy = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/Master_data");

// define a buffer size around point locations (for zonal stats)
var buf=600 


// define years and dates to include in image collection
var startYear  = 2019;    
var endYear    = 2020;  
// var startDay   = '06-01'; // what is the beginning of date filter | month-day
// var endDay     = '09-30'; // what is the end of date filter | month-day

// Date range
var Date_Start = ee.Date('2019-01-01');
var Date_End = ee.Date('2019-12-31');
  
 
//set number of months in time interval
var interval=12;

// import shapefile of study area
var study_area = ee.FeatureCollection("users/bgcasey/provincial_boundaries/Alberta_boundary");
// var study_area = ee.FeatureCollection("users/bgcasey/old/studyarea_CL");

//########################################################################################################
//##### Setup ##### 
//########################################################################################################

// for zonal stats create buffer around points
var ss_xy_buff= ss_xy.map(function(pt){
    return pt.buffer(buf);
  });
  
var aoi = study_area.geometry();

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

// Load helper functions
var ts = require("users/bgcasey/PIWO:functions/timeseries_functions");
var indices = require("users/bgcasey/PIWO:functions/indices");


//########################################################################################################
//##### Get spatial variables
//########################################################################################################

////////////////////////////////////////
// Canopy height and standard deviation
////////////////////////////////////////

var canopy_height = ee.Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1')
      .rename("canopy_height")
      .clip(aoi)

var canopy_standard_deviation = ee.Image('users/nlang/ETH_GlobalCanopyHeightSD_2020_10m_v1')
      .rename('canopy_standard_deviation')
      .clip(aoi)


//combine bands into single image
var canopy = canopy_height.addBands([canopy_standard_deviation])
// print("canopy", canopy)

var ev_fixed = canopy.reduceRegions({
  collection: ss_xy_buff,
  reducer: ee.Reducer.mean(),
  scale: 600
});

print("ev_fixed", ev_fixed.limit(10))


////////////////////////////////////////
// Landcover Indices
////////////////////////////////////////

var ev_lc= indices.Landcover_ts(ev_fixed, Date_Start, Date_End)
print("ev_lc", ev_lc.limit(2))

////////////////////////////////////////
// Sentinel spectral indices
////////////////////////////////////////

var S2_collection=ts.S2_DRS_fn(dates, interval, aoi)
    .map(indices.addNDRS)
    .map(indices.createBinaryMask);
print("S2_collection", S2_collection.limit(1))

/// extract to points
var ev_all = S2_collection.map(function(img) {
  return img.reduceRegions({
    collection: ev_lc,
    crs:'EPSG:3348',
    reducer: ee.Reducer.mean(), // set the names of output properties to the corresponding band names
    scale: 600,
    tileScale: 4
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(img); //to get year and month properties from the stack
  });
}).flatten(); //  Flattens collections
 
print("ev.all", ev_all.limit(10))


// //########################################################################################################
// // // ### Export data to a csv ###
// //########################################################################################################

// Exclude specific fields
// Define the selector to exclude
var excludedSelectors = ['month', 'system:time_start', 'count', 'date', 'histogram', 'year'];

// Get all selectors from the table
var allSelectors = ev_all.first().propertyNames();
// print(allSelectors, "allselectors")

// Filter out the excluded selectors
var includedSelectors = allSelectors.filter(ee.Filter.inList('item', excludedSelectors).not());

// Create a new feature collection with the desired selectors
var filteredTable = ev_all.select(includedSelectors);

//Export table to drive
Export.table.toDrive({
  folder: 'PIWO_GEE',
  collection: filteredTable,
  description:'ss_all_indices',
  fileFormat: 'csv',
  // selectors: allSelectors
});


// //########################################################################################################
// //##### Visualize ##### 
// //########################################################################################################

/////////////////////////////////////////////////
// Add layers to map
/////////////////////////////////////////////////

var LC = ee.ImageCollection('projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2')
var Forests=LC.first().eq(210).or(LC.first().eq(220).or(LC.first().eq(230)))
Map.addLayer(Forests.clip(aoi), {palette:['brown', 'green']}, "Forests")


// var abmiLiDAR = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/LiDAR_Imagery_External_24Apr23");
// Map.addLayer(abmiLiDAR, {}, "abmiLiDAR");

  
var DRS_Image = ee.Image(S2_collection.first().select('DRS')); 
  Map.addLayer(
  DRS_Image, 
  {min: 0, max: 0.00004, palette: ['green', 'yellow', 'red']},
  'DRS')

var NDVI_Image = ee.Image(S2_collection.first().select('NDVI')); 
  Map.addLayer(
  NDVI_Image, 
  {min: 0, max: 1, palette: ['white', 'green']},
  'NDVI')


var NDRS_Image = ee.Image(S2_collection.first().select('NDRS')); 
  Map.addLayer(
  NDRS_Image, 
  {min: 0, max: 0.4, palette: ['green', 'yellow', 'red']},
  'NDRS')

var stressed_image = ee.Image(S2_collection.first().select('NDRS_stressed')); 
  Map.addLayer(
  stressed_image, 
  {palette: ['white', 'red'], opacity: 0.8},
  'NDRS_stressed')

Map.addLayer(ss_xy, {}, "ss_xy");

/////////////////////////////////////////////////
// Histograms
/////////////////////////////////////////////////


var chart=ui.Chart.image.histogram({image:S2_collection.select(['DRS']).first(), scale: 1000})
        // .setSeriesNames(['mean_temp_offset'])
        .setOptions({
          title: 'DRS Histogram',
          hAxis: {
            title: 'DRS',
            titleTextStyle: {italic: false, bold: true},
          },
          // vAxis:
          //     {title: 'count', titleTextStyle: {italic: false, bold: true}},
          colors: ['2D333C'],
          legend: {position: 'none'},
          titlePosition: 'none'
        });
print(chart);



var chart=ui.Chart.image.histogram({image:S2_collection.select(['NDVI']).first(), scale: 1000})
        // .setSeriesNames(['mean_temp_offset'])
        .setOptions({
          title: 'NDVI Histogram',
          hAxis: {
            title: 'NDVI',
            titleTextStyle: {italic: false, bold: true},
          },
          // vAxis:
          //     {title: 'count', titleTextStyle: {italic: false, bold: true}},
          colors: ['2D333C'],
          legend: {position: 'none'},
          titlePosition: 'none'
        });
print(chart);



var chart=ui.Chart.image.histogram({image:S2_collection.select(['NDRS']).first(), scale: 1000})
        // .setSeriesNames(['mean_temp_offset'])
        .setOptions({
          title: 'NDRS Histogram',
          hAxis: {
            title: 'NDRS',
            titleTextStyle: {italic: false, bold: true},
          },
          // vAxis:
          //     {title: 'count', titleTextStyle: {italic: false, bold: true}},
          colors: ['2D333C'],
          legend: {position: 'none'},
          titlePosition: 'none'
        });
print(chart);


