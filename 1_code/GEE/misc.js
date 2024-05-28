/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var table = ee.FeatureCollection("users/bgcasey/provincial_boundaries/Alberta_boundary"),
    table2 = ee.FeatureCollection("users/bgcasey/old/studyarea_CL"),
    HFI = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/o18_HarvestAreas_HFI_2019");
/***** End of imports. If edited, may not auto-convert in the playground. *****/



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
var startDay   = '06-01'; // what is the beginning of date filter | month-day
var endDay     = '09-30'; // what is the end of date filter | month-day

// Date range
var Date_Start = ee.Date('2019-01-01');
var Date_End = ee.Date('2022-12-31');
  
 
//set number of months in time interval
var interval=12;

// import shapefile of study area
var study_area = ee.FeatureCollection("users/bgcasey/provincial_boundaries/Alberta_boundary");
// var study_area = ee.FeatureCollection("users/bgcasey/old/studyarea_CL");


// Human footprint layer
var HFI = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/o18_HarvestAreas_HFI_2019");
print(HFI.limit(10), "HFI")

//########################################################################################################
//##### Setup ##### 
//########################################################################################################

// for zonal stats create buffer around points
var ss_xy_buff= ss_xy.map(function(pt){
    return pt.buffer(buf);
  });
print("ss_xy_buff", ss_xy_buff.limit(10))
  
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


//########################################################################################################
//##### Helper functions ##### 
//########################################################################################################

// Get a time series of Landsat images
var ts = require("users/bgcasey/PIWO:functions/timeseries_functions");
var indices = require("users/bgcasey/PIWO:functions/indices");


//########################################################################################################
//##### Get spatial variables
//########################################################################################################


////////////////////////////////////////
// Canopy height
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
  scale: 30
});

print("ev_fixed", ev_fixed.limit(10))

////////////////////////////////////////
// Landcover Indices
////////////////////////////////////////

// var LC = ee.ImageCollection('projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2').
// filterDate(Date_Start, Date_End);

// print("LC", LC)

      
// // choose reducers
// var reducers = ee.Reducer.count().combine({
//   reducer2: ee.Reducer.frequencyHistogram(),
//   sharedInputs: true
// });

// var LC_1 = LC.map(function(img) {
//   return img.reduceRegions({
//     collection: ev_fixed,
//     reducer: reducers, // set the names of output properties to the corresponding band names
//     scale: 30,
//     tileScale: 2
//   }).map(function (feature) {
//             var histogramResults = ee.Dictionary(feature.get('histogram'));
//             var pixel_count= ee.Number(feature.get('count'))
//       return feature.copyProperties(img, ['system:time_start']) //to get year properties from the stack
//             .set(// get proportion of landcover from histogram 
//                 // by dividing histogram pixel values by the total pixel_count.
//                 'Unclassified', ee.Number(histogramResults.get('0', 0)).divide(pixel_count),
//                 'Water', ee.Number(histogramResults.get('20', 0)).divide(pixel_count),
//                 'Snow_Ice', ee.Number(histogramResults.get('31', 0)).divide(pixel_count),
//                 'Rock_Rubble', ee.Number(histogramResults.get('32', 0)).divide(pixel_count),
//                 'Exposed_Barren_land', ee.Number(histogramResults.get('33', 0)).divide(pixel_count),
//                 'Bryoids', ee.Number(histogramResults.get('40', 0)).divide(pixel_count),
//                 'Shrubs', ee.Number(histogramResults.get('50', 0)).divide(pixel_count),
//                 'Wetland', ee.Number(histogramResults.get('80', 0)).divide(pixel_count),
//                 'Wetland-treed', ee.Number(histogramResults.get('81', 0)).divide(pixel_count),
//                 'Herbs', ee.Number(histogramResults.get('100', 0)).divide(pixel_count),
//                 'Coniferous', ee.Number(histogramResults.get('210', 0)).divide(pixel_count),
//                 'Broadleaf', ee.Number(histogramResults.get('220', 0)).divide(pixel_count),
//                 'Mixedwood', ee.Number(histogramResults.get('230', 0)).divide(pixel_count),
//                 'landcover_yr', img.date().format('YYYY'));
//   })
// }).flatten(); //  Flattens collections of collections into a feature collection of those collections

// print("LC1", LC_1.limit(2))
// var LC = ee.ImageCollection('projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2').
// filterDate(Date_Start, Date_End);
// LC.map(LC_fn)

var mappedCollection = ev_fixed.map(function(collection) {
  return indices.LC_fn(collection,  Date_Start, Date_End);
});
print(mappedCollection)



////////////////////////////////////////
// Sentinel spectral indices
////////////////////////////////////////

var S2_collection=ts.S2_DRS_fn(dates, interval, aoi)
                  .map(indices.addNDRS)
                  .map(indices.createBinaryMask)

print(S2_collection.limit(2), "S2_NDRS")



// ########################################################################################################
// ### Save/export landcover data ###
// ########################################################################################################

// Export landcover data to a csv
Export.table.toDrive({
  folder: 'PIWO_GEE',
  collection: LC_1,
  description:'gee_metrics',
  fileFormat: 'csv'
    // selectors: [ // choose properties to include in export table
    //               'SS', 
    //               'srvy_yr',
    //               'landcover_yr',
    //               'count'
    //               ] 
});


// //########################################################################################################
// //##### Visualize ##### 
// //########################################################################################################

/////////////////////////////////////////////////
// Add layers to map
/////////////////////////////////////////////////

var abmiLiDAR = ee.FeatureCollection("projects/ee-bgcasey-piwo/assets/LiDAR_Imagery_External_24Apr23");
Map.addLayer(abmiLiDAR, {}, "abmiLiDAR");

  
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



var chart=ui.Chart.image.histogram({image:S2_NDRS.select(['NDRS']).first(), scale: 1000})
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
