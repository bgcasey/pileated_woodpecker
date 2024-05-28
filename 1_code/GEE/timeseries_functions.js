
//########################################################################################################
//##### Helper functions ##### 
//########################################################################################################
  
var masks = require("users/bgcasey/functions:masks");
var s2_indices = require("users/bgcasey/functions:sentinel_indices");

////////////////////////////////
//// Sentinel
////////////////////////////////

exports.S2_DRS_fn = function(dates, interval, aoi){
  
  var s2_ts = function(d1) {
    var start = ee.Date(d1);
    var end = ee.Date(d1).advance(interval, 'month');
    var date_range = ee.DateRange(start, end);
    var date = ee.Date(d1)
    var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
      .filterDate(date_range)
      .filter(ee.Filter.calendarRange(6,9,'month'))
      // Pre-filter to get less cloudy granules.
      .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE',20))
      .filterBounds (aoi)
      .map(masks.maskS2clouds)
      // .map(masks.maskS2vegetation)
      // .map(masks.maskByLandcover)
      // .map(masks.maskByForestAge)
      // .map(masks.dynamicWorld)
      .map(function(i){return i.multiply(0.0001)}) //adjust for scale factor
      .map(s2_indices.addNDVI)
      .map(s2_indices.addNDWI)
      .map(s2_indices.addDSWI)
      .map(s2_indices.addRDI)
      .map(s2_indices.addNDRE3)      
      .map(s2_indices.addDRS)
      // // .map(addNDWI)
      // // .map(S2maskedWater)
      // .map(s2_indices.addNDRS)
      // .map(s2_indices.createBinaryMask)
    return(s2
          .select(['NDVI', 'NDWI', 'DSWI', 'RDI', 'NDRE3', 'DRS'])
          // .select(['NDVI', 'DRS'])
          .median()
          .set("date", date,"month", date.get('month'), "year", date.get('year'))
          // .copyProperties(['system:time_start'])

          )
          ;
    };

  var s2_collection=ee.ImageCollection((dates).map(s2_ts))
    .map(function(img){return img.clip(aoi)});
  return s2_collection;
}

exports.S2_NDRS_fn = function(image){image
      .map(s2_indices.addNDRS)
      .map(s2_indices.createBinaryMask)
    return(image
          .set("date", date,"month", date.get('month'), "year", date.get('year'))
          .select(['NDVI', 'DRS', 'NDRS', 'NDRS_stressed'])
          )
          ;
}



/////////////////////////////////////////////////////////////////////////
//// landsat
/////////////////////////////////////////////////////////////////////////


var landsat = require("users/bgcasey/functions:landsat_functions");

exports.leo7_fn = function(dates, interval, aoi) {
  
  var leo7_ts = function(d1) {
  var start = ee.Date(d1);
  var end = ee.Date(d1).advance(interval, 'month');
  var date_range = ee.DateRange(start, end);
  var date =ee.Date(d1)
  
  var leo5=ee.ImageCollection('LANDSAT/LT05/C02/T1_L2')
    .filterDate(date_range)
    // .filterBounds(aoi)
  var leo7=ee.ImageCollection('LANDSAT/LE07/C02/T1_L2')
    .filterDate(date_range)
    // .filterBounds(aoi)
  var mergedCollection = leo5.merge(leo7)
    .map(landsat.applyScaleFactors)
    .map(landsat.mask_cloud_snow) // apply the cloud mask function
    .map(landsat.addNDVI)  // apply NDVI function
    .map(landsat.addNDMI)  // apply NDMI function
    .map(landsat.addEVI)  // apply NDMI function
    .map(landsat.addSAVI)
    .map(landsat.addBSI)
    .map(landsat.addSI)
    .map(landsat.addLAI)
    .map(landsat.addDSWI)
    .map(landsat.addDRS)
    // .map(function(img){return img.clip(aoi).reproject({crs: 'EPSG:4326', scale:30})})//clip to study area
  return(mergedCollection
        .median()
        .set("date", date,"month", date.get('month'), "year", date.get('year'))
        .select(['NDVI', 'NDMI', 'EVI', 'SAVI', 'BSI', 'SI', 'LAI', 'DSWI', 'DRS'])
        // .select(['NDVI', 'NDMI', 'EVI', 'SAVI', 'BSI', 'SI', 'LAI'])

        )
        ;
  };

  var leo7=ee.ImageCollection((dates).map(leo7_ts))
    .map(function(img){return img.clip(aoi)});
  return leo7;

};

/////////////////////////////////////////////////////////////////////////
//// Snow landsat
/////////////////////////////////////////////////////////////////////////

exports.leo7_snow_fn = function(dates, interval, aoi) {
  
  var leo7_snow_ts = function(d1) {
  var start = ee.Date(d1);
  var end = ee.Date(d1).advance(interval, 'month');
  var date_range = ee.DateRange(start, end);
  var date =ee.Date(d1)
  var leo5=ee.ImageCollection('LANDSAT/LT05/C02/T1_L2')
    .filterDate(date_range)
    // .filterBounds(aoi)
  var leo7=ee.ImageCollection('LANDSAT/LE07/C02/T1_L2')
  var mergedCollection = leo5.merge(leo7)
    .filterDate(date_range)
    // .filterBounds(aoi)
    .map(landsat.applyScaleFactors)
    .map(landsat.mask_cloud) // apply the cloud mask function
    .map(landsat.addNDSI)  // apply NDSI function
    .map(landsat.addSnow)  // apply Snow function
  return(mergedCollection
        .median()
        .set("date", date,"month", date.get('month'), "year", date.get('year'))
        .select(['NDSI', 'snow']))
        ;
  };
  
  var snow=ee.ImageCollection((dates).map(leo7_snow_ts))
    .map(function(img){return img.clip(aoi)});
  return snow;

};


/////////////////////////////////////////////////////////////////////////
//// Terra 
/////////////////////////////////////////////////////////////////////////

exports.terra_fn = function(dates, interval, aoi) {
  
        var terra_ts = function(d1) {
        function applyScaleFactors(image) {
        var bands_1 = image.select('aet',
                                        'def',
                                        'pet',
                                        'soil',
                                        'srad',
                                        'tmmn',
                                        'tmmx').multiply(0.1);
        var bands_01 = image.select('pdsi',
                                    'vpd',
                                    'vs').multiply(0.1);
        var bands_001 = image.select('vap').multiply(0.001);                            
        return image.addBands(bands_1, null, true)
                    .addBands(bands_01, null, true)
                    .addBands(bands_001, null, true);
      }
        var start = ee.Date(d1);
        var end = ee.Date(d1).advance(interval, 'month');
        var date_range = ee.DateRange(start, end);
        var date =ee.Date(d1)
        var terra=ee.ImageCollection('IDAHO_EPSCOR/TERRACLIMATE')
          .filterDate(date_range)
          .map(applyScaleFactors)
        return(terra
              .median()
              .set("date", date,"month", date.get('month'), "year", date.get('year'))
              )
              ;
      };

  var leo7=ee.ImageCollection((dates).map(terra_ts))
    .map(function(img){return img.clip(aoi)});
  return leo7;

};

/////////////////////////////////////////////////////////////////////////
//// NOAA cloud
/////////////////////////////////////////////////////////////////////////

exports.noaa_fn = function(dates, interval, aoi) {
  
        var noaa_ts = function(d1) {
      function applyScaleFactors(image) {
        var bands_1 = image.select('cloud_fraction', 'cloud_probability').multiply(0.00393701).add(0.5);
        return image.addBands(bands_1, null, true);
      }
        var start = ee.Date(d1);
        var end = ee.Date(d1).advance(interval, 'month');
        var date_range = ee.DateRange(start, end);
        var date =ee.Date(d1)
        var noaa=ee.ImageCollection('NOAA/CDR/PATMOSX/V53')
          .select(['cloud_fraction', 'cloud_probability'])
          .filterDate(date_range)
          .map(applyScaleFactors)
        return(noaa
              .median()
              .set("date", date,"month", date.get('month'), "year", date.get('year'))
              )
              ;
      };
      
  var leo7=ee.ImageCollection((dates).map(noaa_ts))
    .map(function(img){return img.clip(aoi)});
  return leo7;

};


/////////////////////////////////////////////////
// Landcover variables
/// From 'projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2'
/////////////////////////////////////////////////////////////////////////

// feature_collection are the features you are extracting landcover data to 
exports.Landcover_ts = function(feature_collection, Date_Start, Date_End) {
      var LC = ee.ImageCollection('projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2').
      filterDate(Date_Start, Date_End);
      
      // choose reducers
      var reducers = ee.Reducer.count().combine({
        reducer2: ee.Reducer.frequencyHistogram(),
        sharedInputs: true
      });
      
      var LC_1 = LC.map(function(img) {
        return img.reduceRegions({
          collection: feature_collection,
          reducer: reducers, // set the names of output properties to the corresponding band names
          scale: 30,
          tileScale: 2
        }).map(function (feature) {
                  var histogramResults = ee.Dictionary(feature.get('histogram'));
                  var pixel_count= ee.Number(feature.get('count'))
            return feature.copyProperties(img, ['system:time_start']) //to get year properties from the stack
                  .set(// get proportion of landcover from histogram 
                      // by dividing histogram pixel values by the total pixel_count.
                      'Unclassified', ee.Number(histogramResults.get('0', 0)).divide(pixel_count),
                      'Water', ee.Number(histogramResults.get('20', 0)).divide(pixel_count),
                      'Snow_Ice', ee.Number(histogramResults.get('31', 0)).divide(pixel_count),
                      'Rock_Rubble', ee.Number(histogramResults.get('32', 0)).divide(pixel_count),
                      'Exposed_Barren_land', ee.Number(histogramResults.get('33', 0)).divide(pixel_count),
                      'Bryoids', ee.Number(histogramResults.get('40', 0)).divide(pixel_count),
                      'Shrubs', ee.Number(histogramResults.get('50', 0)).divide(pixel_count),
                      'Wetland', ee.Number(histogramResults.get('80', 0)).divide(pixel_count),
                      'Wetland-treed', ee.Number(histogramResults.get('81', 0)).divide(pixel_count),
                      'Herbs', ee.Number(histogramResults.get('100', 0)).divide(pixel_count),
                      'Coniferous', ee.Number(histogramResults.get('210', 0)).divide(pixel_count),
                      'Broadleaf', ee.Number(histogramResults.get('220', 0)).divide(pixel_count),
                      'Mixedwood', ee.Number(histogramResults.get('230', 0)).divide(pixel_count),
                      'landcover_yr', img.date().format('YYYY'));
        })
      }).flatten(); //  Flattens collections of collections into a feature collection of those collections
      return LC_1;
}


/////////////////////////////////////////////////
// Landcover variables
/// From 'projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2'
/////////////////////////////////////////////////////////////////////////

// feature_collection are the features you are extracting landcover data to 
exports.Landcover_focal_ts = function(feature_collection, Date_Start, Date_End, aoi, kernal_size) {
      var LC = ee.ImageCollection('projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2').
      filterDate(Date_Start, Date_End).clip(aoi);
      
      // Define the desired kernel radius in meters
      var radiusInMeters = kernal_size; // Adjust the size as needed
      
      // Get the projection of the image
      var projection = LC.projection();


      // Calculate the equivalent radius in pixels based on the specified radius in meters
      var radiusInPixels = ee.Number(radiusInMeters).divide(projection.nominalScale()).round();

      // Create a circular kernel with an approximate radius
      var kernel = ee.Kernel.circle(radiusInPixels, 'pixels');

      // Set the fill value for pixels that do not equal the landcover class being assessed
      var fillValue = 0;

      var classValues = [0,20,31,32,33,40,50,80,81,100,210,220,230]; // Replace with your actual landcover class numbers
      var newClassNames = [
        "Unclassified",
        "Water",
        "Snow/Ice",
        "Rock/Rubble",
        "Exposed/Barren land",
        "Bryoids",
        "Shrubs",
        "Wetland",
        "Wetland-treed",
        "Herbs",
        "Coniferous",
        "Broadleaf",
        "Mixedwood"
        ];
      // Define the suffix you want to add to all band names
        
      // Function to calculate class proportions within the kernel
      var calculateClassProportions = function(image) {
        var proportions = classValues.map(function(classValue) {
          var classCount = image.updateMask(image.eq(classValue)).reduce(ee.Reducer.count());
          var totalCount = image.reduce(ee.Reducer.count());
          var proportion = classCount.divide(totalCount).rename('Proportion_' + classValue);
          return proportion;
        });
        return ee.Image(proportions);
      };
      // Apply the function the landcover data
      var LC_proportions0 = calculateClassProportions(LC.neighborhoodToBands(kernel)).unmask(fillValue).clip(aoi);
      
      var image=LC_proportions
      
      var renameBands = function(image) {
        return ee.Image(classValues.map(function(value, index) {
          return image.select([index]).rename(newClassNames[index]);
        }));
      };
      
      // Rename the bands using the renameBands function
      var LC_proportions_2 = renameBands(image);
      //print(LC_proportions_150)
      
      // Add suffix to bandnames
      // Get the band names of the reduced image
      var bandNames = LC_proportions_2.bandNames();
      
      // Define a function to add the kernel size to band names
      var addKernelSize = function(bandName) {
        return ee.String(bandName).cat("_").cat(ee.Number(radiusInMeters).format());
      };
      
      // Rename the bands with the kernel size appended
      var LC_proportions_3 = LC_proportions_2.rename(bandNames.map(addKernelSize));

      return LC_proportions_3;
}







