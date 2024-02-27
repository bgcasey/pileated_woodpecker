
//########################################################################################################
//##### Helper functions ##### 
//########################################################################################################

// Get a time series of Landsat images
var masks = require("users/bgcasey/PIWO:functions/masks");
var indices = require("users/bgcasey/PIWO:functions/indices");


////////////////////////////////
//// Time series functions
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
      .map(indices.addNDVI)
      .map(indices.addNDWI)
      .map(indices.addDSWI)
      .map(indices.addRDI)
      .map(indices.addNDRE3)      
      .map(indices.addDRS)
      // // .map(addNDWI)
      // // .map(S2maskedWater)
      // .map(indices.addNDRS)
      // .map(indices.createBinaryMask)
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
      .map(indices.addNDRS)
      .map(indices.createBinaryMask)
    return(image
          .set("date", date,"month", date.get('month'), "year", date.get('year'))
          .select(['NDVI', 'DRS', 'NDRS', 'NDRS_stressed'])
          )
          ;
}

