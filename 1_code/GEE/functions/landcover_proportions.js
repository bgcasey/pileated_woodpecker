/**** Start of imports. If edited, may not auto-convert in the playground. ****/
var image = ee.Image("projects/ee-bgcasey-piwo/assets/brt_ls_hlc_terrain_canopy_29_2_p_piwo");
/***** End of imports. If edited, may not auto-convert in the playground. *****/


/////////////////////////////////////////////////////////////////////////
// Focal stats of landcover variables: proportion of landcover
/////////////////////////////////////////////////////////////////////////

// Written to work with  'projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2'
exports.lc_focal_ts = function(dates, interval, kernal_size, aoi)  {
  
      var lc_ts = function(d1) {
      var start = ee.Date(d1);
      var end = ee.Date(d1).advance(interval, 'month');
      var date_range = ee.DateRange(start, end);
      var date =ee.Date(d1)
      
      
      // var LC=image_collection
      var LC=ee.ImageCollection('projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2')
        .filterDate(start, end).first().clip(aoi);

      
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
      var LC_proportions = calculateClassProportions(LC.neighborhoodToBands(kernel)).unmask(fillValue).clip(aoi);
      
      var image=LC_proportions
      
      var renameBands = function(image) {
        return ee.Image(classValues.map(function(value, index) {
          return image.select([index]).rename(newClassNames[index]);
        }));
      };
      
      // Rename the bands using the renameBands function
      var LC_proportions_1 = renameBands(image);
      //print(LC_proportions_150)
      
      // Add suffix to bandnames
      // Get the band names of the reduced image
      var bandNames = LC_proportions_1.bandNames();
      
      // Define a function to add the kernel size to band names
      var addKernelSize = function(bandName) {
        return ee.String(bandName).cat("_").cat(ee.Number(radiusInMeters).format());
      };
      
      // Rename the bands with the kernel size appended
      var LC_proportions_1 = LC_proportions_1.rename(bandNames.map(addKernelSize))

      return (LC_proportions_1)
      .set("year", date.get('year'));
  };

  // Function to mask each image based on the presense of Land Cover pixels
  var LC2=ee.ImageCollection('projects/sat-io/open-datasets/CA_FOREST_LC_VLCE2').first()
  var maskImages = function(image) {
    // Create a mask by checking for non-null pixels in the Land Cover image
    var mask = LC2.mask();
    
    // Apply the mask to the current image
    var maskedImage = image.updateMask(mask);
    
    // Return the masked image
    return maskedImage;
  };

  var lc_focal_ts=ee.ImageCollection((dates).map(lc_ts))
    .map(function(img){return img.clip(aoi)})
    .map(maskImages)
  return lc_focal_ts;
}

