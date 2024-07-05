/*
 * ---
 * title: "Pileated Woodpecker Project GEE App"
 * author: "Brendan Casey"
 * created: "2024-05-26"
 * description: "This Google Earth Engine (GEE) application 
 * visualizes the field work done and products generated for 
 * our Pileated Woodpecker project. The application includes 
 * various layers such as the locations of known PIWO cavities, 
 * areas searched for cavities, planned pipeline routes, and 
 * the latest PIWO predictive map. It also offers features to 
 * toggle the visibility of these layers and download shapefiles 
 * for further analysis."
 * ---
 */

/**
 * SECTION 1: Importing Assets
 * This section imports geospatial assets such as the study 
 * area, xy locations, pipeline route, and a predictive map 
 * of PIWO occupancy. It also creates a mask for the study area.
 */

/*
 * Import the study area and its geometry, xy locations, PIWO 
 * cavities and searched areas, the pipeline route, and the 
 * predictive map of PIWO occupancy
 */
var studyArea = ee.FeatureCollection(
  "projects/ee-bgcasey-piwomodels/assets/Alberta_boundary"),
    ssXy = ee.FeatureCollection(
  "projects/ee-bgcasey-piwomodels/assets/ss_xy_ab"),
    piwoCavities = ee.FeatureCollection(
  "projects/ee-bgcasey-piwomodels/assets/cavities_xy"),
    piwoSearched = ee.FeatureCollection(
  "projects/ee-bgcasey-piwomodels/assets/piwo_searched"),
    pipeline = ee.FeatureCollection(
  "projects/ee-bgcasey-piwomodels/assets/17324_ProjectBoundary_nad83_csrs_z12"),
    piwoOcc = ee.Image(
  "projects/ee-bgcasey-piwomodels/assets/brt_ls_hlc_terrain_canopy_29_2_p_piwo"),
  piwoReclass = ee.Image(
  "projects/ee-bgcasey-piwomodels/assets/p_piwo_reclass");

print(piwoCavities)
var aoi = studyArea.geometry();
// print(uaLogo)

// Create a mask for the study area and apply it to the predictive map
var boundaryMask = ee.Image.constant(0).paint(aoi, 1);
piwoOcc = piwoOcc.updateMask(piwoOcc.mask().multiply(boundaryMask));
piwoReclass = piwoReclass.updateMask(piwoReclass.mask()
                                     .multiply(boundaryMask));

// Clear the data from previous steps
boundaryMask = null;
aoi = null;
studyArea = null


/**
 * SECTION 2: Setting UI Map Layers
 * This section sets up the map layers for the UI. It defines the color 
 * palette for the terrain and the visualization parameters for the 
 * occupancy. It also creates map layers for the pipeline route, PIWO 
 * cavities, and searched areas, and adds them to the map.
 */

// Define the color palettes for rasters
var rTerrain20 = ["00A600", "13AD00", "28B400", "3EBB00", 
  "56C200", "70C900", "8BD000", "#A7D700", "C6DE00", 
  "E6E600", "E7D217", "E8C32E", "E9B846", "EBB25E", 
  "#ECB176", "EDB48E", "EEBCA7", "F0C9C0", "F1DBD9", "F2F2F2"];

var rTerrain04 = ["00A600",  "8BD000",  "EBB25E", "F2F2F2"];


// Define the visualization parameters for the occupancy
var visOccu = {min: 0, max: 1, palette: rTerrain20.reverse()};        

// Create map layers for the occupancy, pipeline route, PIWO cavities, 
// and searched areas
var piwoOccLayer = ui.Map.Layer(piwoOcc, visOccu, 
  "Pileated woodpecker occupancy").setShown(1).setOpacity(.7);        

// Define the visualization parameters for the reclassed map
var visReclass = {min: 0, max: 4, palette: rTerrain04.reverse()};   

var piwoReclassLayer = ui.Map.Layer(piwoReclass, visReclass, 
  "Pileated woodpecker habitat suitability").setShown(1)
  .setOpacity(.7);  

var pipelineLyr = ui.Map.Layer(pipeline.style(
  {color: '05f9e2', fillColor: '#fdbf6f20', width: 1}), {}, 
  "pipeline route").setShown(0);
Map.add(pipelineLyr);

var piwoSearchedLyr = ui.Map.Layer(piwoSearched.style(
  {color: 'd8f400', pointSize: 3}), {}, 
  "Searched areas").setShown(1);
Map.add(piwoSearchedLyr);




// var piwoCavitiesLyr = ui.Map.Layer(piwoCavities.style(
//   {color: 'f50b86', pointSize: 3}), {}, "PIWO Cavities").setShown(1);
// Map.add(piwoCavitiesLyr);

var piwoCavitiesLyr = ui.Map.Layer(piwoCavities.map(function(f) {
    return f.set({
        'style': {
            color: f.get("color"),
            pointSize: f.get("size"),
    }})}).style({styleProperty: "style"})).setShown(1);
Map.add(piwoCavitiesLyr);

/**
 * SECTION 3: Creating Legend
 * This section creates a legend for the map. It includes a color bar 
 * and labels for the probability of PIWO occupancy.
 */

// Occupancy raster
// Create a panel for the legend
// Occupancy raster
// Create a panel for the legend
var legendRaster = ui.Panel({
  style: {
    position: 'bottom-right', 
    padding: '5px 15px', 
    shown: false
  }
});

// Add a title to the legend
var legendTitle = ui.Label({
  value: 'Probability of Pileated Woodpecker occupancy',
  style: {
    fontWeight: 'bold', 
    fontSize: '12px', 
    margin: '0 0 10px 0px'
  }
});
legendRaster.add(legendTitle);

// Create a function to generate color bar parameters
function makeColorBarParams(palette) {
  return {
    bbox: [0, 0, 1, 0.1], 
    dimensions: '100x10', 
    format: 'png',
    min: 0, 
    max: 1, 
    palette: palette
  };
}

// Create the color bar for the legend
var colorBarOccu = ui.Thumbnail({
  image: ee.Image.pixelLonLat().select(0),
  params: makeColorBarParams(visOccu.palette),
  style: {
    stretch: 'horizontal', 
    margin: '0px 8px', 
    maxHeight: '24px'
  }
});
legendRaster.add(colorBarOccu);

// Create labels for the legend
var legendLabelsOccu = ui.Panel({
  widgets: [
    ui.Label(visOccu.min, {margin: '4px 8px'}),
    ui.Label(
      ((visOccu.max - visOccu.min) / 2 + visOccu.min), {
        margin: '4px 8px', 
        textAlign: 'center', 
        stretch: 'horizontal'
      }
    ),
    ui.Label(visOccu.max, {margin: '4px 8px'})
  ],
  layout: ui.Panel.Layout.flow('horizontal')
});
legendRaster.add(legendLabelsOccu);
Map.add(legendRaster);

// Reclassified raster
// Define the values and their corresponding colors
var values = [0, 1, 2, 3];
var colors = ["F2F2F2", "#EBB25E", "#8BD000", "#00A600"];
var labels = [
  "low",
  "moderate", 
  "high", 
  "very high"
];

// Create a legend
var legendReclass = ui.Panel({
  style: {
    position: 'bottom-right', 
    shown: false
  }
});
legendReclass.add(ui.Label(
  'Probability of detecting PIWO vocalization', 
  {fontWeight: 'bold'}
));


// Add one row per value
values.forEach(function(val, index) {
  var colorBox = ui.Label({
    style: {
      backgroundColor: colors[index],
      padding: '10px',
      margin: '0 0 4px 0'
    }
  });
  var description = ui.Label({
    value: labels[index],
    style: {margin: '0 0 4px 6px'}
  });
  legendReclass.add(ui.Panel([colorBox, description], 
    ui.Panel.Layout.Flow('horizontal')));
});

Map.add(legendReclass);

// Woodpecker cavities
// Define the values, colors, and sizes
var diameters = ["2.5 - 5.0", "5.0 - 7.6", "7.6 - 10.2", "> 10.2"];
var colors = ['#d7b5d8', '#df65b0', '#dd1c77', '#980043'];
var sizes = [10, 15, 20, 25]; // Size for legend circles (pixels)

// Create a legend panel
var legendCavities = ui.Panel({
  style: {
    position: 'bottom-right',
    padding: '8px 15px'
  }
});

// Add a title to the legend
var legendTitleCavities = ui.Label({
  value: 'Woodpecker Cavities',
  style: {
    fontWeight: 'bold',
    fontSize: '14px',
    margin: '0 0 4px 0'
  }
});
legendCavities.add(legendTitleCavities);

// Add a subtitle to the legend
var legendSubtitleCavities = ui.Label({
  value: 'Diameter (cm)',
  style: {
    fontWeight: 'normal',
    fontSize: '12px',
    margin: '0 0 10px 0'
  }
});
legendCavities.add(legendSubtitleCavities);


// Function to create a legend item
function createLegendItem(color, size, label) {
  var colorCircle = ui.Label({
    value: '\u25cf',
    style: {
      color: color,
      fontSize: size + 'px',
      margin: '0 6px 0 0',
      padding: '0'
    }
  });

  var description = ui.Label({
    value: label,
    style: {margin: '0 0 4px 0', padding: '0'}
  });
  
  return ui.Panel([colorCircle, description], ui.Panel.Layout.Flow('horizontal'));
}

// Add legend items
for (var i = 0; i < diameters.length; i++) {
  legendCavities.add(createLegendItem(colors[i], sizes[i], diameters[i]));
}

Map.add(legendCavities);





/**
 * SECTION 4: Toggle Map Features
 * This section creates checkboxes to toggle the visibility of different 
 * map features. It includes checkboxes for the pipeline route, PIWO 
 * nest cavities, searched areas, and PIWO map.
 */

/**
 * Creates a styled checkbox for toggling map features.
 *
 * @param {string} label - The label for the checkbox.
 * @param {string} color - The color of the checkbox icon.
 * @param {boolean} shown - The initial visibility of the layer.
 * @param {Object} layer - The map layer associated with the checkbox.
 * @returns {Object} panel - A panel containing the checkbox, icon, and label.
 */
function createStyledCheckbox(label, color, shown, layer) {
  var checkbox;

  // If the layer is the PIWO occurrence layer or the reclassified layer, 
  // create a checkbox that toggles the visibility of the 
  // layer and the corresponding legend.
  if (layer === piwoOccLayer || layer === piwoReclassLayer) {
    checkbox = ui.Checkbox('', shown);
    checkbox.onChange(function(checked) {
      if (checked) {
        Map.layers().insert(0, layer); // Add layer at the bottom
        (layer === piwoOccLayer ? legendRaster : legendReclass)
        .style().set('shown', true);
      } else {
        Map.remove(layer);
        (layer === piwoOccLayer ? legendRaster : legendReclass)
        .style().set('shown', false);
      }
    });
  } else {
    // For other layers, create a checkbox that toggles the visibility
    // of the layer.
    checkbox = ui.Checkbox('', shown);
    checkbox.onChange(function(checked) {
      layer.setShown(checked);
    });
  }

  var icon;

  // Set the icon for the checkbox based on the layer.
  if (layer === piwoCavitiesLyr) {
    icon = ui.Label('\u25cf', {color: '#' + color});
  } else if (layer === pipelineLyr) {
    icon = ui.Label('\u25A1', {color: '#' + color});
  } else if (layer === piwoSearchedLyr) {
    icon = ui.Label('\u25cf', {color: '#' + color});  
  // } else if (layer === pipelineLyr || layer === piwoSearchedLyr) {
  //   icon = ui.Label('\u25A1', {color: '#' + color});  
  } else if (layer === piwoOccLayer) {
    icon = ui.Label('\u25A1', {color: '#' + color});
  } else if (layer === piwoReclassLayer) {
    icon = ui.Label('\u25A1', {color: '#' + color}); 
  } else {
    icon = ui.Label('', {color: '#' + color});
  }

  // Create a label for the checkbox.
  var textLabel = ui.Label(label);

  // Create a panel containing the checkbox, icon, and label.
  var panel = ui.Panel([checkbox, icon, textLabel], 
    ui.Panel.Layout.flow('horizontal'), {margin: '0 0 0 0'});

  return panel;
}

// Create checkboxes for the pipeline route, PIWO nest cavities, 
// searched areas, and PIWO map
var checkboxMaster = ui.Panel({
  layout: ui.Panel.Layout.flow('vertical'),
  style: {width: '100%', padding: '0px', margin: '0px'}
});

checkboxMaster.add(createStyledCheckbox(
  'Pathways Pipeline', '05f9e2', false, pipelineLyr));
checkboxMaster.add(createStyledCheckbox(
  'Woodpecker cavities', 'f50b86', true, 
  piwoCavitiesLyr));
checkboxMaster.add(createStyledCheckbox(
  'Searched areas', 'd8f400', true, piwoSearchedLyr));
checkboxMaster.add(createStyledCheckbox(
  'Pileated woodpecker occupancy', "FFFFFF", false, piwoOccLayer));
checkboxMaster.add(createStyledCheckbox(
  'Probability of detecting PIWO vocalization', "FFFFFF", false, 
  piwoReclassLayer));

/**
 * SECTION 5: Download Buttons
 * This section creates buttons to download the PIWO cavities and 
 * searched areas as shapefiles.
 */

// Create buttons to download the PIWO cavities and searched areas
var downloadCavitiesButton = ui.Button({
  label: 'Download cavity locations',
  onClick: function() {
    var link = piwoCavities.getDownloadURL({
      format: 'SHP',
      filename: 'cavities_locations'
    });
    window.open(link, '_blank');
  }
});

var downloadSearchedButton = ui.Button({
  label: 'Download searched areas',
  onClick: function() {
    var link = piwoSearched.getDownloadURL({
      format: 'SHP',
      filename: 'piwo_searched_areas'
    });
    window.open(link, '_blank');
  }
});

var buttonPanel = ui.Panel({
  widgets: [downloadCavitiesButton, downloadSearchedButton],
  layout: ui.Panel.Layout.flow('horizontal'),
  style: {stretch: 'horizontal'}
});

/**
 * SECTION 6: Main Panel
 * This section sets up the main panel of the UI. It includes an 
 * introduction, a description of the project, links to the GitHub 
 * repository and data submission/request forms, and separators for 
 * visual organization.
 */

// Set up the main panel of the UI
var mainPanel = ui.Panel(
  {style: {width:'30%', padding: '20px 20px '}});
ui.root.insert(0, mainPanel);

// Add an introduction, a description of the project, links to the GitHub 
// repository and data submission/request forms, and separators to the main panel
var title = ui.Label('Pileated Woodpecker Nest Detection', {
  fontWeight: 'bold', 
  fontSize: '24px', 
  margin: '10px 0px 0px 8px'
});

var authors = ui.Label('Dr. Brendan Casey, Simran Baines, Dr. Erin Bayne', {
  fontWeight: 'bold',
  margin: '0px 0px 15px 8px'
});

var description1 = ui.Label(
  'The Migratory Bird Convention Act (MBCA) and the ' +
  'Migratory Birds Regulations, 2022 (MBR 2022) aim to ' +
  'protect and conserve migratory birds, both as populations ' +
  'and as individuals. Recent amendments to the MBR 2022 ' +
  'clarify the year-round protection attributed to certain ' +
  'species, including the Pileated Woodpecker, to limit the ' +
  'destruction of nests that appear unoccupied but remain ' +
  'valuable ecologically. As such, the MBR 2022 now ' +
  'stipulates that, among others, the nesting cavities of ' +
  'Pileated woodpeckers are protected year-round. Unoccupied ' +
  'nesting cavities must remain protected for a designated ' +
  'wait period of 36 months before removal or destruction is ' +
  'allowed.',
  {margin: '0px 30px 15px 8px'}
);

var description2 = ui.Label(
  'Efficient compliance with the MBR 2022 requires an ' +
  'understanding of where Pileated Woodpecker are located. ' +
  'This project provides a set of tools to help users identify ' +
  'where Pileated Woodpecker are likely to be and where ' +
  'additional monitoring is and is not needed to find nests. ' +
  'Here we present our latest predictive map (based on where ' +
  'machine learning models and province-wide remote sensing ' +
  'variables predict you will hear/see a Pileated Woodpecker). ' +
  'Areas that we have searched for nest cavities and known nest ' +
  'cavity locations are also shown. The predictive map will be ' +
  'validated and updated regularly as new data comes in so ' +
  'changes are to be expected. Data and survey protocols are ' +
  'available on request through the links provided below. ' +
  'Please direct all correspondence to Dr. Brendan Casey ' +
  '(bgcasey@ualberta.ca)',  
  {margin: '0px 30px 15px 8px'}
);


var github = ui.Label('https://github.com/bgcasey/pileated_woodpecker', {
  color: '0000EE',
  margin: '0px 0px 15px 8px'
}, 'https://github.com/bgcasey/pileated_woodpecker');

// var doiLink = ui.Label({
//   value: 'DOI: 10.5281/zenodo.11396172',
//   style: {
//     color: '0000EE',
//     margin: '0px 0px 10px 8px'
//   }
// }).setUrl('https://zenodo.org/doi/10.5281/zenodo.11396172');

var submitData = ui.Label('Submit Data', {
  color: '0000EE'
}, 'https://docs.google.com/forms/d/e/1FAIpQLSef4UjSEU1yvH9As-'+
   'J1GekMhWCnC211ky7-OCsjx4IhgwHLQg/viewform?usp=sf_link',
  {margin: '0px 0px 0px 20px'
});


var requestData = ui.Label(
  'Request Data', 
  {
    color: '0000EE'
  }, 
  'https://docs.google.com/forms/d/e/1FAIpQLScHc8rUl5wZnR4b3QPCCkv0cGcFO9wXX-' +
  'bRzneLQSTzGb0mEQ/viewform?usp=sf_link',
    {margin: '0px 0px 15px 20px'
});

var citationHeader = ui.Label('Data citation:', {
  fontWeight: 'bold',
  margin: '0px 0px 0px 8px'
});

var citation = ui.Label(
  'Brendan Casey, Bayne, E., & Baines, S. (2024). ' +
  'Improving Pileated Woodpecker nest cavity detection (v0.10). ' +
  'Zenodo. https://doi.org/10.5281/zenodo.11396172',
  {
    color: '000000',
    margin: '0px 30px 10px 8px'
  }
);


function createSeparator(color) {
  return ui.Panel([
    ui.Label({
      value: '______________________________________________________________________',
      style: {fontWeight: 'bold', color: color, margin: '0px 30px 10px 8px'}
    })
  ]);
}

// // Add a label to the panel
// var label = ui.Label('In Progress', {
//   fontSize: '24px',
//   fontWeight: 'bold',
//   color: 'red'
// });


// Add sections to the main panel
mainPanel
  // .add(label)
  .add(title)
  .add(authors)
  // .add(github)
  // .add(doiLink)
  .add(description1)
  .add(description2)
  .add(createSeparator('2D333C'))
  .add(checkboxMaster)
  .add(createSeparator('FFFFFF'))
  .add(buttonPanel)
  .add(createSeparator('FFFFFF'))
  .add(requestData)
  .add(submitData)
  .add(createSeparator('FFFFFF'))
  .add(citationHeader)
  .add(citation)

/**
 * SECTION 7: Customize Basemap
 * This section creates a custom basemap using 
 * https://snazzymaps.com/style/28780/at, 
 * and centers the map on the study area.
 */
  
var Basemap = [
    {
        "featureType": "administrative",
        "elementType": "all",
        "stylers": [
            {
                "hue": "#3a3935"
            },
            {
                "saturation": 5
            },
            {
                "lightness": -57
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "administrative",
        "elementType": "geometry.fill",
        "stylers": [
            {
                "color": "#d25151"
            }
        ]
    },
    {
        "featureType": "administrative.country",
        "elementType": "geometry.fill",
        "stylers": [
            {
                "color": "#a75858"
            }
        ]
    },
    {
        "featureType": "administrative.province",
        "elementType": "geometry.stroke",
        "stylers": [
            {
                "hue": "#ffffff"
            },
            {
              "weight": 1.5
            },
            {
                "lightness": 100
            },
            {
                "visibility": "on"
            }
        ]
    },
     {
        "featureType": "administrative.province",
        "elementType": "labels",
        "stylers": [
            {
                "hue": "#ffffff"
            },
            {
                "lightness": 100
            },
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "administrative.locality",
        "elementType": "labels",
        "stylers": [
            {
                "hue": "#B2BEB5"
            },
            {
                "lightness": 30
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "administrative.neighborhood",
        "elementType": "all",
        "stylers": [
            {
                "hue": "#ffffff"
            },
            {
                "lightness": 100
            },
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "administrative.land_parcel",
        "elementType": "all",
        "stylers": [
            {
                "hue": "#ffffff"
            },
            {
                "lightness": 50
            },
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "landscape",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#b7caaa"
            },
            {
                "saturation": -14
            },
            {
                "lightness": -18
            },
            {
                "visibility": "on"
            }
        ]
    },
    {
        "featureType": "landscape.man_made",
        "elementType": "all",
        "stylers": [
            {
                "hue": "#cbdac1"
            },
            {
                "saturation": -6
            },
            {
                "lightness": -9
            },
            {
                "visibility": "on"
            }
        ]
    },
    {
        "featureType": "poi",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#c17118"
            },
            {
                "saturation": 61
            },
            {
                "lightness": -45
            },
            {
                "visibility": "on"
            }
        ]
    },
    {
        "featureType": "poi.medical",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#cba923"
            },
            {
                "saturation": 50
            },
            {
                "lightness": -46
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "poi.park",
        "elementType": "all",
        "stylers": [
            {
                "hue": "#8ba975"
            },
            {
                "saturation": -46
            },
            {
                "lightness": -28
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "road",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#8d9b83"
            },
            {
                "saturation": -89
            },
            {
                "lightness": -12
            },
            {
                "visibility": "on"
            }
        ]
    },
    {
        "featureType": "road.highway",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#d4dad0"
            },
            {
                "saturation": -88
            },
            {
                "lightness": 54
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "road.arterial",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#bdc5b6"
            },
            {
                "saturation": -89
            },
            {
                "lightness": -3
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "road.local",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#bdc5b6"
            },
            {
                "saturation": -89
            },
            {
                "lightness": -26
            },
            {
                "visibility": "on"
            }
        ]
    },
    {
        "featureType": "transit",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#a43218"
            },
            {
                "saturation": 74
            },
            {
                "lightness": -51
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "transit.station",
        "elementType": "labels.text.fill",
        "stylers": [
            {
                "color": "#57de54"
            }
        ]
    },
    {
        "featureType": "water",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#165c64"
            },
            {
                "saturation": 34
            },
            {
                "lightness": -69
            },
            {
                "visibility": "on"
            }
        ]
    }
]

// Apply the custom basemap style
Map.setOptions('Basemap', {'Basemap': Basemap});

// Center the map on the study area
Map.setCenter(-112.4652, 55.2103, 7.5);

