
# Pileated Woodpecker Nest Detection


[![DOI](https://zenodo.org/badge/648137985.svg)](https://zenodo.org/doi/10.5281/zenodo.11396172) ![In Progress](https://img.shields.io/badge/Status-In%20Progress-yellow)

The _Migratory Bird Convention Act_ (MBCA) and the Migratory Birds Regulations, 2022 (MBR 2022) aim to protect and conserve migratory birds, both as populations and as individuals. Recent amendments to the MBR 2022 clarify the year-round protection attributed to certain species, including the Pileated Woodpecker, to limit the destruction of nests that appear unoccupied but remain valuable ecologically. As such, the MBR 2022 now stipulates that, among others, the nesting cavities of Pileated woodpeckers are protected year-round. Unoccupied nesting cavities must remain protected for a designated wait period of 36 months before removal or destruction is allowed.

Efficient compliance with the MBR 2022 requires an understanding of where Pileated Woodpecker are located. This project provides a set of tools to help users identify where Pileated Woodpecker are likely to be and where additional monitoring is and is not needed to find nests. Here we present our latest [predictive map](https://ee-bgcasey-piwomodels.projects.earthengine.app/view/pileatedwoodpecker) (based on where machine learning models and province-wide remote sensing variables predict you will hear/see a Pileated Woodpecker). Areas that we have searched for nest cavities and known nest cavity locations are also shown. The predictive map will be validated and updated regularly as new data comes in so changes are to be expected. Data and survey protocols are available on request through the links provided below. Please direct all correspondence to Dr. Brendan Casey (bgcasey@ualberta.ca)

Below is an outline of our methodological workflow. 

1. [Sourcing and processing Pileated Woodpecker data](documentation/piwo_data.md)
   1. [fetch_bird_data_from_WildTrax.R](1_code/r_scripts/fetch_bird_data_from_WildTrax.R)
   2. [clean_bird_data.R](1_code/r_scripts/clean_bird_data.R)
   3. [create_spatial_object_with_point_count_locations.R](1_code/r_scripts/create_spatial_object_with_point_count_locations.R)
   4. [qpad_offsets.R](1_code/r_scripts/qpad_offsets.R)
2. [Extracting spatial covariates](documentation/spatial_covariates.md)
   1. [extract_spatial_data.js](1_code/GEE/extract_spatial_data.js)
   2. [mosaic_rasters.R](1_code/r_scripts/mosaic_rasters.R)
   3. [scanfi_data.R](1_code/r_scripts/scanfi_data.R)
   4. [extract_natural_regions_and_subregions.R](1_code/r_scripts/extract_natural_regions_and_subregions.R)
3. [Statistical analyses](documentation/statistical_analyses.md)
   1. [format_data_for_models.R](1_code/r_scripts/format_data_for_models.R)
   2. [boosted_regression_trees.R](1_code/r_scripts/boosted_regression_trees.R)
      1. [tune_gbm_model.R](1_code/r_scripts/functions/tune_gbm_model.R)
      2. [bootstrap_brt.R](1_code/r_scripts/functions/bootstrap_brt.R)
   3. [prepare_prediction_grid.R](1_code/r_scripts/prepare_prediction_grid.R)
   5. [spatial_predictions.R](1_code/r_scripts/spatial_predictions.R)
   6. [summarize_results.R](1_code/r_scripts/summarize_results.R)
4. [Model validation and random stratified sampling](documentation/random_stratified_sampling.md)
   1. [random_stratified_sampling.R](1_code/r_scripts/random_stratified_sampling.R)
5. [Building a web application using Google Earth Engine](documentation/gee_web_application.md)
   1. [completed_fieldwork.R](1_code/r_scripts/completed_fieldwork.R)
   2. [piwo_webapp.js](1_code/GEE/piwo_webapp.js)



----
**Citation:**

Brendan Casey, Bayne, E., & Bains, S. (2024). Improving Pileated Woodpecker nest cavity detection (v0.10). Zenodo. https://doi.org/10.5281/zenodo.11396173
