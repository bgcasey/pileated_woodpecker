
# Pileated Woodpecker Nest Detection


[![DOI](https://zenodo.org/badge/648137985.svg)](https://zenodo.org/doi/10.5281/zenodo.11396172) ![In Progress](https://img.shields.io/badge/Status-In%20Progress-yellow)

The _Migratory Bird Convention Act_ (MBCA) and the _Migratory Birds Regulations_, 2022 (MBR 2022) aim to protect and conserve migratory birds, both as populations and as individuals. Recent amendments to the MBR 2022 clarify the year-round protection attributed to certain species, including the Pileated Woodpecker, to limit the destruction of nests that appear unoccupied, but remain valuable ecologically. As such, the MBR 2022 now stipulates that, among others, the nesting cavities of Pileated woodpeckers are protected year-round. Unoccupied nesting cavities must remain protected for a designated wait period of 36 months before removal or destruction is allowed. 

Efficient compliance with the MBR 2022 requires an understanding of where Pileated Woodpecker are located. This project provides a set of tools to help industry identify where Pileated Woodpecker are likely to be and where additional monitoring is and is not needed to find nests.

A [web application](https://ee-bgcasey-piwomodels.projects.earthengine.app/view/pileatedwoodpecker) presents our latest predictive map, areas that we have searched for Pileated Woodpecker nest cavities, and known nest cavity locations. The predictive map will be validated and updated as new data comes in.

![test](1_code/r_scripts/02_clean bird_data.R)

Below is an outline of our methodological workflow. 

1. [Sourcing and processing Pileated Woodpecker data](documentation/piwo_data.md)
   1. [01_fetch_bird_data_from_WildTrax.R](1_code/r_scripts/01_fetch_bird_data_from_WildTrax.R)
   2. [02_clean_bird_data.R](1_code/r_scripts/02_clean_bird_data.R)
   3. [03_create_spatial_object_with_point_count_locations.R](1_code/r_scripts/03_create_spatial_object_with_point_count_locations.R)
   4. [04_qpad_offsets.R](1_code/r_scripts/04_qpad_offsets.R)
2. [Extracting spatial covariates](documentation/spatial_covariates.md)
   1. [extract_spatial_data.js](1_code/GEE/extract_spatial_data.js)
   2. [05_summarize_covariates.R](1_code/r_scripts/05_summarize_covariates.R)
3. [Statistical analyses](documentation/statistical_analyses.md)
   1. [06_boosted_regression_trees.R](1_code/r_scripts/06_boosted_regression_trees.R)
   2. [07_summarize_results.R](1_code/r_scripts/07_summarize_results.R)
   3. [08_spatial_predictions.R](1_code/r_scripts/08_spatial_predictions.R)
4. [Model validation and random stratified sampling](documentation/random_stratified_sampling.md)
   1. [09_random_stratified_sampling.R](1_code/r_scripts/09_random_stratified_sampling.R)
5. [Building a web application using Google Earth Engine](documentation/gee_web_application.md)
   1. [completed_fieldwork.R](1_code/r_scripts/completed_fieldwork.R)
   2. [piwo_webapp.js](1_code/GEE/piwo_webapp.js)


----
**Citation:**

Brendan Casey, Bayne, E., & Baines, S. (2024). Improving Pileated Woodpecker nest cavity detection (v0.10). Zenodo. https://doi.org/10.5281/zenodo.11396173
