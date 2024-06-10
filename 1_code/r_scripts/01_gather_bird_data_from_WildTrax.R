# ---
# title: "Gather Bird Data from WildTrax"
# author: "Brendan Casey"
# created: "2024-01-13"
# description: "This script is designed to fetch bird data from the 
# WildTrax platform. It logs into WildTrax using 
# credentials stored in a user config file. The script proceeds to 
# fetch all data  by sensor type: PC and ARU. For each sensor 
# type. The output of this module is two .rData files containing 
# bird detection data from WildTrax separated by sensor type 
# (PC and ARU). The files are named wildtrax_raw_pc_<date>.rData and 
# wildtrax_raw_aru_<date>.rData, where <date> is the current system 
# date. These files can be loaded into an R environment for 
# further analysis."
# ---

#  Setup ----

## Load packages ----
# If not installed, uncomment the lines below to install
# install.packages("remotes")
# remotes::install_github("ABbiodiversity/wildRtrax")

library(wildRtrax)
library(tidyverse)

## Login to WildTrax ----
# The config file contains the login credentials. 
config <- "1_code/r_scripts/.wtlogin.R"
source(config)
wt_auth()

# ********************************************************************
# Fetch data from WildTrax by sensor type: PC and ARU ---- 

## Point count data ----
# Get the summary of downloads for the point counts. Filter for PC 
# sensor data,download the report for each project, and store in a 
# dataframe.
my_report_pc <- wt_get_download_summary(sensor_id = "PC") %>%
  tibble::as_tibble() %>%
  filter(sensor=="PC") %>%
  dplyr::mutate(data = purrr::map(.x = project_id, 
                                  .f = ~wt_download_report(
                                    project_id = .x, 
                                    sensor_id = "PC", 
                                    weather_cols = F, 
                                    reports = "main"))) %>%
  dplyr::select(c(data)) %>%
  mutate(class=sapply(data, function(df) 
    class(df$individual_count))) %>%
  filter(class!="logical") %>%
  filter(class!="NULL") %>%
  mutate(data = lapply(data, function(df) 
    mutate(df, individual_count = as.character(individual_count)))) %>%
  mutate(data = lapply(data, function(df) 
    mutate(df, survey_id = as.character(survey_id)))) %>%
  dplyr::select(-class) %>%
  unnest(col = data) %>%
  mutate(survey_type="PC")

## ARU data ----
# Get the summary of downloads for the ARUs. Filter for ARU sensor 
# data, download the report for each project, and store in a 
# dataframe.
my_report_aru <- wt_get_download_summary(sensor_id = "PC") %>%
  tibble::as_tibble() %>%
  filter(sensor=="ARU") %>%
  dplyr::mutate(data = purrr::map(.x = project_id, 
                                  .f = ~wt_download_report(
                                    project_id = .x, 
                                    sensor_id = "ARU", 
                                    weather_cols = F, 
                                    reports = "main"))) %>%
  dplyr::select(c(project, data)) %>%
  mutate(class=sapply(data, function(df) 
    class(df$individual_count))) %>%
  filter(class!="logical") %>%
  filter(class!="NULL") %>%
  mutate(data = lapply(data, function(df) 
    mutate(df, individual_count = as.character(individual_count)))) %>%
  dplyr::select(-class) %>%
  unnest(col=data) %>%
  mutate(survey_type="ARU")

# ********************************************************************
# Save ----
wildtrax_raw_pc <- my_report_pc
save(wildtrax_raw_pc, 
     file=paste0("0_data/manual/wildtrax_raw_pc_", 
                 Sys.Date(), ".rData"))

wildtrax_raw_aru <- my_report_aru
save(wildtrax_raw_aru, 
     file=paste0("0_data/manual/wildtrax_raw_aru_", 
                 Sys.Date(), ".rData"))
