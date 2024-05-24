
# ---
# title: ""
# author: ""
# created: ""
# description: ""
# ---

#Setup ----
library(tidyverse)
##Load packages----
library(httr) # for http requests
library(jsonlite)  # if needing json format



cID <- "5249"  # client ID
secret <- "bEqr1LBQWmGFURt42V7Yogi7ZgQnuzjzVGqz4aDq"  # client secret

# The following arguments can be found via the API tab in the 
# eipcollect form dashboard. 
proj.slug <- "bu-piwo-cavity-survey-photos"  # project slug
form.ref <- "916196c9db8345dfad511f18283b9f0b_65f45a58f2e6f"  # form reference
branch.ref <- "916196c9db8345dfad511f18283b9f0b_65f45a58f2e6f_65f45b0ae8946"  # branch reference

res <- POST("https://five.epicollect.net/api/oauth/token",
    body = list(grant_type = "client_credentials",
        client_id = cID, client_secret = secret))
http_status(res)
token <- content(res)$access_token

# url.form<-
# paste('https://five.epicollect.net/api/export/entries/',
# proj.slug, '?map_index=0&form_ref=', form.ref,
# '&format=json', sep= '') ## if using json

url.form<-
  paste('https://five.epicollect.net/api/export/entries/',
        proj.slug, '?map_index=&form_ref=', form.ref,
        '&format=json', sep= '') ## if using json


res1 <- GET(url.form, add_headers(Authorization = paste("Bearer",
    token)))
http_status(res1)
ct1<- fromJSON(rawToChar(content(res1))) ## if
# using json
str(ct1)

url.branch<-
paste('https://five.epicollect.net/api/export/entries/',
proj.slug, '?map_index=0&branch_ref=',
branch.ref, "&format=json",
sep= '') ## if using json; pushing max number
# of records from default 50 to 10^6


res2 <- GET(url.branch, add_headers(Authorization = paste("Bearer",
    token)))
http_status(res2)
ct2<- fromJSON(rawToChar(content(res2))) ## if
# using json
str(ct2)




















## ////////////////////////////////////////////////////////////////
# Get locations from epicollect download----

# Load packages----
library(tidyverse)  # for data manipulation
library(sf)  # for spatial data

# Load data----
# ct1<- read.csv('epicollect_data.csv')
bd <- read_csv("~/Downloads/bu-2024-aru-visit-form-csv/form-1__visit-data.csv", 
                              col_types = cols_only(created_at = col_guess(), 
                                                    `1_FCODE` = col_guess(), `6_WOOD` = col_guess(), 
                                                    `10_Visit_Date` = col_guess(), lat_12_Location_from_Cel = col_guess(), 
                                                    long_12_Location_from_Cel = col_guess(), 
                                                    `13_Latitude_inReach` = col_guess(), 
                                                    `14_Longitude_inReach` = col_guess()))

bd1 <- bd %>%
  dplyr::filter(`1_FCODE` == 'WOOD')

library(readr)

dirPath <- "~/Downloads/bu-piwo-cavity-survey-photos-csv/"

survey_photos <- read_csv(
  paste0(dirPath, "form-1__survey-photos.csv")
)

branch_photos <- read_csv(
  paste0(dirPath, 
         "branch-1__add-a-branch-for-each-location-youre-stopping-at-for-pictures.csv")
)

survey_photos_1<-survey_photos%>%
  select(c(ec5_id=ec5_uuid,
           ss='2_What_is_the_name_o',
           date='3_Date'))%>%
  mutate(ss = toupper(ss))


branch_photos_1<-branch_photos%>%
  select(c(ec5_id=ec5_branch_owner_uuid,
           lat=lat_6_Location_within_th, 
           lon=long_6_Location_within_th))


epi_join <- survey_photos_1 %>%
  left_join(branch_photos_1)


ss_plan_points<-st_read("0_data/external/Simran_Bains/fieldwork_2024/sample_locations_v4/sample_locations_v4.shp")
ss_plan_square<-st_read("0_data/external/Simran_Bains/fieldwork_2024/sample_locations_v4/sample_squares_v4.shp")

survey_photos_1<-survey_photos%>%
  select(ss='2_What_is_the_name_o')%>%
  mutate(ss = toupper(ss))%>%
  distinct()


ss_visited<-ss_plan%>%
  semi_join(survey_photos_1, by=c('Name'='ss'))


ss_plan_points_df<-as.data.frame(ss_plan_points)%>%
  select(-geometry)



ss_plan_square_1<-ss_plan_square%>%
  left_join(ss_plan_points_df)%>%
  semi_join(survey_photos_1, by=c('Name'='ss'))


piwo_searched_square<-st_transform(ss_plan_square_1, crs = 4326)

st_write(piwo_searched_square, 
         "2_pipeline/tmp/piwo_searched_square.shp")
test<-st_read("2_pipeline/tmp/piwo_searched_square.shp")


test<-ss_visited_square%>%
  filter(index==5)

test1<-ss_visited%>%
  filter(index==5)

library(sf)

# Plot the polygon object
plot(test$geometry, col = "lightblue", border = "darkblue")

# Add the points on top
plot(test1$geometry, add = TRUE, col = "red", pch = 1)


st_write("0_data/external/Simran_Bains/fieldwork_2024/sample_locations_v4/sample_squares_v4.geojson"

