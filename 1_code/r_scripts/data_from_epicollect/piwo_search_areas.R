# ---
# title: "piwo search areas"
# author: "Brendan Casey"
# created: "2024-05-23"
# description: "code to extract piwo search areas data"
# ---

# Setup ----

## Load packages---
library(tidyverse) # for data manipulation
library(sf)  # for spatial data
library(tidyverse)  # for data manipulation
library(httr) # for http requests (epicollect)
library(jsonlite)  # if needing json format

## Import data----
### Get planned locations
ss_plan_points<-st_read(paste0("0_data/external/Simran_Bains/",
                               "fieldwork_2024/sample_locations_v4/",
                               "sample_locations_v4.shp"))
ss_plan_square<-st_read(paste0("0_data/external/Simran_Bains/",
                               "fieldwork_2024/sample_locations_v4/",
                               "sample_squares_v4.shp"))

## ////////////////////////////////////////////////////////////////

# Get data from epicollect----
cID <- "5249"  # client ID
secret <- "bEqr1LBQWmGFURt42V7Yogi7ZgQnuzjzVGqz4aDq"  # client secret

# The following arguments can be found via the API tab in the 
# eipcollect form dashboard. 
proj.slug <- "# Get data from epicollect----
cID <- "5249"  # client ID
secret <- "bEqr1LBQWmGFURt42V7Yogi7ZgQnuzjzVGqz4aDq"  # client secret

# The following arguments can be found via the API tab in the 
# eipcollect form dashboard. 
proj.slug <- "bu-piwo-cavity-survey-photos"  # project slug
form.ref <- paste0("916196c9db8345dfad511f18283b9f0b",
                   "_65f45a58f2e6f")  # form reference
branch.ref.1 <- paste0("916196c9db8345dfad511f18283b9f0b_65f45a58f2e6f",
                     "_65f45b0ae8946")  # branch reference"  # project slug
form.ref <- paste0("916196c9db8345dfad511f18283b9f0b",
                   "_65f45a58f2e6f")  # form reference
branch.ref.1 <- paste0("916196c9db8345dfad511f18283b9f0b_65f45a58f2e6f",
                     "_65f45b0ae8946")  # branch reference

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
        '&format=json&per_page=500', sep= '') ## if using json


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
        sep= '') 

res2 <- GET(url.branch, add_headers(Authorization = paste("Bearer",
                                                          token)))
http_status(res2)
ct2<- fromJSON(rawToChar(content(res2))) ## if
# using json
str(ct2)

## ////////////////////////////////////////////////////////////////

# Get search locations ----
## Searched locations
epi_ss<-as.data.frame(ct1$data$entries)%>%
  select(c(ss='2_What_is_the_name_o'))%>%
  mutate(ss = toupper(ss))%>%
  distinct()

ss_plan_points_df<-as.data.frame(ss_plan_points)%>%
  select(-geometry)

piwo_searched<-ss_plan_square%>%
  left_join(ss_plan_points_df)%>%
  semi_join(epi_ss, by=c('Name'='ss'))

# piwo_searched<-st_transform(piwo_searched, crs = 4326)

st_write(piwo_searched, 
         "3_output/shapefiles/piwo_searched.shp")

