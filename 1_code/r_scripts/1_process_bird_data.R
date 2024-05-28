# ---
# title: "Process bird data"
# author: "Brendan Casey"
# created: "September 7, 2023"
# ---

#Notes----
# Process wildtrax bird data and prepare it to generate QPAD offsets.
  

#Setup----
##load packages----
library(dplyr)
library(sf)
library(tmap)
library(tmaptools)
library(OpenStreetMap)

##load bird data----
### from Elly's QPAD work----
load("0_data/external/bird_data/qpadv4_clean.Rdata")

### Austins data----
Master_data <- read_csv("0_data/external/Austin_Zeller/code/PIWO_Bayesian_Model_Comparison-main/Master_data.csv")

##load study area
aoi<-st_read("0_data/external/Alberta/Alberta.shp")

#filter stations to study area----

##create spatial data frame of stations----
### Wildtrax stations----
ss_xy<-visit%>%
  dplyr::select(location, lon, lat)%>%
  distinct()

ss_xy<-st_as_sf(ss_xy, coords=c("lon","lat"), crs=4326)
ss_xy_1<-st_transform(ss_xy, crs=3400)

#### Filter stations to study area polygon----
ss_xy_ab <- st_intersection(ss_xy_1, aoi)
save(ss_xy_ab, file="0_data/manual/response/ss_xy_ab.rData")
st_write(ss_xy_ab, "0_data/manual/response/ss_xy_ab.shp")


### Austin's stations----
ss_xy_az<-Master_data%>%
  dplyr::select(location, longitude, latitude)%>%
  rename(c(lat=latitude, lon=longitude))%>%
  distinct()

ss_xy_az<-st_as_sf(ss_xy, coords=c("lon","lat"), crs=4326)
ss_xy_az_1<-st_transform(ss_xy_az, crs=3400)
save(ss_xy_az_1, file="0_data/manual/response/ss_xy_az_1.rData")
st_write(ss_xy_az_1, "0_data/manual/response/ss_xy_az_1.shp")

### Combine AZ and WildTrax points into single shape file



##Create a map----
### Wildtrax all----
# Create a map of points and study area

#alberta boundary
alberta<-aoi

mainmap<-
  # overlay difference polygon to mask content outside
  tm_shape(aoi)+
  tm_borders(lwd=.9, col="black")+
  tm_shape(ss_xy_ab)+
  tm_symbols(shape = 1, alpha = .3, size = .2, col = "red")+
  tm_scale_bar(position=c("left", "BOTTOM"), text.color = "black", color.light="lightgrey")+
  tm_graticules(lines=FALSE)

tmap_save(mainmap,filename="3_output/maps/studyArea.png",
          dpi=300, 
          height=300, width=160, units="mm")


### Austin Zellers data----
# Create a map of points and study area
mainmap<-
  # overlay difference polygon to mask content outside
  tm_shape(aoi)+
  tm_borders(lwd=.9, col="black")+
  tm_shape(ss_xy_az_1)+
  tm_symbols(shape = 1, alpha = .7, size = .2, col = "red")+
  tm_scale_bar(position=c("left", "BOTTOM"), text.color = "white", color.light="lightgrey")+
  tm_graticules(lines=FALSE)

tmap_save(mainmap,filename="3_output/maps/studyArea_AZ.png",
          dpi=300, 
          height=300, width=160, units="mm")


##Filter bird data based on stations----
bird_ab<-semi_join(bird, ss_xy_ab, by = join_by(location))
visit_ab<-semi_join(visit, ss_xy_ab, by = join_by(location))


#Get max distance and duration from point count methods columns----
visit_ab <- visit_ab %>%
  mutate(distanceMethod= gsub("-ARU", "", distanceMethod))%>%
  mutate(MAXDIS = sapply(strsplit(distanceMethod, "-"), function(x) tail(x, 1)))%>%
  mutate(MAXDIS = gsub("m", "", MAXDIS))%>%
  mutate(MAXDUR = sapply(strsplit(durationMethod, "-"), function(x) tail(x, 1)))%>%
  mutate(MAXDUR = gsub("min", "", MAXDUR))%>%
  mutate(MAXDUR = gsub("\\+", "", MAXDUR))%>%
  mutate(MAXDUR=as.numeric(MAXDUR))%>%
  mutate(MAXDIS=as.numeric(MAXDIS))

#Format bird detection data_frame----
bird_ab<-bird_ab%>%
  select(-c(isSeen, isHeard))%>%
  tidyr::pivot_wider(names_from=species, values_from=abundance, values_fn={sum}, values_fill = 0)%>%
  select("id", "project" ,"sensor","location","buffer","lat","lon","year", 
         "date",  "observer","distanceMethod","durationMethod",  "TM", "distanceBand" ,"durationInterval", "PIWO")

#Save----
save(visit_ab, bird_ab, species,  file="0_data/manual/response/bird_station_data.Rdata")




