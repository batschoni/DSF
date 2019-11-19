library(tidyverse)
library(sp)
library(raster)
library(rgdal)
rm(list = ls())
data_bnb = read.csv("/Users/kasparlichtsteiner/Downloads/AB_NYC_2019.csv")

Coordinates = tibble(rep(1,nrow(data_bnb)))
Coordinates = mutate(Coordinates, longitude = as.numeric(data_bnb$longitude), latitude = as.numeric(data_bnb$latitude))
Coordinates = select_(Coordinates, -rep(1,nrow(data_bnb)))

s = shapefile("data/geolocation/ZIP_CODE_040114.shp")
pts <- Coordinates
pts <- pts[complete.cases(pts),]
coordinates(pts) <- ~longitude+latitude
proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")
pts <- spTransform(pts, proj4string(s))

# this does the lon/lat to zip mapping
zip_where <- pts %over% s
Coordinates = mutate(Coordinates, ZIP = zip_where$ZIPCODE)
Coordinates = inner_join(Coordinates, data_bnb, by = c("longitude", "latitude"))

numbers = Coordinates %>% 
  group_by(ZIP) %>% 
  summarise(count = n())