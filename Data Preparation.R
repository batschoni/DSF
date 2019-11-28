# Install required packages
# install.packages("ggmap")
# install.packages("raster")
# install.packages("rgdal")

library(tidyverse)
library(ggmap) # to get coordinates from a address
register_google(key = "") # the service is free but requires email registration
<<<<<<< HEAD
library("sp") # Library for spatial data
library("raster") # Library for spatial data
=======
  # Library for spatial data
  library(sp)
library(raster)
>>>>>>> 27bd7cd1fbaba01949d12d54ea3938ae86dc3d86
library(rgdal)


# downlaod and first cleaning of inspection data
#########################################################################################

# Initially download and save the file
#inspect_data <- read.csv("https://data.ny.gov/api/views/d6dy-3h7r/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)
#save(inspect_data, file = "./data/inspect_data_original.RData")

# Load the original file previously downloaded
load("./data/inspect_data_original.RData")

inspect_data <- as_tibble(inspect_data)

# Inspection grade as numbers
mapping <- c("A" = 3, "B" = 2, "C" = 1)
inspect_data <- inspect_data %>%
  mutate(Inspection.Grade = mapping[Inspection.Grade])

rm(mapping)

inspect_data = inspect_data %>% 
  mutate(Inspection.Date = as.Date(Inspection.Date, format = "%m/%d/%Y")) %>% #convert values to dates for calculation
  dplyr::mutate(id = row_number()) %>% #add row number as id
  filter(Trade.Name != "") %>% #drops 7obs with missing trade name
  arrange(Inspection.Date) #sort by date

#if the same shop has several inspections - keep only the newest
inspect_data = inspect_data %>%
  group_by(Trade.Name) %>% 
  summarise_all(funs(first)) %>% #<----- check here Warning message: funs() is soft deprecated as of dplyr 0.8.0
  ungroup

# finds all shop chains
inspect_data_chains = inspect_data %>% 
  dplyr::group_by(Owner.Name) %>%  #group by owner to see who owns more than one company
  dplyr::summarise(count = n())

inspect_data_chains$chain[which(inspect_data_chains$count == 1)] = 0 #gives every owner the value 0 if only one shop owned
inspect_data_chains$chain[which(inspect_data_chains$count >= 2)] = 1 #gives value 1 if > 1 shop owned

#merges the data
inspect_data = inspect_data %>%
  inner_join(inspect_data_chains, by = "Owner.Name")

rm(inspect_data_chains)

#########################################################################################

# Add Coordinates of shops
#########################################################################################

# Function extracts the coordinates as two vectors
coord <- function(string_vector){
  # splits character at "("
  string_vector <- strsplit(string_vector, "\\(")
  # keeps 2nd split
  string_vector <- sapply(string_vector, "[", 2)
  # splits character at ","
  string_vector <- strsplit(string_vector, "\\,")
  # keeps 1st split
  latitude <- sapply(string_vector, "[", 1)
  latitude <- as.numeric(latitude)
  # keeps 2nd element of previous split
  string_vector <- sapply(string_vector, "[", 2)
  # splits string at ")"
  string_vector <- strsplit(string_vector, "\\)")
  # keeps 1st split
  longitude <- sapply(string_vector, "[", 1)
  longitude <- as.numeric(longitude)
  location <- cbind(longitude, latitude)
  return(location)
}

# We replace location by Longitude and Latitude
inspect_data <- inspect_data %>%
  mutate(Longitude = coord(Location)[,1],
         Latitude = coord(Location)[,2]) %>%
  dplyr::select(-Location)

rm(coord)

# 748 coordinates are missing
table(is.na(inspect_data$Longitude))

# create address column
inspect_data <- inspect_data %>%
  mutate(Address = str_c(Street, Zip.Code, sep = ", ")) %>%
  mutate(Address = str_c(Address, City, sep = " ")) %>%
  mutate(Address = str_c(Address, State.Code, sep = ", "))

# use Google maps to get missing coordiantes (takes few minutes)
inspect_data_na <- inspect_data %>%
  filter(is.na(Latitude)) %>%
  mutate_geocode(Address) %>%
  mutate(Latitude = lat, Longitude = lon) %>%
  dplyr::select(-c(lat, lon)) %>%
  filter(!is.na(Latitude)) # 248 still missing and are dropped

# add new coordinates
inspect_data <- inspect_data %>%
  filter(!is.na(Latitude)) %>%
  bind_rows(inspect_data_na)

table(is.na(inspect_data$Longitude)) # no more coordinates with NA

rm(inspect_data_na)

save(inspect_data, file = "./data/inspect_data.RData")

#########################################################################################

# Spatial Data
#########################################################################################

# Haversine Formula
haversine <- function(lat1, lon1, lat2, lon2){
  # to radians
  φ1 <- (lat1 * pi) / (180)
  φ2 <- (lat2 * pi) / (180)
  Δφ <- ((lat2 - lat1) * pi) / (180)
  Δλ <- ((lon2 - lon1) * pi) / (180)
  R = 6371000 # eath's radius in metres
  a = sin(Δφ/2)^2 + cos(φ1) *cos(φ2) *sin(Δλ/2)^2
  c = 2*atan2(sqrt(a), sqrt(1-a))
  d = R*c
  return(d)
}

# gives me the n closest obs. to coordniates in df with rows latitude and longitude
n_closest <- function(df, n, lat, lon){
  dist_vect <- haversine(lat, lon, as.matrix(df$Latitude), as.matrix(df$Longitude))
  dist_sort <- sort(dist_vect, decreasing = FALSE)[1:n]
  rows <- which(dist_vect %in% dist_sort)
  inspections_sub <- df[rows, ]
  return(inspections_sub)
}

# shop density and rating of closest shop
rating_closest_neighb <- c()
shop_density <- c()
for (i in 1:nrow(inspect_data)){
  lat = as.numeric(inspect_data$Latitude[i])
  lon = as.numeric(inspect_data$Longitude[i])
  inspect_data_sub <- inspect_data %>%
    slice(-i)
  # get the density of shops in 1km distance
  distances <- haversine(lat, lon, inspect_data_sub$Latitude, inspect_data_sub$Longitude)
  distances <- length(which(distances < 1000))
  shop_density <- c(shop_density, distances)
  # get the grade of the closest shop
  inspect_data_sub <- n_closest(inspect_data_sub, 1, lat, lon)
  inspect_grade <- inspect_data_sub$Inspection.Grade
  inspect_grade <- round(mean(inspect_grade)) # rounded mean from multiple shops with same closest distance
  rating_closest_neighb <- c(rating_closest_neighb, inspect_grade)
}

# append the df
inspect_data <- inspect_data %>%
  mutate(shop_density = shop_density,
         rating_closest_neighb = rating_closest_neighb)

rm(distances, i, inspect_grade, lat, lon, rating_closest_neighb, shop_density, n_closest, inspect_data_sub)

save(inspect_data, file = "./data/inspect_data.RData")
#########################################################################################

# Add Google Ratings
#########################################################################################
load("data/results_scraping_final")
google_ratings <-  data
google_ratings <- dplyr::select(google_ratings, -X)
#google_ratings = google_ratings[complete.cases(google_ratings[,]),]
#google_ratings$Reviews[which(google_ratings$Reviews!=0)] = 1
inspect_data = inner_join(inspect_data, google_ratings, by = "Trade.Name")

inspect_data <- inspect_data %>%
  dplyr::select(-City.y) %>%
  rename(City = City.x) 

rm(google_ratings, data)

save(inspect_data, file = "./data/inspect_data.RData")
#########################################################################################

# Filter for New York City
#########################################################################################

# only NY City
ny_counties <-  c("New York", "Kings", "Bronx", "Richmond", "Queens")
ny_inspect_data <- inspect_data[which(inspect_data$County %in% ny_counties),]
rm(ny_counties)

save(ny_inspect_data, file = "./data/ny_inspect_data.RData")
#########################################################################################

# Add Demographic Information
#########################################################################################

demographic_data <- read.csv("./data/inspectionsDem.cvs.gz")
demographic_data <- demographic_data %>% distinct(Street, .keep_all = TRUE)
ny_inspect_dem <- merge(ny_inspect_data, demographic_data, by = "Street")
ny_inspect_data <- ny_inspect_dem


#########################################################################################

# Add Airbnb Data
#########################################################################################
data_bnb <- read.csv("data/ab_nyc_19.csv") #reading data

longitude = as.numeric(data_bnb$longitude)
latitude = as.numeric(data_bnb$latitude)
Coordinates  = tibble(longitude, latitude)
s = shapefile("data/geolocation/ZIP_CODE_040114.shp")
pts <- Coordinates
pts <- pts[complete.cases(pts),]
coordinates(pts) <- ~longitude+latitude
proj4string(pts) <- CRS("+proj=longlat +datum=WGS84")
pts <- spTransform(pts, proj4string(s))

# this does the lon/lat to zip mapping
zip_where <- pts %over% s
data_bnb = data_bnb %>% 
  mutate(ZIP = zip_where$ZIPCODE) %>% 
  inner_join(Coordinates, data_bnb, by = c("longitude", "latitude")) %>% 
  dplyr::select(ZIP, price, neighbourhood_group)

summary = data_bnb %>% 
  group_by(ZIP) %>% 
  summarise(count = n(), mean = mean(price))

data_bnb = inner_join(data_bnb, summary, by = "ZIP")
data_bnb$ZIP = as.numeric(data_bnb$ZIP)

data_bnb = data_bnb %>% 
  distinct(ZIP, .keep_all = TRUE) %>% 
  dplyr::select(-price) %>% 
  rename(Zip.Code = ZIP, Numb_Rooms = count, Avr_Price = mean)

ny_inspect_data = inner_join(ny_inspect_data, data_bnb, by = "Zip.Code")

rm(Coordinates, data_bnb, pts, s, summary, zip_where, latitude, longitude)

save(ny_inspect_data, file = "./data/ny_inspect_data.RData")
#########################################################################################

# NYC Subway locations
#########################################################################################

# Initially download and save the file
#subway_data <- read.csv("https://data.ny.gov/api/views/i9wp-a4ja/rows.csv?accessType=DOWNLOAD&sorting=true", stringsAsFactors = FALSE)
#save(subway_data, file = "./data/subway_data.RData")

# Load the saved data
load("./data/subway_data.RData")

subway_data <- as_tibble(subway_data)

# Keep only unique stations
subway_data <- subway_data %>%
  distinct(Station.Name, .keep_all = TRUE) %>%
  rename(Latitude = Station.Latitude,
         Longitude = Station.Longitude)

# Distances in meter to next subway station (ONLY NYC)
subway_distance <- c()
for (i in 1:nrow(ny_inspect_data)){
  lat <-  as.numeric(ny_inspect_data$Latitude[i])
  lon <-  as.numeric(ny_inspect_data$Longitude[i])
  distances <- haversine(lat, lon, subway_data$Latitude, subway_data$Longitude)
  distances <- min(distances)
  subway_distance <-  c(subway_distance, distances)
}

ny_inspect_data <- ny_inspect_data %>%
  mutate(subway_distance = subway_distance)

rm(i, lat, lon, subway_data, haversine, subway_distance, distances)

save(ny_inspect_data, file = "./data/ny_inspect_data.RData")
#########################################################################################