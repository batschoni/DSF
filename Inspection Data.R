# install.packages("ggmap")
# install.packages("raster")
library("tidyverse")
library("ggmap") # to get coordinates from a address
register_google(key = "") # the service is free but requires email registration
library("tidyverse")
# Library for spatial data
library("sp")
library("raster")


# downlaod and cleaning of inspection data
#########################################################################################


# Download the file
inspections <- read.csv("https://data.ny.gov/api/views/d6dy-3h7r/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)

# Save it in the data folder
save(inspections, file = "./data/Retail_Food_Store_Inspections.RData")


inspections <- read.csv("https://data.ny.gov/api/views/d6dy-3h7r/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)


inspections <- as_tibble(inspections)

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
####
#insepctions2 <- inspections
inspections <- insepctions2
####

# We replace location by Longitude and Latitude
inspections <- inspections %>%
  mutate(Longitude = coord(inspections$Location)[,1],
         Latitude = coord(inspections$Location)[,2]) %>%
  dplyr::select(-Location)

# 1044 coordinates are missing
table(is.na(inspections$Longitude))

# create address column
inspections <- inspections %>%
  mutate(Address = str_c(Street, Zip.Code, sep = ", ")) %>%
  mutate(Address = str_c(Address, City, sep = " ")) %>%
  mutate(Address = str_c(Address, State.Code, sep = ", "))

# use Google maps to get missing coordiantes (takes few minutes)
inspections_na <- inspections[] %>%
  filter(is.na(Latitude)) %>%
  mutate_geocode(Address) %>%
  mutate(Latitude = lat, Longitude = lon) %>%
  dplyr::select(-c(lat, lon)) %>%
  filter(!is.na(Latitude)) # 267 still missing and are dropped

# add new coordinates
inspections <- inspections %>%
  filter(!is.na(Latitude)) %>%
  bind_rows(inspections_na)





save(inspections, file = "./data/Retail_Food_Store_Inspections.RData")


#########################################################################################

# Filter for New York City
#########################################################################################
# How much coordinates are missing only NY City?
ny_counties <-  c("New York", "Kings", "Bronx", "Richmond", "Queens")
ny_inspections <- inspections[which(inspections$County %in% ny_counties),]
table(is.na(ny_inspections$Longitude))


summary(is.na(coord(inspections$Location)[,1]))

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

# gives me the n closest obs. to coordniates in df
n_closest <- function(df, n, colnr_lat, colnr_lon, lat, lon){
  dist_vect <- haversine(lat, lon, as.matrix(df[, colnr_lat]), as.matrix(df[, colnr_lon]))
  dist_sort <- sort(dist_vect, decreasing = FALSE)[1:n]
  rows <- which(dist_vect %in% dist_sort)
  inspections_sub <- inspections[rows, ]
  return(inspections_sub)
}

# get the rating of the closes shop
rating_closest_neighb <- c()
for (i in 1:nrow(inspections)){
  lat = as.numeric(inspections[i, 13])
  lon = as.numeric(inspections[i, 12])
  inspections_sub <- inspections %>%
    slice(-i)
  inspections_sub <- n_closest(inspections_sub, 1, 13, 12, lat, lon)
  rating_closest_neighb <- c(rating_closest_neighb, inspections_sub[2])
}
