# install.packages("ggmap")

library("tidyverse")
library("ggmap") # to get coordinates from a address
register_google(key = "") # the service is free but requires email registration

library("tidyverse")

# downlaod and cleaning of inspection data
#########################################################################################


# Download the file
inspections <- read.csv("https://data.ny.gov/api/views/d6dy-3h7r/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)
inspectionsL <- unite(inspections, Address , c(Street, City, State.Code, Zip.Code), sep = ", ", remove = FALSE)
inspection <- inspectionsL  %>% distinct(Trade.Name, .keep_all = TRUE)
length(unique(inspection$Address))

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
  longitude <- sapply(string_vector, "[", 1)

  longitude <- as.numeric(longitude)


  # keeps 2nd element of previous split
  string_vector <- sapply(string_vector, "[", 2)
  # splits string at ")"
  string_vector <- strsplit(string_vector, "\\)")
  # keeps 1st split
  latitude <- sapply(string_vector, "[", 1)

  latitude <- as.numeric(latitude)
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
  select(-Location)

# 1029 coordinates are missing
table(is.na(inspections$Longitude))

# address column
inspections <- inspections %>%
  mutate(Address = str_c(Street, Zip.Code, sep = ", ")) %>%
  mutate(Address = str_c(Address, City, sep = " ")) %>%
  mutate(Address = str_c(Address, State.Code, sep = ", "))

# use Google maps to get missing coordiantes
inspections_na <- inspections %>%
  filter(is.na(Latitude)) %>%
  mutate_geocode(Address) %>%
  mutate(Latitude = lat, Longitude = lon) %>%
  select(-c(lat, lon)) %>%
  filter(is.na(Latitude)) # 267 still missing and are dropped

# add new coordinates
inspections %>%
  filter(!is.na(Latitude)) %>%
  bind_rows(inspections_na)

# How much coordinates are missing only NY City?
ny_counties <-  c("New York", "Kings", "Bronx", "Richmond", "Queens")
ny_inspections <- inspections[which(inspections$County %in% ny_counties),]
table(is.na(ny_inspections$Longitude))


summary(is.na(coord(inspections$Location)[,1]))



save(inspections, file = "./data/Retail_Food_Store_Inspections.RData")


#########################################################################################

# Library for spatial data
library("sp")

m <- matrix(c(0, 0, 1, 1), ncol = 2, dimnames = list(NULL, c("min", "max")))
crs <- CRS(projargs = as.character(NA))
S <- Spatial(bbox = m, proj4string = crs)
CRAN_df <- 