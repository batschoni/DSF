
library("tidyverse")

# downlaod and cleaning of inspection data
#########################################################################################

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
  # keeps 2nd element of previous split
  string_vector <- sapply(string_vector, "[", 2)
  # splits string at ")"
  string_vector <- strsplit(string_vector, "\\)")
  # keeps 1st split
  latitude <- sapply(string_vector, "[", 1)
  location <- cbind(longitude, latitude)
  return(location)
}

summary(is.na(coord(inspections$Location)[,1]))



save(inspections, file = "./data/Retail_Food_Store_Inspections.RData")

#########################################################################################

# Library for spatial data
library("sp")

m <- matrix(c(0, 0, 1, 1), ncol = 2, dimnames = list(NULL, c("min", "max")))
crs <- CRS(projargs = as.character(NA))
S <- Spatial(bbox = m, proj4string = crs)
CRAN_df <- 