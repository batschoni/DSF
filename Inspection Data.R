# install.packages("ggmap")
# install.packages("raster")
library("tidyverse")
library("ggmap") # to get coordinates from a address
register_google(key = "AIzaSyB5prnz72uLxw3jlR7yUYr0qLDw62ZCot4") # the service is free but requires email registration
library("tidyverse")
# Library for spatial data
library("sp")
library("raster")


# downlaod and first cleaning of inspection data
#########################################################################################


# Download the file
inspect_data <- read.csv("https://data.ny.gov/api/views/d6dy-3h7r/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)

# Save it in the data folder
save(inspect_data, file = "./data/inspect_data.RData")

inspect_data <- as_tibble(inspect_data)

# Inspection grade as numbers
mapping <- c("A" = 3, "B" = 2, "C" = 1)
inspect_data <- inspect_data %>%
  mutate(Inspection.Grade = mapping[Inspection.Grade])

rm(mapping)

inspect_data = inspect_data %>% 
  mutate(Inspection.Date = as.Date(Inspection.Date, format = "%m/%d/%Y")) %>% #convert values to dates for calculation
  mutate(id = row_number()) %>% #add row number as id
  filter(Trade.Name != "") %>% #drops 7obs with missing trade name
  arrange(Inspection.Date) #sort by date

#if the same shop has several inspections - keep only the newest
inspect_data = inspect_data %>%
  group_by(Trade.Name) %>% 
  summarise_all(funs(last)) %>% #<----- check here Warning message: funs() is soft deprecated as of dplyr 0.8.0
  ungroup

# finds all shop chains
inspect_data_chains = inspect_data %>% 
  group_by(Owner.Name) %>%  #group by owner to see who owns more than one company
  summarise(count = n())

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

# 749 coordinates are missing
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
  filter(!is.na(Latitude)) # 267 still missing and are dropped

# add new coordinates
inspect_data <- inspect_data %>%
  filter(!is.na(Latitude)) %>%
  bind_rows(inspect_data_na)

table(is.na(inspect_data$Longitude)) # no more coordinates with NA

#rm(inspect_data_na)

save(inspect_data, file = "./data/inspect_data.RData")

#########################################################################################

# Add Demographic Information
#########################################################################################



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

#########################################################################################

# Filter for New York City
#########################################################################################

# only NY City
ny_counties <-  c("New York", "Kings", "Bronx", "Richmond", "Queens")
ny_inspect_data <- inspect_data[which(inspect_data$County %in% ny_counties),]

rm(ny_counties)

#########################################################################################

# NYC Subway locations
#########################################################################################

# Download the file
subway_data <- read.csv("https://data.ny.gov/api/views/i9wp-a4ja/rows.csv?accessType=DOWNLOAD&sorting=true", stringsAsFactors = FALSE)

# Save it in the data folder
save(subway_data, file = "./data/inspect_data.RData")

subway_data <- as_tibble(subway_data)

# Keep only unique stations
subway_data <- subway_data %>%
  distinct(Station.Name, .keep_all = TRUE) %>%
  rename(Latitude = Station.Latitude,
         Longitude = Station.Longitude)

# Distances in meter to next subway station (ONLY NYC)
subway_distance <- c()
for (i in 1:nrow(ny_inspect_data)){
  lat <-  as.numeric(ny_inspect_data$Latitude[1])
  lon <-  as.numeric(ny_inspect_data$Longitude[1])
  distances <- haversine(lat, lon, subway_data$Latitude, subway_data$Longitude)
  distances <- min(distances)
  subway_distance <-  c(subway_distance, distances)
}

ny_inspect_data <- ny_inspect_data %>%
  mutate(subway_distance = subway_distance)

#########################################################################################

# Tranformation before analysis
#########################################################################################

set.seed(123)

data <- inspect_data %>%
  mutate(Inspection.Grade = factor(Inspection.Grade, levels = c(3, 2, 1), labels = c("A", "B", "C"))) %>%
  dplyr::select(c(Inspection.Grade, shop_density, rating_closest_neighb, chain, count))

# Resammple to address imbalances in the data
paste("sub_insect_data","A", sep = "") <- data[which(data$Inspection.Grade=="A"), ]
sub_insect_dataB <- data[which(data$Inspection.Grade=="B"), ]
sub_insect_dataC <- data[which(data$Inspection.Grade=="C"), ]

length(as.matrix(unique(data["Inspection.Grade"])))
sort(as.matrix(unique(data["Inspection.Grade"])))
sample(1:nrow(sub_insect_dataA), 1500, replace=T)
sample(1:nrow(sub_insect_dataB), 500, replace=T)
sample(1:nrow(sub_insect_dataC), 1000, replace=T)

sub_insect_dataA$Inspection.Grade[1] %in% data$Inspection.Grade
nrow(merge(sub_insect_dataA$Inspection.Grade,data))

# under-sample A Grades
data2 <- sample(sub_insect_dataA, size = 2000, replace = FALSE)
data2 <- data2 %>%
  bind_rows(sub_insect_dataB) %>%
  bind_rows(sub_insect_dataC)
library(plyr)
#########################################################################################

# First descriptive plots
#########################################################################################
#install.packages("ggthemes")
library(ggthemes)

ggplot(data = data, aes(x=Inspection.Grade)) +
  geom_histogram(stat = "count", fill = "lightgrey") +
  theme(legend.position="top") +
  labs(title="Histogram of Inspection Grades",
       x="Grade from A to C",
       y = "Count") +
  theme_gray()

#########################################################################################

# LDA Model Selection
#########################################################################################
library(MASS) # For Discriminant Analysis
library(ISLR)

# LDA assumes that covariates have a multivariate Gaussian distribution
# Coefficients give the lane / plane where the prediction changes / prediction boundaries


# Implements lda for increasing number of covariates
model_selection_lda <- function(df_train, df_test, Y){
  # all covariates
  col_names <- colnames(df_train)
  col_names <- col_names[which(col_names != Y)]
  comb_nrs <- 2^length(col_names) - 1
  var_comb <- matrix(data = NA, ncol = 1, nrow = comb_nrs)
  row_names <- c()
  row_nr <- 1
  comb_size <- 0
  iterations <- length(col_names)
  for(i in 1:iterations){
    comb_size <- comb_size + ncol(combn(col_names, i))
    row_names <- c(row_names, combn(col_names, i, function(x) paste(x, collapse='\n')))
    var_comb[row_nr:comb_size, 1] <- combn(col_names, i, function(x) paste(x, collapse=' + '))
    row_nr <- row_nr + ncol(combn(col_names, i))
  }
  error_rate <- matrix(data = NA, ncol = 1, nrow = comb_nrs)
  rownames(error_rate) <- row_names
  for(i in 1:comb_nrs){
    myformula <- paste( Y, '~', var_comb[i, 1] )
    myformula <- as.formula(myformula)
    model_fit <- lda(myformula, data = df_train)
    model_pred <- predict(model_fit, df_test)
    correct_pred <- which(model_pred$class != as.matrix(df_test[Y]))
    error <- length(correct_pred) / nrow(df_test[Y])
    error_rate[i, 1] <- error
  }
  return(error_rate)
}

# Implements K-Fold Cross Validation
k_fold_CV <- function(df, Y, K){
  fold <- round(nrow(df) / K)
  cross_val_err = matrix(data = NA, nrow = ncol(df) - 1, ncol = K+1)
  for(i in 1:K){
    train_data <- df[-c((1+(i-1)*fold):(i*fold)),]
    testing_data <- df[(1+(i-1)*fold):(i*fold),]
    err <- model_selection_lda(train_data, testing_data, Y)
    cross_val_err[,i] <- err[, 2]
  }
  cross_val_err[,K+1] <- apply(cross_val_err, 1, mean)
  return(cross_val_err)
}

# Implement over- and under-bagging
over_under_bagging <- function(df, Y, B, sample_size){
  set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(data[Y])== classes[i]), ])
  }
  oob_err = matrix(data = NA, nrow = 2^(ncol(df) - 1) - 1, ncol = B)
  for(i in 1:B){
    sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
    sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
    sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
    train_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
    testing_data <- subsetA[-(sampleA),]
    testing_data <- testing_data[1:(sample_size[1]/2), ]
    testing_data <- rbind(testing_data,
                          subsetB[-(sampleB), ], 
                          subsetC[-(sampleC), ])
    err <- model_selection_lda(train_data, testing_data, Y)
    oob_err[,i] <- err
  }
  oob_err_final <- as.tibble(apply(oob_err, 1, mean, na.rm=TRUE))
  oob_err_final <- cbind(rownames(err), oob_err_final)
  #oob_err_final <- as.matrix(apply(oob_err, 1, mean, na.rm=TRUE))
  #rownames(oob_err_final) <-  rownames(err)
  return(oob_err_final)
}


# implement LDA with under-bagging
lda_under_bagging_error <- over_under_bagging(data,
                                             Y = "Inspection.Grade",
                                             B = 100,
                                             sample_size = c(1500, 500, 1000))
# implement LDA with over-bagging
lda_over_bagging_error <- over_under_bagging(data,
                                              Y = "Inspection.Grade",
                                              B = 100,
                                              sample_size = c(15000, 5000, 10000))

lda_error <- full_join(lda_under_bagging_error, lda_over_bagging_error, by = "rownames(err)")

colnames(lda_error) = c("Covariates", "Under_bagging_error", "Over_bagging_error")

ggplot(data = lda_error, aes(x = Covariates, group=1)) +
  geom_line(aes(y = Under_bagging_error), color = "Blue" ) +
  geom_line(aes(y = Over_bagging_error), color = "Red") +
  labs(title="Prediction Rate LDA",
       x="Covariate Combination",
       y = "OOB Error Rate") +
  theme_gray()

#########################################################################################

# LDA Model Selection
#########################################################################################

# Implement over- and under-bagging
over_under_bagging <- function(df, Y, B, sample_size){
  set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(data[Y])== classes[i]), ])
  }
  oob_err = matrix(data = NA, nrow = 2^(ncol(df) - 1) - 1, ncol = B)
  for(i in 1:B){
    sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
    sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
    sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
    train_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
    testing_data <- rbind(subsetA[-(sampleA), ], subsetB[-(sampleB), ], subsetC[-(sampleC), ])
    model_fit <- lda(Inspection.Grade ~ chain, data = train_data)
    model_pred <- predict(model_fit, testing_data)
  }
  return(cbind(model_pred$class, testing_data$Inspection.Grade))
}


decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  browser()
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- nrow(unique(cl))
  
  plot(data, col = (cl+1L), pch = (cl+1L), ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

as.integer(nrow(unique(data[, "Inspection.Grade"]))+1L)

test <- over_under_bagging(data,
                   Y = "Inspection.Grade",
                   B = 1,
                   sample_size = c(1500, 500, 1000))

test <- lda(Inspection.Grade ~ count + rating_closest_neighb, data = data)

decisionplot(test, data, class = "Inspection.Grade", main = "LDA")



ggplot(data = data, aes(y = shop_density, x = count)) +
  geom_point(aes(color=Inspection.Grade), alpha=1) +
  theme_gray()
#scale_color_manual(values=c("darkgreen", "yellow", "red")) 

#########################################################################################