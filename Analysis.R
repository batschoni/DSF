
# Tranformation before analysis
#########################################################################################

# Load data for entire state
load("./data/inspect_data.RData")
# Load data for NYC
load("./data/ny_inspect_data.RData")

set.seed(123)

data <- inspect_data %>%
  mutate(Inspection.Grade = factor(Inspection.Grade, levels = c(3, 2, 1), labels = c("A", "B", "C"))) %>%
  dplyr::select(c(Inspection.Grade, 
                  shop_density,
                  rating_closest_neighb,
                  chain,
                  count,
                  Number_of_Reviews))

ny_data <- ny_inspect_data %>%
  mutate(Inspection.Grade = factor(Inspection.Grade, levels = c(3, 2, 1), labels = c("A", "B", "C"))) %>%
  dplyr::select(c(Inspection.Grade, 
                  shop_density,
                  rating_closest_neighb,
                  chain,
                  count,
                  Number_of_Reviews,
                  neighbourhood_group,
                  Numb_Rooms,
                  Avr_Price,
                  subway_distance))

table(is.na(ny_data$Number_of_Reviews))

rm(ny_inspect_data, inspect_data)

# NA check

#########################################################################################

# First descriptive plots
#########################################################################################
#install.packages("ggthemes")
library(ggthemes)

ggplot(data = ny_data, aes(x=Inspection.Grade)) +
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
# Takes train and test dataset and the Y variable as character
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
  # add numbers to row labels
  row_names <- paste(1:length(row_names), sep = " ", row_names)
  error_rate <- matrix(data = NA, ncol = 1, nrow = comb_nrs)
  rownames(error_rate) <- row_names
  for(i in 1:comb_nrs){
    myformula <- paste( Y, '~', var_comb[i, 1] ) #<--------------------
    myformula <- as.formula(myformula)
    model_fit <- lda(myformula, data = df_train) #<--------------------
    model_pred <- predict(model_fit, df_test) #<--------------------
    correct_pred <- which(model_pred$class != as.matrix(df_test[Y])) #<--------------------
    error <- length(correct_pred) / nrow(df_test[Y])
    error_rate[i, 1] <- error
  }
  return(error_rate)
}

# Implements K-Fold Cross Validation
# Takes a dataframe, the Y variable as charakter and the number of fold K
k_fold_CV <- function(df, Y, K){
  fold <- round(nrow(df) / K)
  cross_val_err = matrix(data = NA, nrow = 2^(ncol(df)-1) - 1, ncol = K)
  for(i in 1:K){
    train_data <- df[-c((1+(i-1)*fold):(i*fold)),]
    testing_data <- df[(1+(i-1)*fold):(i*fold),]
    err <- model_selection_lda(train_data, testing_data, Y)
    cross_val_err[,i] <- err[, 1]
  }
  cross_val_err <- as.tibble(apply(cross_val_err, 1, mean, na.rm=TRUE))
  rownames(cross_val_err) = rownames(err)
  return(cross_val_err)
}

# Implement over- and underbagging with CV Errors
# Takes a dataframe, the Y variable as charakter, the number of bagged models B as well as
# as well as the number of each class from the original df
over_under_bagging <- function(df, Y, B, sample_size){
  set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
  }
  CV_err = matrix(data = NA, nrow = 2^(ncol(df) - 1) - 1, ncol = B)
  for(i in 1:B){
    sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
    sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
    sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
    bagging_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
    err <- k_fold_CV(bagging_data, Y, 10)
    CV_err[,i] <- as.matrix(err[, 1])
  }
  CV_err_final <- as.tibble(apply(CV_err, 1, mean, na.rm=TRUE))
  CV_err_final <- cbind(rownames(err), CV_err_final)
  #oob_err_final <- as.matrix(apply(oob_err, 1, mean, na.rm=TRUE))
  #rownames(oob_err_final) <-  rownames(err)
  return(CV_err_final)
}

# Implement over- and under-bagging with OOB Errors
over_under_bagging <- function(df, Y, B, sample_size){
  set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
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
lda_under_bagging_error <- over_under_bagging(ny_data,
                                              Y = "Inspection.Grade",
                                              B = 100,
                                              sample_size = c(700, 700, 700))
# implement LDA with over-bagging
lda_over_bagging_error <- over_under_bagging(ny_data,
                                             Y = "Inspection.Grade",
                                             B = 100,
                                             sample_size = c(5000, 5000, 5000))

lda_error <- full_join(lda_under_bagging_error, lda_over_bagging_error, by = "rownames(err)")

colnames(lda_error) = c("Covariates", "Under_bagging_error", "Over_bagging_error")

ggplot(data = lda_error, aes(x = Covariates, group=1)) +
  geom_line(aes(y = Under_bagging_error), color = "Blue" ) +
  geom_line(aes(y = Over_bagging_error), color = "Red") +
  labs(title="Prediction Rate LDA",
       x="Covariate Combination",
       y = "Error Rate") +
  theme_gray()

# Lowest OOB Error is achievd wih under-bagging and the variables count + number_of_reviews

rm(model_selection_lda, over_under_bagging, lda_over_bagging_error, lda_under_bagging_error)

#########################################################################################

# LDA Estimation
#########################################################################################
set.seed(1234)

# Returns a bagging sample
bagging_sample <- function(df, Y, sample_size){
  # get all classes of the Y variable
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  # create a subset of each class
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
  }
  # get samples of each class according to the sample size
  sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
  sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
  sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
  bagging_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
  return(bagging_data)
}

# number of bagging repetitions
B = 10
bagged_models=list()
# empty matrix for bagging predictions
bagged_predictions=matrix(data = NA, nrow = 2100, ncol = B)
for (i in 1:B){
  # bagging sample
  sample <- bagging_sample(ny_data,
                 Y = "Inspection.Grade",
                 sample_size = c(700, 700, 700))
  # fits the model with the bagging sample
  model_fit <- lda(Inspection.Grade~count + subway_distance, data = sample)
  bagged_models <- c(bagged_models, list(model_fit))
  # predicts the values for the entire dataset
  mode_pred <- predict(model_fit, data = ny_data)
  bagged_predictions[, i] <- mode_pred$class
}

# Implements majority voting over the B bagging predictions
maj_vote <- function(x) {
  table = table(x)
  majority = which.max(table)
}

pred_lda <- apply(bagged_predictions, 1, maj_vote)


lda_pred <- predict(test[2], data = data)

lda_pred$post[, c("A","B")] %*% c(1,1)
matrix((lda_pred$post[, c("A","B")] %*% c(1,1)), length(xs), length(ys))
       
lda_fit$scaling
table(data$Inspection.Grade, lda_pred$class)

# Grid values
resolution = 200
r <- sapply(data[c("count", "subway_distance")], range, na.rm = TRUE)
xs <- seq(r[1,1], r[2,1], length.out = resolution)
ys <- seq(r[1,2], r[2,2], length.out = resolution)
g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
colnames(g) <- colnames(r)
g <- as.data.frame(g)
# Lda prediction
lda_pred <- predict(lda_fit, g)
lda_pred <- as.factor(lda_pred$class)
# Grid data
grid_data <- cbind(lda_pred, g)
grid_data <- as.data.frame(grid_data)
# Decision border
dec_border <- matrix(as.integer(lda_pred), nrow = resolution, byrow = TRUE)
zs <- lda_pred$post[, c("A","B")] %*% c(1,1)

ggplot(data = data, aes(y = shop_density, x = Number_of_Reviews)) +
  #geom_point(data = grid_data, aes(color=lda_pred), alpha=0.1, size = 0.5) +
  geom_point(aes(color=Inspection.Grade), alpha=1)+
  geom_contour(aes(y = ys, x = xs, z=zs), 
               breaks=c(0,.5))
  theme_gray()

rm(g, grid_data, r, lda_pred, resolution, xs, ys, dec_border, zs)

length(lda_pred)
ggplot(data = data, aes(y = shop_density, x = count)) +
  geom_point(aes(color=Inspection.Grade), alpha=1) +
  theme_gray()
#scale_color_manual(values=c("darkgreen", "yellow", "red")) 

#########################################################################################