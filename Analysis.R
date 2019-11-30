library(plyr) # for ldply (similar to apply but output is a df not a list)
library(MASS) # For Discriminant Analysis
library(ISLR) # For Discriminant Analysis
library(class) # For KNN

# Tranformation before analysis----
#########################################################################################

# Load data for entire state
load("./data/inspect_data.RData")
# Load data for NYC
load("./data/ny_inspect_data.RData")

set.seed(123)

data <- inspect_data %>%
  mutate(Inspection.Grade = factor(Inspection.Grade, levels = c(1, 2, 3), labels = c("A", "B", "C"))) %>%
  dplyr::select(c(Inspection.Grade, 
                  shop_density,
                  rating_closest_neighb,
                  chain,
                  count,
                  Number_of_Reviews))

ny_data <- ny_inspect_data %>%
  mutate(Inspection.Grade = factor(Inspection.Grade, levels = c(1, 2, 3), labels = c("A", "B", "C"))) %>%
  dplyr::select(c(Inspection.Grade, 
                  shop_density,
                  rating_closest_neighb,
                  chain,
                  count,
                  Number_of_Reviews,
                  neighbourhood_group,
                  Numb_Rooms,
                  Avr_Price,
                  subway_distance,
                  TotalPop.per.CenTrac,
                  Men.per.CenTrac,
                  White.per.CenTrac,
                  IncomePerCap.per.CenTrac,
                  Poverty.per.CenTrac,
                  Professional.per.CenTrac,
                  Service.per.CenTrac,
                  Office.per.CenTrac,
                  Construction.per.CenTrac,
                  Production.per.CenTrac,
                  Transit.per.CenTrac,
                  Unemployment.per.CenTrac))

# change the variable from factor to numeric for KNN and Boosting
ny_data <- ny_data %>%
  mutate(neighbourhood_group = as.numeric(neighbourhood_group))

table(is.na(ny_data$Number_of_Reviews))

rm(ny_inspect_data, inspect_data)

# NA check

#########################################################################################

# First descriptive plots----
#########################################################################################

ggplot(data = ny_data, aes(x=Inspection.Grade)) +
  geom_histogram(stat = "count", fill = "lightgrey") +
  theme(legend.position="top") +
  labs(title="Histogram of Inspection Grades",
       x="Grade from A to C",
       y = "Count") +
  theme_gray()

#########################################################################################

# LDA Model Selection----
#########################################################################################


# LDA assumes that covariates have a multivariate Gaussian distribution
# Coefficients give the lane / plane where the prediction changes / prediction boundaries


forward_stepwise_selection <- function(df, Y, FUN, K = 10){
  browser()
  # All covariate names
  covariates <- colnames(df)
  covariates <- covariates[which(covariates != Y)]
  # number of covaraies
  p <- length(covariates)
  # Matrix to store errors
  model_errors <- matrix(data = c(1:p, rep(NA, p)), nrow = p, ncol = 2)
  # select first covariate randomly
  model_covariates <- c()
  for(i in 0:(p-1)){
    # Not yet used covariates
    not_model_covariates <- covariates[which(!(covariates %in% model_covariates))]
    # identity matrix to select each variable once
    select_covaraite <- .col(c((p-i), (p-i))) == .row(c((p-i), (p-i)))
    # formula for model estimate
    allModelsList <- apply(select_covaraite, 1, function(x)
      paste(c(model_covariates, not_model_covariates[x]),
            collapse= " + "))
    #allModelsList <- as.matrix(allModelsList)
    allModelsList <- lapply(allModelsList, function(x)
      as.formula(paste(c(Y, x), collapse = " ~ ")))
    # >>>> CV
    # Implement cross validation
    fold <- round(nrow(df) / K)
    cross_val_err = matrix(data = NA, nrow = length(not_model_covariates), ncol = K)
    for(j in 1:K){
      train_data <- df[-c((1+(j-1)*fold):(j*fold)),]
      testing_data <- df[(1+(j-1)*fold):(j*fold),]
      # Fit models
      model_fit <- lapply(allModelsList, function(x) FUN(x, data=train_data))
      # Predict
      model_pred <-  transpose(ldply(model_fit, function(x) predict(x, newdata=testing_data)$class))
      # Each column = Prediction results for one variable used
      model_pred <- data.frame(matrix(unlist(model_pred), ncol=length(model_pred), byrow=F))
      colnames(model_pred) <- not_model_covariates
      # Prediction Error (Rate of Wrong Predictions)
      pred_error <- apply(model_pred, 2, function(x) x != as.numeric(testing_data[, Y]))
      pred_error <- as.data.frame(
        apply(pred_error, 2, function(x) (nrow(testing_data)-sum(x, na.rm = TRUE))/nrow(testing_data)))
      cross_val_err[,j] <- pred_error[, 1]
    }
    # Average of CV prediction error
    cross_val_err <- as.tibble(apply(cross_val_err, 1, mean, na.rm=TRUE))
    # Best prediction (takes the first one if equal performance)
    best_covariate <- not_model_covariates[which(cross_val_err == min(cross_val_err))[1]]
    model_covariates <- c(model_covariates, best_covariate)
    # Error of best prediction
    model_errors[(i+1), 2] <- min(cross_val_err)
  }
}

forward_stepwise_selection(ny_data, "Inspection.Grade", lda)
paste( "Inspection.Grade", '~', "test")
test <- lda(Inspection.Grade ~ count, data = ny_data)
predict(test, newdata = ny_data)$class

best_subset_selection <- function(df_train, df_test, Y, FUN){
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
    myformula <- paste( Y, '~', var_comb[i, 1] )
    myformula <- as.formula(myformula)
    model_fit <- FUN(myformula, data = df_train)
    model_pred <- predict(model_fit, df_test)
    # If model = discriminant analysis
    if (length(model_pred) == 3){
      model_pred <- model_pred$class 
    }
    correct_pred <- which(model_pred != as.matrix(df_test[Y]))
    error <- length(correct_pred) / nrow(df_test[Y])
    error_rate[i, 1] <- error
  }
  return(error_rate)
}

# Implements K-Fold Cross Validation
# Takes a dataframe, the Y variable as charakter and the number of fold K
k_fold_CV <- function(df, Y, K, FUN){
  fold <- round(nrow(df) / K)
  cross_val_err = matrix(data = NA, nrow = 2^(ncol(df)-1) - 1, ncol = K)
  for(i in 1:K){
    train_data <- df[-c((1+(i-1)*fold):(i*fold)),]
    testing_data <- df[(1+(i-1)*fold):(i*fold),]
    err <- model_selection(train_data, testing_data, Y, FUN)
    cross_val_err[,i] <- err[, 1]
  }
  cross_val_err <- as.tibble(apply(cross_val_err, 1, mean, na.rm=TRUE))
  rownames(cross_val_err) = rownames(err)
  return(cross_val_err)
}

# Implement over- and underbagging with CV Errors
# Takes a dataframe, the Y variable as charakter, the number of bagged models B as well as
# as well as the number of each class from the original df
over_under_bagging <- function(df, Y, B, sample_size, FUN){
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
    err <- k_fold_CV(bagging_data, Y, 10, FUN)
    CV_err[,i] <- as.matrix(err[, 1])
  }
  CV_err_final <- as.tibble(apply(CV_err, 1, mean, na.rm=TRUE))
  CV_err_final <- cbind(rownames(err), CV_err_final)
  #oob_err_final <- as.matrix(apply(oob_err, 1, mean, na.rm=TRUE))
  #rownames(oob_err_final) <-  rownames(err)
  return(CV_err_final)
}

# Implement over- and under-bagging with OOB Errors
# Takes a dataframe, the Y variable as charakter, the number of bagged models B as well as
# as well as the number of each class from the original df
over_under_bagging <- function(df, Y, B, sample_size, FUN){
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
    err <- model_selection(train_data, testing_data, Y, FUN)
    oob_err[,i] <- err
  }
  oob_err_final <- as.tibble(apply(oob_err, 1, mean, na.rm=TRUE))
  oob_err_final <- cbind(rownames(err), oob_err_final)
  #oob_err_final <- as.matrix(apply(oob_err, 1, mean, na.rm=TRUE))
  #rownames(oob_err_final) <-  rownames(err)
  return(oob_err_final)
}

###
library(randomForest)
test <- randomForest(Inspection.Grade ~ count, data = ny_data)
test <- glm(Inspection.Grade ~ count, data = ny_data, family = "binomial")
test <- lda(Inspection.Grade ~ count, data = ny_data)
pred <- predict(test, data = ny_data)
length(pred)
###

# implement LDA with under-bagging
lda_under_bagging_error <- over_under_bagging(ny_data,
                                              Y = "Inspection.Grade",
                                              B = 2,
                                              sample_size = c(700, 700, 700),
                                              FUN = lda)
# implement LDA with over-bagging
lda_over_bagging_error <- over_under_bagging(ny_data,
                                             Y = "Inspection.Grade",
                                             B = 2,
                                             sample_size = c(5000, 5000, 5000),
                                             FUN = lda)

lda_error <- full_join(lda_under_bagging_error, lda_over_bagging_error, by = "rownames(err)")

# we create X-lables for the plot of the Bagged-CV Error
covariates <- ncol(ny_data) - 1
covariates_comb <- c()
for (i in 1:covariates){
  for (j in 1:choose(covariates,i)){
    covariates_comb <- c(covariates_comb, paste(i, j, sep = "."))
  }
}
lda_error <- cbind(lda_error, covariates_comb)

rm(i, j, covariates, covariates_comb)

colnames(lda_error) <- c("Covariates", "Under_bagging_error", "Over_bagging_error", "Model")

ggplot(data = lda_error, aes(x = Model, group=1)) +
  geom_line(aes(y = Under_bagging_error), color = "Blue" ) +
  geom_line(aes(y = Over_bagging_error), color = "Red") +
  labs(title="Prediction Rate LDA",
       x="Covariate Combination",
       y = "Error Rate") +
  theme_gray()

# Lowest OOB Error is achievd wih under-bagging and the variables count + number_of_reviews

rm(lda_over_bagging_error, lda_under_bagging_error)

#########################################################################################

# LDA Estimate Selected Model----
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
B = 100
bagged_models=list()
# empty matrix for bagging predictions
bagged_predictions=matrix(data = NA, nrow = nrow(ny_data), ncol = B)
# empty matrix for bagging used for the plot
bagged_predictions_plot=matrix(data = NA, nrow = nrow(g), ncol = B)
for (i in 1:B){
  # bagging sample
  sample <- bagging_sample(ny_data,
                 Y = "Inspection.Grade",
                 sample_size = c(700, 700, 700))
  # fits the model with the bagging sample
  model_fit <- lda(Inspection.Grade~shop_density + count, data = sample)
  bagged_models <- c(bagged_models, list(model_fit))
  # predicts the values for the entire dataset
  model_pred <- predict(model_fit, newdata = ny_data)
  bagged_predictions[, i] <- model_pred$class
  # predicts the values for the generated plot points
  model_pred <- predict(model_fit, newdata = g)
  bagged_predictions_plot[, i] <- model_pred$class
}

# Implements majority voting over the B bagging predictions
maj_vote <- function(x) {
  table = table(x)
  majority = which.max(table)
}

pred_lda <- apply(bagged_predictions, 1, maj_vote)
pred_lda <- factor(pred_lda, levels = c(3, 2, 1), labels = c("A", "B", "C"))
pred_lda_plot <- apply(bagged_predictions_plot, 1, maj_vote)
pred_lda_plot <- factor(pred_lda_plot, levels = c(3, 2, 1), labels = c("A", "B", "C"))

rm(maj_vote, sample, model_fit, model_pred, B, i)


# Grid values
data = ny_data
resolution = 200
r <- sapply(data[c("shop_density", "count")], range, na.rm = TRUE)
xs <- seq(r[1,1], r[2,1], length.out = resolution)
ys <- seq(r[1,2], r[2,2], length.out = resolution)
g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
colnames(g) <- colnames(r)
g <- as.data.frame(g)
# Grid data
grid_data <- cbind(pred_lda_plot, g)
grid_data <- as.data.frame(grid_data)
# Decision border
dec_border <- matrix(as.integer(lda_pred), nrow = resolution, byrow = TRUE)
zs <- lda_pred$post[, c("A","B")] %*% c(1,1)

ggplot(data = data, aes(y = shop_density, x = count)) +
  geom_point(data = grid_data, aes(color=pred_lda_plot), alpha=0.3, size = 0.5) +
  geom_point(aes(color=Inspection.Grade), alpha=1)+
  #geom_contour(aes(y = ys, x = xs, z=zs), 
  #             breaks=c(0,.5))
  theme_gray()

rm(g, grid_data, r, lda_pred, resolution, xs, ys, dec_border, zs)


#########################################################################################

# QDA Model Selection----
#########################################################################################

# LDA assumes that covariates have a multivariate Gaussian distribution
# Coefficients give the lane / plane where the prediction changes / prediction boundaries

# implement QDA with under-bagging
qda_under_bagging_error <- over_under_bagging(ny_data,
                                              Y = "Inspection.Grade",
                                              B = 10,
                                              sample_size = c(700, 700, 700),
                                              FUN = qda)
# implement QDA with over-bagging
qda_over_bagging_error <- over_under_bagging(ny_data,
                                             Y = "Inspection.Grade",
                                             B = 10,
                                             sample_size = c(5000, 5000, 5000),
                                             FUN = qda)

qda_error <- full_join(qda_under_bagging_error, qda_over_bagging_error, by = "rownames(err)")

# we create X-lables for the plot of the Bagged-CV Error
covariates <- ncol(ny_data) - 1
covariates_comb <- c()
for (i in 1:covariates){
  for (j in 1:choose(covariates,i)){
    covariates_comb <- c(covariates_comb, paste(i, j, sep = "."))
  }
}
lda_error <- cbind(lda_error, covariates_comb)

rm(i, j, covariates, covariates_comb)

colnames(lda_error) <- c("Covariates", "Under_bagging_error", "Over_bagging_error", "Model")

ggplot(data = lda_error, aes(x = Model, group=1)) +
  geom_line(aes(y = Under_bagging_error), color = "Blue" ) +
  geom_line(aes(y = Over_bagging_error), color = "Red") +
  labs(title="Prediction Rate LDA",
       x="Covariate Combination",
       y = "Error Rate") +
  theme_gray()

# Lowest OOB Error is achievd wih under-bagging and the variables count + number_of_reviews

rm(model_selection, over_under_bagging, lda_over_bagging_error, lda_under_bagging_error)

#########################################################################################

# QDA Estimate Selected Model----
#########################################################################################
set.seed(1234)

# number of bagging repetitions
B = 100
bagged_models=list()
# empty matrix for bagging predictions
bagged_predictions=matrix(data = NA, nrow = nrow(ny_data), ncol = B)
# empty matrix for bagging used for the plot
bagged_predictions_plot=matrix(data = NA, nrow = nrow(g), ncol = B)
for (i in 1:B){
  # bagging sample
  sample <- bagging_sample(ny_data,
                           Y = "Inspection.Grade",
                           sample_size = c(700, 700, 700))
  # fits the model with the bagging sample
  model_fit <- qda(Inspection.Grade~shop_density + count, data = sample)
  bagged_models <- c(bagged_models, list(model_fit))
  # predicts the values for the entire dataset
  model_pred <- predict(model_fit, newdata = ny_data)
  bagged_predictions[, i] <- model_pred$class
  # predicts the values for the generated plot points
  model_pred <- predict(model_fit, newdata = g)
  bagged_predictions_plot[, i] <- model_pred$class
}

# Implements majority voting over the B bagging predictions
maj_vote <- function(x) {
  table = table(x)
  majority = which.max(table)
}

pred_qda <- apply(bagged_predictions, 1, maj_vote)
pred_qda <- factor(pred_lda, levels = c(3, 2, 1), labels = c("A", "B", "C"))
pred_qda_plot <- apply(bagged_predictions_plot, 1, maj_vote)
pred_qda_plot <- factor(pred_lda_plot, levels = c(3, 2, 1), labels = c("A", "B", "C"))

rm(maj_vote, sample, model_fit, model_pred, B, i)


# Grid values
data = ny_data
resolution = 200
r <- sapply(data[c("shop_density", "count")], range, na.rm = TRUE)
xs <- seq(r[1,1], r[2,1], length.out = resolution)
ys <- seq(r[1,2], r[2,2], length.out = resolution)
g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
colnames(g) <- colnames(r)
g <- as.data.frame(g)
# Grid data
grid_data <- cbind(bagged_predictions_plot[,2], g)
grid_data <- as.data.frame(grid_data)
# Decision border
dec_border <- matrix(as.integer(qda_pred), nrow = resolution, byrow = TRUE)
zs <- lda_pred$post[, c("A","B")] %*% c(1,1)

ggplot(data = data, aes(y = shop_density, x = count)) +
  geom_point(data = grid_data, aes(color=pred_lda_plot), alpha=0.3, size = 0.5) +
  geom_point(aes(color=Inspection.Grade), alpha=1)+
  #geom_contour(aes(y = ys, x = xs, z=zs), 
  #             breaks=c(0,.5))
  theme_gray()

rm(g, grid_data, r, lda_pred, resolution, xs, ys, dec_border, zs)


#########################################################################################

# KNN Model Selection----
#########################################################################################

# Slightly adjust the function to KNN
model_selection_KNN <- function(df_train, df_test, Y, K){
  # all covariates
  col_names <- colnames(df_train)
  col_names <- col_names[which(col_names != Y)]
  # Class vectors
  Y_train <- df_train[, Y]
  Y_test <- df_test[, Y]
  # Change format that it works with knn function
  Y_train <- factor(as.matrix(Y_train))
  df_train <- df_train[, col_names]
  df_test <- df_test[, col_names]
  comb_nrs <- 2^length(col_names) - 1
  row_names <- c()
  comb_size <- 0
  iterations <- length(col_names)
  for(i in 1:iterations){
    comb_size <- comb_size + ncol(combn(col_names, i))
    row_names <- c(row_names, combn(col_names, i, function(x) paste(x, collapse='\n')))
  }
  error_rate <- matrix(data = NA, ncol = 1, nrow = comb_nrs)
  rownames(error_rate) <- row_names
  counter <- 1
  for(i in 1:iterations){
    for(j in 1:ncol(combn(1:iterations, i))){
    df_train_KNN <- df_train[, combn(1:iterations, i)[, j]]
    df_test_KNN <- df_test[, combn(1:iterations, i)[, j]]
    model_fit <- knn(train = df_train, test = df_test, cl = Y_train, k = K)
    wrong_pred <- which(model_fit != as.matrix(Y_test))
    error <- length(wrong_pred) / nrow(Y_test)
    error_rate[counter, 1] <- error
    counter <- counter + 1
    }
  }
  return(error_rate)
}

as.numeric((Y_train))

Y_train <- factor(as.matrix(ny_data[1:4000, "Inspection.Grade"]))
as.numeric(factor(as.matrix(Y_train)))
df_train <- ny_data[1:4000, ]
df_test <- ny_data[4001:7000, ]

knn(train = df_train, test = df_test, cl = Y_train, k = 10)

model_selection_KNN(df_train, df_test, "Inspection.Grade", 10)

k_fold_CV <- function(df, Y, K, k){
  fold <- round(nrow(df) / K)
  cross_val_err = matrix(data = NA, nrow = 2^(ncol(df)-1) - 1, ncol = K)
  for(i in 1:K){
    train_data <- df[-c((1+(i-1)*fold):(i*fold)),]
    testing_data <- df[(1+(i-1)*fold):(i*fold),]
    err <- model_selection_KNN(train_data, testing_data, Y, k)
    cross_val_err[,i] <- err[, 1]
  }
  cross_val_err <- as.tibble(apply(cross_val_err, 1, mean, na.rm=TRUE))
  rownames(cross_val_err) = rownames(err)
  return(cross_val_err)
}

over_under_bagging <- function(df, Y, B, sample_size, k){
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
    err <- k_fold_CV(bagging_data, Y, 10, k)
    CV_err[,i] <- as.matrix(err[, 1])
  }
  CV_err_final <- as.tibble(apply(CV_err, 1, mean, na.rm=TRUE))
  CV_err_final <- cbind(rownames(err), CV_err_final)
  #oob_err_final <- as.matrix(apply(oob_err, 1, mean, na.rm=TRUE))
  #rownames(oob_err_final) <-  rownames(err)
  return(CV_err_final)
}


# implement KNN with different Ks and under-bagging
Ks = 10
knn_under_bagging_error <- matrix(data = NA, 
                                  ncol = 2^(ncol(ny_data) - 1) -1,
                                  nrow = Ks)

knn_over_bagging_error <- matrix(data = NA, 
                                  ncol = 2^(ncol(ny_data) - 1) -1,
                                  nrow = Ks)
for(i in 1:Ks){
  knn_under_bagging_error[, Ks] <- over_under_bagging(ny_data,
                                                Y = "Inspection.Grade",
                                                B = 10,
                                                sample_size = c(700, 700, 700),
                                                k = Ks)
}
for(i in 1:Ks){
# implement KNN with different Ks and over-bagging
  knn_over_bagging_error[, Ks] <- over_under_bagging(ny_data,
                                             Y = "Inspection.Grade",
                                             B = 10,
                                             sample_size = c(5000, 5000, 5000),
                                             k = 10)
}
lda_error <- full_join(lda_under_bagging_error, lda_over_bagging_error, by = "rownames(err)")

# we create X-lables for the plot of the Bagged-CV Error
covariates <- ncol(ny_data) - 1
covariates_comb <- c()
for (i in 1:covariates){
  for (j in 1:choose(covariates,i)){
    covariates_comb <- c(covariates_comb, paste(i, j, sep = "."))
  }
}
lda_error <- cbind(lda_error, covariates_comb)

rm(i, j, covariates, covariates_comb)

colnames(lda_error) <- c("Covariates", "Under_bagging_error", "Over_bagging_error", "Model")

ggplot(data = lda_error, aes(x = Model, group=1)) +
  geom_line(aes(y = Under_bagging_error), color = "Blue" ) +
  geom_line(aes(y = Over_bagging_error), color = "Red") +
  labs(title="Prediction Rate LDA",
       x="Covariate Combination",
       y = "Error Rate") +
  theme_gray()

# Lowest OOB Error is achievd wih under-bagging and the variables count + number_of_reviews

rm(model_selection, over_under_bagging, lda_over_bagging_error, lda_under_bagging_error)

#########################################################################################