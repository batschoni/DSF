library(rsample)      # data splitting 
library(randomForest) # basic implementation of random forests
library(ranger)       # a faster implementation of random forests
library(caret)        # an aggregator package for performing many machine learning models
library(tidyverse)
library(ggplot2)     # model visualization
library(pdp)         # model visualization
library(vtreat)
library(xgboost)
library(h2o)

load("data/ny_inspect_data.Rdata") #loading the data set
ny_inspect_data = as.tibble(ny_inspect_data)
ny_inspect_data = na.omit(ny_inspect_data)
# Remove everything except the potential covariates
ny_data <- ny_inspect_data %>%
  dplyr::select(-c(Address,
                   Trade.Name, 
                   County, 
                   Inspection.Date, 
                   Owner.Name, 
                   Street, 
                   City, 
                   State.Code, 
                   Zip.Code, 
                   Deficiency.Number, 
                   Deficiency.Description, 
                   X, 
                   TractId, 
                   Location, 
                   State.per.CenTrac, 
                   County.per.CenTrac, 
                   State.per.County, 
                   neighbourhood_group, 
                   Pacific.per.County,
                   Latitude,
                   Longitude))

# For computation restriction, we need to limit our analysis to the most important covariates
# Therfore, we take the 20 variables with the highest correlation to Inspection Grade.

# Correlation in aboslute term to Inspetion Grade
res <- abs(cor(ny_data))[,1]
res <- as.data.frame(res)
# 21 largest correlation (inclusive Inspection Grade to itself)
largest_corr <- sort(res[,1], decreasing = TRUE)[1:41]
covariates <- rownames(res)
# 20 variables with the larges correlation to inspection grade
covariates <- covariates[which(res[,1] %in% largest_corr)]

# Demographic data are extremely prone to multicolinrearity
# Check correlation of variables
cor = cor(ny_data)
# exclude all that are highly correlated to each other
covariates <- covariates[which(!(covariates %in% c("SelfEmployed.per.County", 
                                                   "Walk.per.County", 
                                                   "PrivateWork.per.County", 
                                                   "Construction.per.County", 
                                                   "Drive.per.County", 
                                                   "Carpool.per.County", 
                                                   "Men.per.County", 
                                                   "MeanCommute.per.County", 
                                                   "WorkAtHome.per.County",
                                                   "FamilyWork.per.County",
                                                   "PublicWork.per.County",
                                                   "VotingAgeCitizen.per.County",
                                                   "Transit.per.County",
                                                   "Employed.per.County",
                                                   "Native.per.County",
                                                   "TotalPop.per.County",
                                                   "PrivateWork.per.CenTrac",
                                                   "IncomeErr.per.County",
                                                   "IncomePerCap.per.County",
                                                   "Hispanic.per.County",
                                                   "Longitude",
                                                   "Latitude")))]
#"Construction.per.County", "Drive.per.County"

# Select 20 best covariates
ny_data <- ny_data %>%
  mutate(Inspection.Grade = factor(Inspection.Grade, levels = c(1, 2, 3), labels = c("A", "B", "C"))) %>%
  dplyr::select(covariates)



#transform to factors for random forest analysis
ny_data$Inspection.Grade = as.factor(ny_data$Inspection.Grade)
ny_data$chain = as.factor(ny_data$chain)
ny_data$rating_closest_neighb = as.factor(ny_data$rating_closest_neighb)
ny_data$neighbourhood_group = as.factor(ny_data$neighbourhood_group)
#ny_data$Inspection.Grade = factor(ny_data$Inspection.Grade, levels = c(3, 2, 1), labels = c("A", "B", "C"))

length(which(is.na(ny_data[,]))) #check if any parameter contains NA values

rm(res, cor, covariates, largest_corr, ny_inspect_data)
###Random Forest----

###Model Selection---------

#dealing with imbalanced training data
bagging_sampling <- function(df, Y, sample_size) {
  #set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
  }
  
  sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
  sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
  sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
  bagging_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
  bagging_data <- bagging_data[sample(nrow(bagging_data)),]
  
  return(bagging_data)
}

#setting parameter for tuning
B = 10
sample_size = c(700, 700, 700)



#set tuning parameter
tuneGrid <- expand.grid(
  mtry       = seq(1, 20, by = 2),  
  min.node.size  = seq(2,9,2),
  splitrule = c("gini"))
  
  
  
  over_under_bagging <- function(df, Y, B, sample_size, tuneGrid) {
    
    #allocate matrix for results
    bagging_error = as_tibble(matrix(ncol = 7, nrow = (B+1)*nrow(tuneGrid)))
    
    for (i in 1:B) {
      
      rf_train = bagging_sampling(ny_data, "Inspection.Grade", sample_size)
      
      #calculating models with CV
      ctrl <- trainControl(method = "cv", savePredictions = "final", allowParallel = TRUE) #use parallelization to enhance computation speed
      model_fit <- train(Inspection.Grade~., 
                         data = rf_train, 
                         method = "ranger", #use of random forest
                         trControl = ctrl, #set up the settings for CV
                         tuneGrid = tuneGrid, #calculate the best model using the tuning parameters
                         importance = "impurity") #to analyze the variable importance
      
      index <- seq(1, nrow(bagging_error), by = nrow(tuneGrid)) #save the results in the matrix
      bagging_error[index[i]:(index[i+1]-1),] = model_fit$results[,]
      
      print(i)
    }
    return(bagging_error)
  }
  
  #Function Data prep for CV_errors
  mean_CV_error = function(bagging_error) {
    
    mean_CV_error = bagging_error
    mean_CV_error = mean_CV_error %>% 
      rename(mtry=V1, nodeSize = V2, splitrule = V3, Accuracy = V4, Kappa = V5) %>% 
      #mutate(splitrule = if_else(splitrule == 1, "gini", "extratrees")) %>% 
      select(-V6, -V7) %>% 
      group_by(mtry, nodeSize, splitrule) %>% 
      summarise(mean = mean(Accuracy, na.rm = TRUE)) %>% 
      arrange(mean)
      
    mean_CV_error = mean_CV_error[1:nrow(mean_CV_error)-1,]
    
    return(mean_CV_error)
  }
  
  #Model without bagging for comparison
  
  for (i in 1:B) {
    
  without_model <- train(Inspection.Grade~., 
                     data = ny_data, 
                     method = "ranger", #use of random forest
                     trControl = ctrl, #set up the settings for CV
                     tuneGrid = tuneGrid, #calculate the best model using the tuning parameters
                     importance = "impurity") #to analyze the variable importance
  
  bagging_error[index[i]:(index[i+1]-1),] = model_fit$results[,]
  
  }
  under_bagging_error <- over_under_bagging(ny_data, "Inspection.Grade", B = 10, tuneGrid = tuneGrid, sample_size = c(700,700,700))
  over_bagging_error <- over_under_bagging(ny_data, "Inspection.Grade", B = 10, tuneGrid = tuneGrid, sample_size = c(5000,5000,5000))
  
  #calculating the mean errors over all estimations for under and over-sampling
  mean_CV_error_under <- mean_CV_error(under_bagging_error)
  mean_CV_error_over <- mean_CV_error(over_bagging_error)
  mean_CV_error_without <- mean_CV_error(without_bagging_error)
  
  
  #Graphs
  ggplot(varImp(model_fit))
  model_fit$times
  ggplot(model_fit)
  plot(model_fit)


bagging1bis10 = bagging_error
rm(tuneGrid, ctrl, rf_train, index, i, sample_size)

parameter_optimal_rf = mean_CV_error[which.max(mean_CV_error$mean),]

#train the optimal model, evaluated from above
pred_matrix = matrix(nrow=sum(sample_size), ncol=B)

for (i in 1:B) {
  
  
  rf_optimal_training <- bagging_sampling(df = ny_data, Y = "Inspection.Grade", sample_size = c(700,700,700))
 
  
  optimal_model_rf = ranger(Inspection.Grade~.,
                            mtry = as.numeric(parameter_optimal_rf[1]),
                            splitrule = as.character(parameter_optimal_rf[3]),
                            min.node.size = as.numeric(parameter_optimal_rf[2]),
                            data = rf_optimal_training,
                            importance = "impurity")
  
  pred = optimal_model_rf$predictions
  pred = predict(optimal_model_rf, data = ny_data)
  pred_matrix[,i] = model_fit$pred$pred
}

variableImportance_plot = ggplot(optimal_model_rf$variable.importance)

#function to use the majority vote in a data frame
chooseBestModel <- function(x) {
  tabulatedOutcomes <- table(x) 
  sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
  mostCommonLabel <- names(sortedOutcomes)[1]
  mostCommonLabel
}

#Majority vote for the predictions
pred_matrix_majority = as.numeric(apply(pred_matrix, 1, chooseBestModel))

length(which(pred$predictions!=ames_test$Inspection.Grade))/nrow(ames_test)
rf_error <- full_join(rf_under_bagging_error, rf_over_bagging_error, by)



# we create X-lables for the plot of the Bagged-CV Error
covariates <- ncol(ny_data) - 1
covariates_comb <- c()
for (i in 1:covariates){
  for (j in 1:choose(covariates,i)){
    covariates_comb <- c(covariates_comb, paste(i, j, sep = "."))
  }
}
model_fit$terms
rf_error <- cbind(rf_error, covariates_comb)


rm(i, j, covariates, covariates_comb)

colnames(lda_error) <- c("Covariates", "Under_bagging_error", "Over_bagging_error", "Model")

ggplot(data = lda_error, aes(x = Model, group=1)) +
  geom_line(aes(y = Under_bagging_error), color = "Blue" ) +
  geom_line(aes(y = Over_bagging_error), color = "Red") +
  labs(title="Prediction Rate LDA",
       x="Covariate Combination",
       y = "Error Rate") +
  theme_gray()



ggplot(varImp(model_fit))
ggplot(model_fit)


#tuning parameter
hyper_grid <- expand.grid(
  eta = c(.01, .05, .1, .3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(.65, .8, 1), 
  colsample_bytree = c(.8, .9, 1),
)


over_under_bagging_boosting <- function(df, Y, B, sample_size, tuneGrid) {
  
  #allocate matrix for results
  bagging_error = as_tibble(matrix(ncol = 7, nrow = (B+1)*nrow(tuneGrid)))
  
  for (i in 1:B) {
    
    rf_train = bagging_sampling(ny_data, "Inspection.Grade", sample_size)
    
    #calculating models with CV
    ctrl <- trainControl(method = "cv", savePredictions = "final", allowParallel = TRUE) #use parallelization to enhance computation speed
    model_fit <- train(Inspection.Grade~., 
                       data = rf_train, 
                       method = "xbgTree", #use of xtreme boosting gradient
                       trControl = ctrl, #set up the settings for CV
                       tuneGrid = tuneGrid, #calculate the best model using the tuning parameters
                       importance = "impurity") #to analyze the variable importance
    
    index <- seq(1, nrow(bagging_error), by = nrow(tuneGrid)) #save the results in the matrix
    bagging_error[index[i]:(index[i+1]-1),] = model_fit$results[,]
    
    print(i)
  }
  return(bagging_error)
}

#calculate the responding under and over-bagging errors for xtreme gradient boosting
unders_boosting_error <- over_under_bagging_boosting(ny_data, B = 10, Y = "Inspection.Grade", sample_size = c(700,700,700))
over_boosting_error <- over_under_bagging_boosting(ny_data, B = 10, Y = "Inspection.Grade", sample_size = c(5000,5000,5000))

mean_CV_error_under_boosting <- mean_CV_error(under_boosting_error)
mean_CV_error_over_boosting <- mean_CV_error(over_boosting_error)

#the evaluation of the parameters of the optimal model
parameter_optimal_boosting = mean_CV_error[which.max(mean_CV_error$mean),]

#train the optimal model, evaluated from above

B = 100
sample_size = c(700,700,700)
pred_matrix_boosting = matrix(nrow=sum(sample_size), ncol=B)

#optimal evaluated tuning parameter for xgboosting
params <- list(
  eta = 0.01,
  max_depth = 5,
  min_child_weight = 5,
  subsample = 0.65,
  colsample_bytree = 1
)

#use B = 100 iterations to get a stable perdiction and apply the majority vote for the final prediction
for (i in 1:B) {
  
  
  boosting_optimal_training <- bagging_sampling(df = ny_data, Y = "Inspection.Grade", sample_size = c(700,700,700))

  #calculating final model with evaluated parameters
  optimal_model_boosting  <- xgboost(
          params = params,
          data = boosting_optimal_training,
          label = "Inspection.Grade",
          nrounds = 1430,
          objective = "multi:softprob",
          verbose = 1
  )
  
  pred = optimal_model_boosting$predictions
  pred = predict(optimal_model_boosting, data = ny_data)
  pred_matrix_boosting[,i] = model_fit$pred$pred
}

#importance of the variables used in the boosting model
importance_matrix <- xgb.importance(model = optimal_model_boosting)
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

#partial dependence plot to show marginal effect of one or two features on outcome
pdp_plot <- optimal_model_boosting %>%
  partial(pred.var = "shop_density", n.trees = 1430, grid.resolution = 100, train = boosting_optimal_training) %>%
  autoplot(rug = TRUE, train = boosting_optimal_training) +
  ggtitle("PDP")

pdp_plot


ice_plot <- optimal_model_boosting %>%
  partial(pred.var = "shop_density", n.trees = 1430, grid.resolution = 100, train = boosting_optimal_training, ice = TRUE) %>%
  autoplot(rug = TRUE, train = boosting_optimal_training, alpha = .1, center = TRUE) +
  ggtitle("ICE")

ice_plot