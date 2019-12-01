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
ny_data = dplyr::select(ny_inspect_data, Inspection.Grade, shop_density, count, chain, rating_closest_neighb, Number_of_Reviews, Avr_Price, Numb_Rooms, neighbourhood_group)
ny_data = dplyr::select(ny_inspect_data, Inspection.Grade, shop_density, count, chain, rating_closest_neighb, Number_of_Reviews, Avr_Price, Numb_Rooms, neighbourhood_group,TotalPop.per.CenTrac,
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
                        Unemployment.per.CenTrac)
ny_data = dplyr::select(ny_inspect_data, Inspection.Grade, 
                        shop_density, 
                        count, 
                        chain, 
                        rating_closest_neighb, 
                        Number_of_Reviews, 
                        Avr_Price, 
                        Numb_Rooms, 
                        neighbourhood_group,
                        Employed.per.County,
                        PublicWork)

ny_data = dplyr::select(ny_inspect_data,
                        -Address,
                        -Trade.Name,
                        -County,
                        -Inspection.Date,
                        -Owner.Name,
                        -Street,
                        -State.Code,
                        -Zip.Code,
                        -Deficiency.Number,
                        -Deficiency.Description,
                        -Longitude,
                        -Latitude,
                        -Reviews,
                        -ny_inspect_data$X,
                        -ny_inspect_data$TractId,
                        -ny_inspect_data$TractId,
                        -Location,
                        -State.per.CenTrac,
                        -County.per.CenTrac,
                        -City,
                        -State.per.County)

ny_data = ny_data[c(1:7,10:ncol(ny_data))]

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
                   Pacific.per.County))


#transform to factors for random forest analysis
ny_data$Inspection.Grade = as.factor(ny_data$Inspection.Grade)
ny_data$chain = as.factor(ny_data$chain)
ny_data$rating_closest_neighb = as.factor(ny_data$rating_closest_neighb)
ny_data$neighbourhood_group = as.factor(ny_data$neighbourhood_group)
ny_data$Inspection.Grade = factor(ny_data$Inspection.Grade, levels = c(3, 2, 1), labels = c("A", "B", "C"))

length(which(is.na(ny_data[,]))) #check if any parameter contains NA values

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
  mtry       = seq(1, 81, by = 8),
  min.node.size  = 2,
  splitrule = c("gini", "extratrees"))
  
  
over_under_bagging <- function(df, Y, B, sample_size, tuneGrid) {
  
  bagging_error = as_tibble(matrix(ncol = 7, nrow = B*nrow(tuneGrid)))
  
  for (i in 1:B) {
   
    rf_train = bagging_sampling(ny_data, "Inspection.Grade", sample_size)
  
    #calculating models with CV
      ctrl <- trainControl(method = "cv", savePredictions = "final", allowParallel = TRUE)
      model_fit <- train(Inspection.Grade~., 
                       data = rf_train, 
                       method = "ranger", 
                       trControl = ctrl, 
                       tuneGrid = tuneGrid, 
                       importance = "impurity")
      index <- seq(1, nrow(bagging_error), by = nrow(tuneGrid))
      bagging_error[index[i]:(index[i+1]-1),] = model_fit$results[,]
      print(i)
}
    return(bagging_error)
}

ggplot(varImp(model_fit))
model_fit
ggplot(model_fit)

#Data prep for CV_errors
mean_CV_error = bagging_error
mean_CV_error = mean_CV_error %>% 
  rename(mtry=V1, nodeSize = V2, splitrule = V3, Accuracy = V4, Kappa = V5) %>% 
  mutate(splitrule = if_else(splitrule == 1, "gini", "extratrees")) %>% 
  select(-V6, -V7) %>% 
  group_by(mtry, nodeSize, splitrule) %>% 
  summarise(mean = mean(Accuracy))

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
  pred_matrix[,i] = model_fit$pred$pred
}

variableImportance_plot = ggplot()

chooseBestModel <- function(x) {
  tabulatedOutcomes <- table(x)
  sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
  mostCommonLabel <- names(sortedOutcomes)[1]
  mostCommonLabel
}

#Majority vote for the predictions
pred_matrix_majority = apply(pred_matrix, 1, chooseBestModel)


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


####Boosting-----
######################################


h2o.init(max_mem_size = "5g")
response <- "Inspection.Grade"
predictors <- setdiff(colnames(ny_data), response)
#setting parameter for tuning
B = 10
sample_size = c(700, 700, 700)


search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "missclassification",
  stopping_tolerance = 0.005,  # stop if improvement is < 0.5%
  stopping_rounds = 10,  # over the last 10 models
  max_runtime_secs = 60*60  # or stop search after 60 min.
)
#set tuning parameter

hyper_grid <- list(
  ntrees = c(500, 1000),
  max_depth = c(1, 3, 5),
  min_rows = c(1, 5, 10),
  learn_rate = c(0.01, 0.05, 0.1),
  col_sample_rate = c(.8, .9, 1)
)


over_under_bagging_boosting <- function(df, Y, B, sample_size, tuneGrid) {
  for (i in 1:B) {
    
    rf_train = as.h2o(bagging_sampling(ny_data, "Inspection.Grade", sample_size))
    split <- h2o.splitFrame(rf_train, ratios = 0.75, 0.25)
    train <- split[[1]]
    valid <- split[[2]]
   
     #calculating models with CV
    grid <- h2o.grid(
      algorithm = "gbm",
      grid_id = "gbm_grid",
      x = predictors, 
      y = response, 
      training_frame = train,
      validation_frame = valid,
      hyper_params = hyper_grid,
      search_criteria = search_criteria, # add search criteria
      ntrees = 5000,
      stopping_rounds = 10,
      stopping_tolerance = 0.005,
      seed = 123
    )
    print(i)
  }
  return(best_tune)
}
ggplot(varImp(model_fit))
ggplot(model_fit)


















over_under_bagging_boosting <- function(df, Y, B, sample_size, tuneGrid){
  set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
  bagged_predictions = matrix(nrow = sum(sample_size[1:3]), ncol = B) #allocating matrix
  best_tune = tibble(mtry = rep(NA, sum(sample_size[1:3])), splitrule = rep("not gini", sum(sample_size[1:3])), min.node.size = rep(NA, sum(sample_size[1:3])) )
  for(i in 1:length(classes)){
    nam <- paste("subset", classes[i], sep = "")
    assign(nam, df[which(as.matrix(df[Y])== classes[i]), ])
  }
  for(i in 1:B){
    sampleA <- sample(1:nrow(subsetA), sample_size[1], replace = T)
    sampleB <- sample(1:nrow(subsetB), sample_size[2], replace = T)
    sampleC <- sample(1:nrow(subsetC), sample_size[3], replace = T)
    bagging_data <- rbind(subsetA[sampleA, ], subsetB[sampleB, ], subsetC[sampleC, ])
    bagging_data <- bagging_data[sample(nrow(bagging_data)),]
    
    ctrl <- trainControl(method = "cv", savePredictions = "final")#, repeats = 3)
    model_fit <- train(Inspection.Grade~., data = bagging_data, method = "xgbTree", trControl = ctrl, tuneGrid_Boosting = tuneGrid)
    pred = model_fit$pred$pred
    
    best_tune[i,] = as.tibble(model_fit$bestTune)
    best_tune$splitrule[i] <-  as.character(model_fit$bestTune$splitrule)
    best_tune[i,4] <- model_fit$results[which.max(model_fit$results[,4]),4]
    best_tune[i,5] <- length(which(pred!=bagging_data$Inspection.Grade))/nrow(df)
    bagged_predictions [,i] = pred      
  }
  best_tune = best_tune %>% 
    rename(Accuracy = V4,Pred_Err = V5)
  
  bagging_list = list(bagged_predictions = bagged_predictions, best_tune = best_tune)
  rm(subsetA, subset1, subset2, ctrl, bagging_data, subset3, classes, pred, sampleA, sampleB, sampleC, Y)
  return(bagging_list)
}

tuneGrid_Boosting <- expand.grid(
  eta = c(.01, .05, .1, .3),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(.65, .8, 1),
  colsample_bytree = c(.8, .9, 1)

)
# implement Random Forest with different tuning parameter and under-bagging
boosting_under_bagging_error <- over_under_bagging_boosting(ny_data,
                                             Y = "Inspection.Grade",
                                             B = 10,
                                             sample_size = c(700, 700, 700),
                                             tuneGrid = tuneGrid_Boosting)

chooseBestModel <- function(x) {
  tabulatedOutcomes <- table(x)
  sortedOutcomes <- sort(tabulatedOutcomes, decreasing=TRUE)
  mostCommonLabel <- names(sortedOutcomes)[1]
  mostCommonLabel
}

bagged_predictions_matrix = as.matrix(bagging_list$bagged_predictions)
pred_boosting_underbagging <- apply(bagged_predictions_matrix, 1, FUN = chooseBestModel)
pred_boosting_underbagging <- factor(pred_boosting_underbagging, levels = c(3, 2, 1), labels = c("A", "B", "C"))

# implement Random Forest with different tuning parameter and over-bagging
boosting_over_bagging_error <- over_under_bagging(ny_data,
                                            Y = "Inspection.Grade",
                                            B = 10,
                                            sample_size = c(5000, 5000, 5000),
                                            tuneGrid = tuneGrid_Boosting)
bagged_predictions_matrix = as.matrix(bagging_list$bagged_predictions)
pred_boosting_overbagging <- apply(bagged_predictions_matrix, 1, FUN = chooseBestModel)
pred_rboostingoverbagging <- factor(pred_boosting_overbagging, levels = c(3, 2, 1), labels = c("A", "B", "C"))

rf_error <- full_join(boosting_under_bagging_error, boosting_over_bagging_error)
