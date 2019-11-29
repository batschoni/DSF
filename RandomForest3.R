library(rsample)      # data splitting 
library(randomForest) # basic implementation of random forests
library(ranger)       # a faster implementation of random forests
library(caret)        # an aggregator package for performing many machine learning models
library(tidyverse)

load("data/ny_inspect_data.Rdata") #loading the data set
ny_data = dplyr::select(ny_inspect_data, Inspection.Grade, shop_density, count, chain, rating_closest_neighb, Number_of_Reviews, Avr_Price, Numb_Rooms, neighbourhood_group)

#transform to factors for random forest analysis
ny_data$Inspection.Grade = as.factor(ny_data$Inspection.Grade)
ny_data$chain = as.factor(ny_data$chain)
ny_data$rating_closest_neighb = as.factor(ny_data$rating_closest_neighb)
ny_data$neighbourhood_group = as.factor(ny_data$neighbourhood_group)

length(which(is.na(ny_data[,]))) #check if any parameter contains NA values

###Random Forest----

over_under_bagging <- function(df, Y, B, sample_size, tuneGrid){
  set.seed(123)
  classes <- as.matrix(unique(df[Y]))
  classes <- sort(classes)
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
                    
              ctrl <- trainControl(method = "cv")#, repeats = 2)
              model_fit <- train(Inspection.Grade~., data = bagging_data, method = "ranger", trControl = ctrl, tuneGrid = tuneGrid)
              pred = predict(model_fit, newdata = df)
              best_tune[i,] <-  as_tibble(model_fit$bestTune)
              best_tune[i,4] <- model_fit$results[which.max(model_fit$results[,4]),4]
              best_tune[i,5] <- length(which(pred!=df$Inspection.Grade))/nrow(df)
                    #CV_err[,i] <- as.matrix(err[, 1])
                  }
                  best_tune = best_tune %>% 
                    rename(Accuracy = V4,Pred_Err = V5)
                  rm(subsetA, subsetB, subsetC, ctrl, bagging_data, B, classes, pred, sampleA, sampleB, sampleC, Y)
                  return(best_tune)
}

#set tuning parameter
tuneGrid <- expand.grid(
  mtry       = seq(1, 8, by = 2),
  min.node.size  = seq(2, 8, by = 2),
  splitrule = c("gini", "extratrees")
)
tune1 = best_tune
# implement Random Forest with different tuning parameter and under-bagging
rf_under_bagging_error <- over_under_bagging(ny_data,
                                                      Y = "Inspection.Grade",
                                                      B = 10,
                                                      sample_size = c(700, 700, 700),
                                                      tuneGrid = tuneGrid)


# implement Random Forest with different tuning parameter and over-bagging
rf_over_bagging_error <- over_under_bagging(ny_data,
                                             Y = "Inspection.Grade",
                                             B = 10,
                                             sample_size = c(5000, 5000, 5000),
                                             tuneGrid = tuneGrid)

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
