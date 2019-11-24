
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
  
  #plot(data, col = (cl+1L), pch = (cl+1L), ...)
  plot(data, col = 4, pch = 4, ...)
  
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



data2 <- data %>%
  dplyr::select(count, rating_closest_neighb, Inspection.Grade)



as.integer(nrow(unique(data[, "Inspection.Grade"]))+1L)



test <- over_under_bagging(data,
                           Y = "Inspection.Grade",
                           B = 1,
                           sample_size = c(1500, 500, 1000))

test <- lda(Inspection.Grade ~ count + rating_closest_neighb, data = data2)

plot(test, col = as.integer(data2$Inspection.Grade))

test$scaling

decisionplot(test, data2, class = "Inspection.Grade", main = "LDA")



ggplot(data = data, aes(y = shop_density, x = count)) +
  geom_point(aes(color=Inspection.Grade), alpha=1) +
  theme_gray()
#scale_color_manual(values=c("darkgreen", "yellow", "red")) 

#########################################################################################