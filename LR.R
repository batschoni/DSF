#LR(nL)
#remove.packages("ggplot2") # Unisntall ggplot
#install.packages("ggplot2") # Install it again
#install.packages("ggthemes")
library(ggplot2)
library(MASS)
library(ISLR)
library(class)
library(ggthemes)
library(corrplot)
library(MASS)
library(glmnet)
library(magrittr)

## Data Preparation

# Load data for NYC
load("~/GitHub/DSF/data/ny_inspect_data.RData")
load("~/GitHub/DSF/data/ny_inspect_data2.RData")
ny_inspect_data_save <- ny_inspect_data
CC <- complete.cases(ny_inspect_data$Inspection.Grade)
ny_inspect_dat <- ny_inspect_data[CC,]
ny_inspect_dat <- na.omit(ny_inspect_dat)
numeric_data <- ny_inspect_data[,-c(1:3, 5:12, 20:24, 60:61, 97)]

set.seed(123)

# Preliminary visualization

ggplot(data = ny_inspect_data, aes(x=Inspection.Grade)) +
  geom_histogram(stat = "count", fill = "black") +
  theme(legend.position="top") +
  labs(title="Histogram of Inspection Grades",
       x="Grade from A to C",
       y = "Count") +
  theme_gray()

corrplot(cor(tD[,c("Number_of_Reviews", "Income.per.CenTrac")]), method = "circle")
corrplot(cor(numeric_data), method = "circle", use="pairwise.complete.obs")
corrplot.mixed(cor(numeric_data), lower.col = "black", number.cex = 1)




    
#For only "Issue" & "No issue"
    
tD <- ny_inspect_data

tD$Inspection.Grade[tD$Inspection.Grade == 1] <- "Issue"
tD$Inspection.Grade[tD$Inspection.Grade == 2] <- "Issue"
tD$Inspection.Grade[tD$Inspection.Grade == 3] <- "No Issue"

#sample_tD <- sample.int(n = nrow(tD), size = floor(.75*nrow(tD)), replace = F)
#tD_train <- tD[sample_tD, ]
#tD_test  <- tD[-sample_tD, ]

#tDtrain <- ny_inspect_data[]

ggplot(data = tD, aes(x=Inspection.Grade)) +
  geom_histogram(stat = "count", fill = "black") +
  theme(legend.position="top") +
  labs(title="Histogram of Inspection Grades",
       x="from No Issue to Issue",
       y = "Count") +
  theme_gray()



#####################################
#Logistical Regression (no L)
################################

# Binomial logistical regression i.e. Only "Issue" & "No issue"
tDa <- na.omit(tD[,c("Inspection.Grade", "Number_of_Reviews", "Income.per.CenTrac")])
train_tDa <- sample.int(n = nrow(tDa), size = floor(.75*nrow(tDa)), replace = F)
#tDa_train <- na.omit(tD_train[,c("Inspection.Grade", "Number_of_Reviews", "Income.per.CenTrac")])
#tDa_test <- na.omit(tD_test[,c("Inspection.Grade", "Number_of_Reviews", "Income.per.CenTrac")])

glm.fits = glm(formula = as.factor(Inspection.Grade) ~ Number_of_Reviews + Income.per.CenTrac, family = binomial, data = tDa, subset = train_tDa)

summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef

glm.probs = predict(glm.fits, tDa[-train_tDa,], type = "response")
# gives the probability that there will be an Issue
contrasts(as.factor(tD$Inspection.Grade))
glm.pred = rep("Issue", nrow(tDa[-train_tDa,]))
glm.pred[glm.probs>=.5] = "No Issue"

all(is.na(tDa$Income.per.CenTrac) == FALSE)
table(glm.pred)
table(glm.pred, tDa[-train_tDa,]$Inspection.Grade)
mean(glm.pred == tDa[-train_tDa,]$Inspection.Grade)
mean(glm.pred != tDa[-train_tDa,])
# could be interesting to specifically find out how often Issue was guessed correctly

# Compute predictions and compare to actual assessments

glm.pred = rep("Issue", nrow(tDa[-train_tDa,]))
glm.pred[glm.probs > .5] = "No Issue"
table(glm.pred, )





#
# Ordinal logistical regression
#

pDa<- na.omit(ny_inspect_dat[,c("Inspection.Grade", "Number_of_Reviews", "Income.per.CenTrac")])
pDa <-data.frame(Inspection.Grade=factor(pDa[,"Inspection.Grade"]),scale(pDa[,c("Inspection.Grade", "Number_of_Reviews", "Income.per.CenTrac")]))
train_pDa <- sample.int(n = nrow(pDa), size = floor(.75*nrow(pDa)), replace = F)

#p.glm.fits = glm(formula = as.factor(Inspection.Grade) ~ Number_of_Reviews + Income.per.CenTrac, family = multinomial, data = pDa, subset = train_pDa)

polr <- polr(formula = as.factor(Inspection.Grade) ~ Number_of_Reviews + Income.per.CenTrac, data = pDa, subset = train_pDa)

summary(polr)
coef(polr)
summary(polr)$coef

polr.probs = predict(glm.fits, tDa[-train_pDa,], type = "response")
# gives the probability that there will be an Issue
contrasts(as.factor(pDa$Inspection.Grade))
polr.pred = rep("C", nrow(pDa[-train_tDa,]))
p.glm.pred[polr.probs>=.33 && polr.probs <= .66] = "B"
p.glm.pred[polr.probs>=.66] = "A"

all(is.na(pDa$Income.per.CenTrac) == FALSE)
table(polr.pred)
table(polr.pred, tDa[-train_tDa,]$Inspection.Grade)
mean(polr.pred == tDa[-train_tDa,]$Inspection.Grade)
mean(polr.pred != tDa[-train_tDa,])


#####################################
#Logistical Regression with Lasso
################################

# Lasso Issue vs. No Issue

x_var <- model.matrix(Inspection.Grade~. , tDa[train_tDa,])[,-1]
y_var <- ifelse(tDa[train_tDa, "Inspection.Grade"] == "Issue", 1, 0)

cv.lasso <- cv.glmnet(x_var, y_var, alpha = 1, family = "binomial", lambda = NULL)
lasso <- glmnet(x_var, y_var, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)
coef(lasso)

x_test <- model.matrix(Inspection.Grade~. , tDa[-(train_tDa),])[,-1]
lasso.probabilities <- lasso %>% predict(newx = x_test)
lasso.predicted.classes <- ifelse(lasso.probabilities > 0.5, "Issue", "No Issue")

#Accuracy of model
observed.classes <- tDa[-(train_tDa), "Inspection.Grade"]
mean(predicted.classes == observed.classes)

#
# Lasso Ordinal logistic regression
#

