library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(data.table)
library(MASS)
library(caret)
library(rpart)          # decision tree methodology
library(rpart.plot)     # decision tree visualization
library(randomForest)   # random forest methodology
library(gbm)   
library(class)
library(glmnet)
setwd("/Users/masondelrio/Desktop/Fall 2020 Files/Fall 2020 Handouts/STA 141A/STA 141A Datasets")

bank.data = read.csv("bank-additional-full.csv", sep = ";")
#cross validation for lasso regression#
x = model.matrix(y~., bank.data)
y = bank.data$y
# leaving only the predictors
x_train = model.matrix(y~., training)
x_test = model.matrix(y~., test)

y_train = training$y
  

y_test = test$y

CV = cv.glmnet(x = x_train, y =y_train, family = "binomial", 
               type.measure = "class", alpha = 1, nlambda = 100)

plot(CV) #left = lowest , right = highly regularized model within 1 SE of the minimal error
out = glmnet(x, y, alpha = 1, lambda = CV$lambda.1se) # Fit lasso model on full dataset
lasso_coef = predict(out, type = "coefficients", s = CV$lambda.1se)[1:20,] # Display coefficients using lambda chosen by CV
lasso_coef
fit = glmnet(x = x_train, y = y_train, family = "binomial", 
             alpha = 1, lambda = CV$lambda.1se)

#list of nonzero coefficients:
fit$beta[,1]
sigCoef = fit$beta[,1] != 0
plot(fit)
#random forest#
bank.data = read.csv("bank-additional-full.csv", sep = ";")
bank.data = na.omit(bank.data)
bank.data$y = as.factor(bank.data$y)
dataset.size = floor(nrow(bank.data)*0.70)
index <- sample(1:nrow(bank.data),size= dataset.size)
training <- bank.data[index,]
test <- bank.data[-index,]
rf <- randomForest(y~., data = training, importance= T)
rf
plot(rf)
rf$
result
#Variable of Importance, Duration ranks number 1.
varImpPlot(rf)


result <- data.frame(test$y, predict(rf,test[,],type = "response"))
rftest <- predict(rf, newdata = test)

rftest
#Using caret Package, get confusion Matrix. 91.34% Accuracy. 
confusionMatrix(data= rftest, test$y)

dim(test)

library(pROC)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf,test[,],type="prob")
auc
# Use pretty colours:
auc<- auc(test$y, prediction_for_roc_curve[,2])
plot(roc(test$y,prediction_for_roc_curve[,2]))
index1 <- sample(1:nrow(bank.data),size= dataset.size)
test <- bank.data[index1,]
plot.roc(test$y, rf$votes[,1], percent = TRUE, col= "#4daf4a", lwd = 4, print.auc = TRUE, print.auc.y=40)

length(train$y)
length(rf$votes[,1])
