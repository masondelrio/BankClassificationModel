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

set.seed(1)
#Load Data
setwd("/Users/masondelrio/Desktop/Fall 2020 Files/Fall 2020 Handouts/STA 141A/STA 141A Datasets")
bank.data = read.csv("bank-additional-full.csv", sep = ";")

#Index the Data into Train and Test
index = sample(1:nrow(bank.data), 0.7*nrow(bank.data))


train = bank.data[index,] #create training data
test = bank.data[-index,] #create test data
dim(train)
dim(test)
#Fit model
glm_model <- glm(y~., family = binomial, data = train)
glm_model
#Set-up Confusion Matrix
glm_prob <- predict.glm(glm_model, test, type = "response")
glm_predict<- rep("no", nrow(test))
glm_predict[glm_prob>.5] <- "yes"
#Confusion Matrix
table(pred=glm_predict, true = test$y)
#Accuracy
mean(glm_predict == test$y)

#Start Lasso
x <- model.matrix(y~., train)

y<- ifelse(train$y == "yes", 1,0)
#Cross Validation for lambda parameter
cv.out <- cv.glmnet(x,y,alpha=1, family="binomial", type.measure = "mse")
#Show log plot
plot(cv.out)
#Minimum lambda
lambda_min <- cv.out$lambda.min
#Lambda value that gives simplest model but also lies within one standard error of the optimal value of lambda.
lambda_1se <- cv.out$lambda.1se

#Show coefficients of lassomodel
coef(cv.out, s = lambda_1se)
#Set up Confusion Matrix
x_test <- model.matrix(y~., test)
lasso_prob <- predict(cv.out, newx = x_test, s = lambda_1se, type = "response")
lasso_predict<- rep("no", nrow(test))
lasso_predict[lasso_prob>.5]<- "yes"
#confusion matrix
table(pred= lasso_predict, true = test$y)

mean(lasso_predict == test$y)

##Following shows logistic model before lasso and after lasso. 
true
pred     no   yes
no  11632  1009
yes   332   591
> mean(glm_predict == test$y)
[1] 0.9011354 #Accuracy of Log Model

> table(pred= lasso_predict, true = test$y)
true
pred     no   yes
no  11706  1066
yes   258   534
> mean(lasso_predict == test$y) #accuracy of the Lasso Log Model
[1] 0.9023887


