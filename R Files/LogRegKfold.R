#K-Fold Cross Validation in R#
#
#Using Logistic Regression


#Load Data
setwd("/Users/masondelrio/Desktop/Fall 2020 Files/Fall 2020 Handouts/STA 141A/STA 141A Datasets")
bank = read.csv("bank-additional-full.csv", sep = ";")
str(bank)


#Install caret function

library(caret)
require(dplyr)

#Partition Data: Create index matrix of selected values
set.seed(1)


# Create index matrix
bank = na.omit(bank)
# data preparation
bank <- bank %>% mutate(marital = ifelse(as.character(marital) =="married", 1, 0))
bank <- bank %>% mutate(housing = ifelse(as.character(housing) =="yes", 1, 0))
bank <- bank %>% mutate(loan = ifelse(as.character(loan) =="yes", 1, 0))
bank <- bank %>% mutate(y = ifelse(as.character(y) =="yes", 1, 0))
bank$job <- as.numeric(factor(bank$job, levels = as.character(unique(bank$job))))
bank$education <- as.numeric(factor(bank$education, levels = as.character(unique(bank$education))))
bank$default <- as.numeric(factor(bank$default, levels = as.character(unique(bank$default))))
bank$contact <- as.numeric(factor(bank$contact, levels = as.character(unique(bank$contact))))
bank$month <- as.numeric(factor(bank$month, levels = as.character(unique(bank$month))))
bank$day_of_week <- as.numeric(factor(bank$day_of_week, levels = as.character(unique(bank$day_of_week))))
bank$poutcome <- as.numeric(factor(bank$poutcome, levels = as.character(unique(bank$poutcome))))


#Don't want list, we get Matrix. 
index <- createDataPartition(bank$y, p = .7, list = FALSE, times = 1)

#Convert Dataframe bank to actual df
bank <- as.data.frame(bank)
# create train dataframe and test dataframe
train_df <- bank[index,]
test_df <- bank[-index,]

#Relabel values of y to factor (1 = yes, 0 = no), because Logistic Regression needs factors for the outcome
train_df$y[train_df$y == 1] <- "yes"
train_df$y[train_df$y == 0] <- "no"

test_df$y[test_df$y == 1] <- "yes"
test_df$y[test_df$y == 0] <- "no"

#Convert outcome variable to type factor with function

train_df$y <- as.factor(train_df$y)
test_df$y <- as.factor(test_df$y)
class(train_df$y)
class(test_df$y)

#Specify type of method and number of folds
control <- trainControl(method = "cv", number = 10, savePredictions = "all",
                        classProbs = TRUE)
#Set random seed for folds
set.seed(1)

#Specify logistic regression model

model1 <- train(y ~., data = train_df, method = 'glm', family = binomial,
                trControl = control)
print(model1)

rfmodel <- train(y ~., data = train_df, method = 'rf', family = binomial,
                        trControl = control)
#Accuracy is 0.909
#Kappa accounts for null model, if we removed all features from training model, what was our baseline?
#Kappa rules of thumb, .45 is moderate model, not substantial but moderate. 

#Output in terms of regression coefficients

summary(model1)
# education, default, contact, month, duration, pdays, emp.var.rate, cons.price.idx, cons.conf.idx are all signficant.

#Variable Importance (predictor variables)

varImp(model1)
#Shows that the duration of the phone call was most important to the response variable. 

#Apply model to test_df 


#Predict outcome using model from train_df applied to test_df

predictions <- predict(model1, newdata = test_df)

#Confusion Matrix

confusionMatrix(data= predictions, test_df$y)


#Model with just most significant variables
model2 <- train(y ~ education + default+contact+ month+duration+ pdays+emp.var.rate+cons.price.idx+cons.conf.idx, data = train_df, method = 'glm', family = binomial,
                trControl = control)
print(model2)




summary(model2)


varImp(model2)

prediction2 <- predict(model2, newdata = test_df)



confusionMatrix(data= prediction2, test_df$y)


