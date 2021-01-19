# Final Project
# SVM

setwd("/Users/kimi/Desktop/2020 Fall Quarter/STA 141 Fundamentals of Statistical Data Science/Final Project/Final Project Data/Bank")
bank <- read.table("./bank-additional/bank-additional-full.csv", head = T, sep = ";")
head(bank)
library(dplyr)
library(factoextra)
library("e1071")
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

# train&test set
head(bank)
dataset.size = floor(nrow(bank)*0.70)
index <- sample(1:nrow(bank),size= dataset.size)
training <- bank[index,]
test <- bank[-index,]
dim(test)

# SVM
SVM <- svm(y~age+job+marital+education+housing+loan+contact+month+day_of_week+duration
           +campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx
           +euribor3m+nr.employed, data = training, importance= T,type="C-classification")
SVM
result <- data.frame(test$y, predict(SVM,test[,1:20],type = "response"))
table(result)
(10687+553)/(10687+248+869+553)
