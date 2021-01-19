bank <- read.csv("~/R/Project Data/Final Project Data/Bank/bank-additional/bank-additional-full.csv", 
                 sep = ";")

library(dplyr)
library(factoextra)
library("e1071")
bank = na.omit(bank)

# data preparation NOTE: copied from kimi and yolanda
bank <- bank %>% mutate(marital = ifelse(as.character(marital) =="married", 1, 0))
bank <- bank %>% mutate(housing = ifelse(as.character(housing) =="yes", 1, 0))
bank <- bank %>% mutate(loan = ifelse(as.character(loan) =="yes", 1, 0))
# bank <- bank %>% mutate(y = ifelse(as.character(y) =="yes", 1, 0))
bank$job <- as.numeric(factor(bank$job, levels = as.character(unique(bank$job))))
bank$education <- as.numeric(factor(bank$education, levels = as.character(unique(bank$education))))
bank$default <- as.numeric(factor(bank$default, levels = as.character(unique(bank$default))))
bank$contact <- as.numeric(factor(bank$contact, levels = as.character(unique(bank$contact))))
bank$month <- as.numeric(factor(bank$month, levels = as.character(unique(bank$month))))
bank$day_of_week <- as.numeric(factor(bank$day_of_week, levels = as.character(unique(bank$day_of_week))))
bank$poutcome <- as.numeric(factor(bank$poutcome, levels = as.character(unique(bank$poutcome))))
bank$y <- factor(bank$y)

# train & test set
set.seed(1)
dataset.size = floor(nrow(bank)*0.70)
index <- sample(1:nrow(bank), size = dataset.size)
training <- bank[index,]
test <- bank[-index,]

# cross validation for svm
set.seed(1)
svm_tuned <- tune(svm, y~., data=training, tunecontrol = tune.control(cross=10))
summary(svm_tuned)
svm_cvd <- svm_tuned$best.model

svm_cvd_result <- table(test$y, predict(svm_cvd, test[,1:20], type = "response"))
sum(diag(svm_cvd_result))/(sum(svm_cvd_result))

# isolation forest
bank1 <- read.csv("~/R/Project Data/Final Project Data/Bank/bank-additional/bank-additional-full.csv", sep=";")
bank1 = na.omit(bank1)
library(isotree)
# fit isolation forest model
set.seed(1)
iso <- isolation.forest(bank1, ntrees = 100, output_score = TRUE)
iso
# count outliers
sum(iso$scores >= 0.5)
# get percentage of data that is outliers
sum(iso$scores >= 0.5)/dim(bank1)[1]
