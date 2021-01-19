# read data
bank <- read.csv("~/R/Project Data/Final Project Data/Bank/bank-additional/bank-additional-full.csv", sep=";")

library(dplyr)
library(factoextra)
library("e1071")
bank = na.omit(bank)

# data preparation NOTE: copied from kimi and yolanda
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

# train & test set
dataset.size = floor(nrow(bank)*0.70)
index <- sample(1:nrow(bank),size= dataset.size)
training <- bank[index,]
test <- bank[-index,]

# training svm models using training
# life situation: age, job, marital, education
svm_life <- svm(y~age+job+marital+education,
                data = training, importance= T,type="C-classification")
# bank related: housing loan, personal loan, credit in default
svm_bank <- svm(y~housing+loan+default,
                data = training, importance= T,type="C-classification")
# last contact related
svm_contact <- svm(y~campaign+pdays+previous+poutcome,
                   data = training, importance= T,type="C-classification")
# social and economic context attribute related
svm_econ <- svm(y~emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed, 
                data = training, importance= T,type="C-classification")

# predicting from models using test
result_life <- data.frame(test$y, predict(svm_life, test[,1:20],type = "response"));
result_bank <- data.frame(test$y, predict(svm_bank, test[,1:20],type = "response"));
result_contact <- data.frame(test$y, predict(svm_contact, test[,1:20],type = "response"));
result_econ <- data.frame(test$y, predict(svm_econ, test[,1:20],type = "response"));

# function to get (accurately classified)/(total samples)
get_acc <- function(result_temp) {
  table_temp <- table(result_temp)
  return(sum(diag(table_temp))/sum(table_temp))
}

# implement get accuracy function
life_acc <- get_acc(result_life)
bank_acc <- get_acc(result_bank)
contact_acc <- get_acc(result_contact)
econ_acc <- get_acc(result_econ)

# show accuracy
life_acc
bank_acc
contact_acc
econ_acc

# results
# > life_acc
# [1] 0.888808
# > bank_acc
# [1] 0.888808
# > contact_acc
# [1] 0.8992474
# > econ_acc
# [1] 0.8901837