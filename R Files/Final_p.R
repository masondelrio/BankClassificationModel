setwd("/Users/yulujin/Desktop/STA141/Final Project/Final Project Data/Bank/")
bank <- read.table("./bank-additional/bank-additional-full.csv", head = T, sep = ";")
head(bank)
library(dplyr)
library(factoextra)
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
pca <- prcomp(bank[,-21], center = TRUE,scale. = TRUE)
summary(pca)
fviz_eig(pca)
pca$sdev^2
#we would eliminate any variable that didn’t have an eigenvalue greater than 1. 
#The idea behind this is that if the eigenvalue is less than 1, 
#then the component accounts for less variance than a single variable contributed.
#Get the scree plot and determine how many pc(s) are enough to represent the data well
plot(1:(length(pca$sdev)),  (pca$sdev)^2, type='b',
     main="Scree Plot", xlab="Number of Components", ylab="Eigenvalue Size")
print(pca$rotation)
#par(pty="s")
#plot(pca$rotation[,1], pca$rotation[,2], 
     #xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
# labeling points with state abbreviations:
#n<- dim(bank)[1]
#ID <- 1:n
#text(pca$rotation[,1], pca$rotation[,2],labels=ID, cex=0.7, lwd=2)
#biplot(pca, xlim=range(-1,1),xlabs=ID)
