library(data.table)
library(bit64)
library(mice)
library(lubridate)  #cool package for converting dates
library(glmnet)
library(randomForest)


#loading dataset
train <- fread("train.csv" , na.strings = c(" " , "NA" , "NaN" , "NULL"), stringsAsFactors = F)
test <- fread("test.csv" ,na.strings = c(" " , "NA" , "NaN" , "NULL"), stringsAsFactors = F )
dim(train)

#getting a first view inside the dataset
names(train)
str(train)


#creating some new variables and formatting old variables for training set
train$`City Group` <- as.factor(train$`City Group`)
train$Type <- as.factor(train$Type)

 train$`Open Date` <-  mdy(train$`Open Date`)
 train$duration <- as.POSIXct(train$`Open Date`)
 d <- Sys.Date()
 l <- as.POSIXct(d)
 
 train$time_since_opening <- NULL
 for (i in 1:137) {
   train$time_since_opening[i] <- (l - train$duration[i])
 }
 
 colnames(train)[2] <- "open_date"
 
 
 
 
 #creating some new variables and formatting old variables for test set
 
 test$`City Group` <- as.factor(test$`City Group`)
 test$Type <- as.factor(test$Type)
 
 test$`Open Date` <-  mdy(test$`Open Date`)
 test$duration <- as.POSIXct(test$`Open Date`)
 d <- Sys.Date()
 l <- as.POSIXct(d)
 
 test$time_since_opening <- NULL
 for (i in 1:137) {
   test$time_since_opening[i] <- (l - test$duration[i])
 
 }
 
 colnames(test)[2] <- "open_date"
 
#creating model using random forest 
train <- as.data.frame(train)
mrf <- randomForest(revenue~.-Id-duration-City-open_date , train )
prediction <- predict(mrf , test)


 
 


