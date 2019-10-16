library(randomForest)
library(tidyverse)
training_data1=read.csv("E:\\Trinity\\machine learning\\tcdml1920-income-ind\\preprocessed_training_data.csv")

View(training_data1)

set.seed(100)
l=sample(1:length(training_data1$income),floor(0.7*length(training_data1$income)))
train_data=training_data1[l,]
test_data=training_data1[-l,]
rf =randomForest(income~.,data=train_data, importance =TRUE,ntree=50,do.trace=TRUE)
?randomForest()
saveRDS(rf,"E:\\Trinity\\machine learning\\tcdml1920-income-ind//rf_1st_submission.rds")
rf
Income = predict (rf ,newdata =test)
plot(yhat.bag , test_data$income)
abline(0,1)
mean((yhat.bag-test_data$income)^2)
importance(rf)
income=as.data.frame(income)
test=cbind(test,Income)
View(income)
summary(test$income)


test=read.csv("E:\\Trinity\\machine learning\\tcdml1920-income-ind\\preprocessed_test_data.csv")
test1=subset(test,select = -Instance)
Income = predict (rf ,newdata =test1)
Income=as.data.frame(Income)
test2=cbind(test,Income)
write_csv(test2,"E:\\Trinity\\machine learning\\tcdml1920-income-ind\\12345678.csv")
write_csv(Income,"E:\\Trinity\\machine learning\\tcdml1920-income-ind\\income.csv")
