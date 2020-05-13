install.packages("rlang")
library(rlang)

data1 <- read.csv(file.choose(), header=TRUE)

actualdata <- data1[,c(2:15)]

#creating
set.seed(100)
trainingRowIndex <- sample(1:nrow(actualdata), 0.9*nrow(actualdata))
#trainingsni data 
trainingData <- actualdata[trainingRowIndex, ]  # model training data 
testData  <- actualdata[-trainingRowIndex, ]   # test data 


##extract 15th column of train dataset because it will be used as 'cl' argument in knn function.
price_target <- actualdata[trainingRowIndex,14]
##extract 15th column if test dataset to measure the accuracy
price_test <- actualdata[-trainingRowIndex,14]

##load the package class

install.packages("class")
library(class)

install.packages("caret")

##run knn function

install.packages("ISLR")
install.packages("MASS")
library(ISLR)
library(MASS)

install.packages("class")
library(class)

preds = FNN::knn.reg(train = trainingData[,c(1:13)], test = testData[,c(1:13)], y = price_target, k = 7)
pred_values = (preds$pred)
actual_values = (price_test)
plot(price_test, pred_values, xlab="y", ylab=expression(hat(y)))

abline(lm(price_test~pred_values), col="red")
cor(price_test, pred_values)

cor(pred_values, actual_values)

#mean absoulte percentage error
mean(abs((pred_values - actual_values))/actual_values)



install.packages("caret")
library(caret)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "RMSE"
#SVM
set.seed(7)
attach(trainingData)
fit.svm <- train(medv~., data=trainingData, method="svmRadial", metric=metric, trControl=control)

# estimate skill of SVM on the validation dataset
predictions_svm <- predict(fit.svm, testData[,c(1:13)])
#mean absoulte percentage error 
mean(abs((predictions_svm - price_test))/price_test)


plot(price_test, predictions_svm, xlab="y", ylab=expression(hat(y)))
abline(lm(price_test~predictions_svm), col="red")
cor(price_test, predictions_svm)


library(caret)
fit.rf <- train(medv~., data=trainingData, method="rf", metric=metric, trControl=control)

# estimate skill of Rain Forest on the validation dataset
predictions_rf <- predict(fit.rf, testData[,c(1:13)])
#mean absoulte percentage error
mean(abs((predictions_rf - price_test))/price_test)

plot(price_test, predictions_rf, xlab="y", ylab=expression(hat(y)))
abline(lm(price_test~predictions_rf), col="red")
cor(price_test, predictions_rf)

#variance of price
var(actualdata$medv)

#interpretation

plot(actualdata$rm, actualdata$medv)
abline(lm(actualdata$medv~actualdata$rm), col="red")

plot(actualdata$lstat, actualdata$medv)
abline(lm(actualdata$medv~actualdata$lstat), col="red")

plot(actualdata$ptratio, actualdata$medv)
abline(lm(actualdata$medv~actualdata$ptratio), col="red")

