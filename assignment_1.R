install.packages("rlang")
library(rlang)

data1 <- read.csv(file.choose(), header = TRUE)

install.packages("dplyr")
library(dplyr)

attach(data1)

summary(Attrition)
summary(Degree_Type)
summary(Attendance_Type)
summary(International_student)
summary(First_in_family)
summary(Gender)
summary(Socio_Economic_Status)
summary(Teaching._Period_Admitted)
summary(Faculty)

summary(Achieved_Credit_Points)
summary(Age)
summary(Failed_Credit_Points)
summary(GPA)
summary(OP_Score)



#Question 2
plot(Gender=='M', GPA)
#library(dplyr)

install.packages("ggplot2")
library(ggplot2)

library(dplyr)
data1 %>%
  filter(Gender=='M')%>%
  select(GPA) -> x
  summary(x)
  boxplot(x)
  
  
library(dplyr)
data1 %>%
  filter(Gender=='F')%>%
  select(GPA) -> x
  summary(x)
  boxplot(x)
  
#t test
t.test(GPA~Gender)

#Question 3
attach(data1)
plot(OP_Score, GPA)
cor(OP_Score, GPA)

#Quwstion 4
  
#choosing predictors
cor(Achieved_Credit_Points, GPA)
cor(GPA, Age)
cor(Failed_Credit_Points, GPA)
cor(OP_Score, GPA)
cor(Faculty, GPA)

#creating Linear Model
set.seed(100)
trainingRowIndex <- sample(1:nrow(data1), 0.8*nrow(data1))
#trainingsni data 
trainingData <- data1[trainingRowIndex, ]  # model training data 
testData  <- data1[-trainingRowIndex, ]   # test data 

#Build the model on training data  
lmMod <- lm(GPA ~ Achieved_Credit_Points+Failed_Credit_Points+OP_Score, data=trainingData)
summary(lmMod)

GPAPred <- predict(lmMod, testData)

#accuracy and error rate
actuals_preds <- data.frame(cbind(actuals=testData$GPA,predicteds=GPAPred))

#plot(actuals_preds$predicteds, actuals_preds$actuals)
cor(actuals_preds$actuals,actuals_preds$predicteds)

plot(lmMod)

install.packages("tidyverse")
install.packages("caret")

library(tidyverse)
library(caret)
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(GPA ~Achieved_Credit_Points, Failed_Credit_Points, Age, OP_Score, data = trainingData, method = "lm",
               trControl = train.control)

print(model)


#Question 5
str(data1)

logistic <- glm(Attrition ~ Achieved_Credit_Points, Failed_Credit_Points,International_student, GPA, Socio_Economic_Status, Faculty, data=trainingData, family="binomial")
summary(logistic)

AttritionPred <- predict(logistic, testData)

#accuracy and error rate
actuals_preds <- data.frame(cbind(actuals=testData$Attrition,predicteds=AttritionPred))

#plot(actuals_preds$predicteds, actuals_preds$actuals)
cor(actuals_preds$actuals,actuals_preds$predicteds)

ll.null <- logistic$null.deviance/-2 
ll.proposed <- logistic$deviance/-2 
(ll.null - ll.proposed) / ll.null
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)
AIC(logistic)
BIC(logistic)