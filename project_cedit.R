# Basic libarry
library(ranger)
library(caret)
library(data.table)
#lord data
creditcard_data <- read.csv("creditcard.csv")
View(creditcard)
#Data Exploration
dim(creditcard_data)
head(creditcard_data,6)
tail(creditcard_data,6)
table(creditcard_data$Class)
summary(creditcard_data$Amount)
names(creditcard_data)
var(creditcard_data$Amount)
sd(creditcard_data$Amount)
#Data Manipulation
head(creditcard_data)
creditcard_data$Amount=scale(creditcard_data$Amount)
NewData=creditcard_data[,-c(1)]
head(NewData)
#Data Modeling
library(caTools)
set.seed(123)
data_sample = sample.split(NewData$Class,SplitRatio=0.80)
train_data = subset(NewData,data_sample==TRUE)
test_data = subset(NewData,data_sample==FALSE)
dim(train_data)
dim(test_data)
#Fitting Logistic Regression Model
Logistic_Model=glm(Class~.,test_data,family=binomial())
summary(Logistic_Model)
plot(Logistic_Model)
#ROC curve
library(pROC)
lr.predict <- predict(Logistic_Model,train_data, probability = TRUE)

auc.gbm = roc(test_data$Class, lr.predict, plot = TRUE, col = "blue")
# decision tree model
library(rpart) 
library(rpart.plot)
decisionTree_model <- rpart(Class ~ . , creditcard_data, method = 'class')
predicted_val <- predict(decisionTree_model, creditcard_data, type = 'class')
probability <- predict(decisionTree_model, creditcard_data, type = 'prob')
rpart.plot(decisionTree_model)