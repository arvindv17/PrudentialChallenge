###################################################
# Mid term Assignment
# Big Data Systems and Intelligence Analytics
# Group 
#Build a linear regression model
###################################################

install.packages("ISLR")
install.packages("leaps")

library(ISLR)
library(leaps)


#set working directory

setwd("E:/Northeastern/NEU/Semester 4/Big Data Analytics/Assignments/Midterm Project")


#Read raw CSV
prudentialCleansedData<- read.csv("preprocess.csv", header = TRUE)

#Split the data into Training and Test. 
sample_size <- floor(0.90 * nrow(prudentialCleansedData))

#Set the seed to make your partition reproductible
set.seed(111)
train_ind <- sample(seq_len(nrow(prudentialCleansedData)), size = sample_size)

#Split the data into training and testing
trainingData <- prudentialCleansedData[train_ind, ]
testData <- prudentialCleansedData[-train_ind, ]


#############################################################################
# Taking too much time.
# Code for Feature Selection

require(leaps)
##### Searching all subset models up to size number of variables
regfit.full=regsubsets (Response~. ,data=testData ,nvmax=8,really.big = TRUE)
exhaustiveRegSummary =summary(regfit.full)
print(exhaustiveRegSummary)
names(exhaustiveRegSummary)
exhaustiveRegSummary$rss
exhaustiveRegSummary$adjr2
############################################################################



## Plotting and choosing the subset
par(mfrow=c(2,2))
plot(exhaustiveRegSummary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")
plot(exhaustiveRegSummary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")


require(leaps)
#### Forward selection #########################################################
regfit.fwd=regsubsets(Response~. ,data=testData ,nvmax=8, method="forward") 
forwardSearchSummary=summary(regfit.fwd)
names(forwardSearchSummary)
forwardSearchSummary
forwardSearchSummary$rss
forwardSearchSummary$adjr2
coef(regfit.fwd,6)
################################################################################


#Fit a linear regression model 
lm.fit = lm(Response~., data = trainingData)

#Summary of the fit
summary(lm.fit)

## Validation for the regression Model
#RegressionOutput file created with summary of lm fit values
fileName <- "RegressionOutputs.csv"
resultDataframe <- prudentialCleansedData[1,1]
resultDataframe <- rbind(resultDataframe, as.data.frame(summary(lm.fit)$coef[,1]))
write.table(resultDataframe, file = fileName, sep = ",", col.names = FALSE)


#Measures of predictive accuracy
#install.packages("forecast")
library(forecast)
pred = predict(lm.fit, testData)
pred1 = as.data.frame(pred)

#calculating accuracy
performanceMetrics <- accuracy(pred, testData$Response)

#creating a transpose to swap row and column
t(performanceMetrics)

#PerformanceMetrics file created with performance metrics like MAE, MAPE, RMSE
fileName <- "Outputs/PerformanceMetrics.csv"
write.table(t(performanceMetrics), file = fileName, sep = ",", row.names = TRUE, col.names = FALSE)
/

##dropterm and model -- feature selection

mod1<-lm(Response~., data=trainingData)
summary(mod1)
dropterm(mod1,testData="F")

summary(mod1)
pred=predict(mod1, testData)
accuracy(pred, trainingData$Response)
View(prudentialCleansedData)
attach(prudentialCleansedData)


##checking linear regression
cor(Response, Id)
plot(prudentialCleansedData$Id,prudentialCleansedData$Response)
modPred<-lm(Response~m., data=trainingData)
summary(modPred)
abline(modPred)

## Creating a new data frame with those values
new_train.data <- data.frame(prudentialCleansedData[,c("Product_Info_4","Ins_Age","Ht","Wt","BMI","Family_Hist_2","Family_Hist_3","Family_Hist_4","Family_Hist_5","Medical_History_1","Medical_History_15","Medical_History_24","Medical_Keyword_2","Medical_Keyword_3","Medical_Keyword_6","Medical_Keyword_9","Medical_Keyword_15","Medical_Keyword_19","Medical_Keyword_22","Medical_Keyword_25","Medical_Keyword_26","Medical_Keyword_29","Medical_Keyword_33","Medical_Keyword_34","Medical_Keyword_37","Medical_Keyword_38","Medical_Keyword_39","Medical_Keyword_41","Medical_Keyword_45","Product_Info_6","InsuredInfo_2","InsuredInfo_5","InsuredInfo_6","InsuredInfo_7","Insurance_History_1","Medical_History_2","Medical_History_4","Medical_History_22","Medical_History_27","prudentialData.cat.InsuredInfo_3_1","prudentialData.cat.Product_Info_2_2","prudentialData.cat.Product_Info_2_5","prudentialData.cat.Product_Info_2_6","prudentialData.cat.Product_Info_2_7","prudentialData.cat.Product_Info_2_8","prudentialData.cat.Product_Info_2_11","prudentialData.cat.Product_Info_2_12","prudentialData.cat.Product_Info_2_15","prudentialData.cat.Product_Info_2_16","prudentialData.cat.Product_Info_2_18","Insurance_History_2_1","Insurance_History_3_1","Medical_History_3_2","Medical_History_7_1","Medical_History_11_2","Medical_History_12_2","Medical_History_13_1","Medical_History_17_2","Medical_History_23_1","Medical_History_29_1","Medical_History_30_2","Medical_History_31_1","Medical_History_35_1","Medical_History_39_1","Medical_History_40_1","Medical_History_41_1","Response")])

dim(new_train.data)
#Creating a new regression model from the important attributes
new_model.fit <- lm(new_trainingData$Response~.,data = new_trainingData)


#Split the data into Training and Test. 
sample_size <- floor(0.90 * nrow(new_train.data))

#Set the seed to make your partition reproductible
set.seed(111)
train_ind <- sample(seq_len(nrow(new_train.data)), size = sample_size)

#Split the data into training and testing
new_trainingData <- new_train.data[train_ind, ]
new_testData <- new_train.data[-train_ind, ]

new_predict <- predict(new_model.fit, new_testData)

cor(new_predict,pred)


write.csv(new_train.data,file = "RegressionCleaned.csv")

#######################################
##                                  
#              SVM
#
#######################################
library(e1071)

## Splitting the 67 column dataset into 10% as SVM is  high processor consuming

#Split the data into Training and Test. 
sample_size <- floor(0.1 * nrow(new_train.data))

#Set the seed to make your partition reproductible
set.seed(111)
train_svm <- sample(seq_len(nrow(new_train.data)), size = sample_size)

#Split the data into training and testing
svm_train <- new_train.data[train_svm, ]
svm.test <- new_train.data[-train_svm, ]

# Building the SVM model
svm.fit <- svm(Response~.,data = svm_train)
summary(svm.fit)

#Predicting the SVM Model
predict.svm <- predict(svm.fit,svm.test)

# Trying to find the best tune of the SVM (Not very useful)
svm.tune <- best.tune(svm,Response~.,data = svm_train,kernel="polynomial")

## Trying to tune the result
tuneResult <- tune(svm, Response~.,data = svm_train,ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
