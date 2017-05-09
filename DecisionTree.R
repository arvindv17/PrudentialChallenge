###################################################
# Credit Card - Mid term Assignment
# Advances in Data Sciences and Architecture
# Group 
###################################################
install.packages("forecast")
install.packages("readxl")
library(rpart)
library(readxl)

setwd("D:\\NEU\\big data sys\\assignments\\mid term proj\\a\\")

#Read cleansed data
prudentialCleanData <- read.csv("preprocess.csv")

#Split the data into Training and Test. 
sample_size <- floor(0.90 * nrow(prudentialCleanData))

#Set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(prudentialCleanData)), size = sample_size)

#Split the data into training and testing
trainingData <- prudentialCleanData[train_ind, ]
testData <- prudentialCleanData[-train_ind, ]

mycontrol = rpart.control(cp = 0, xval = 10)

# Model for decision tree
fittree = rpart(Response ~ . , method = "class",data = trainingData, control = mycontrol)
printcp(fittree)
fittree$cptable
plotcp(fittree)
summary(fittree)

cptarg = sqrt(fittree$cptable[7,1]*fittree$cptable[8,1])
prunedtree = prune(fittree,cp=cptarg)

par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
plot(prunedtree, uniform = T, compress = T, 
     margin = 0.1, branch = 0.3)
text(prunedtree, use.n = T, digits = 3, cex = 0.6)
rsq.rpart(fittree)
#Predicting on test data
fit.preds = predict(prunedtree,newdata=testData,type="class")

#Creating confusion matrix
fit.table = table(testData$Response,fit.preds)
fit.table
#Calculating error rate for the predicted values
ErrorRate <- sum(fit.table[1,2], fit.table[2,1])/ sum(fit.table)

#Using ctree
library(party)
attach(trainingData)
fit <- ctree(Response ~ ., data=trainingData)
plot(fit, main="Conditional Inference Tree for prudential")

test.pred.treeC1 <- predict(prunedtree,testData)
RMSE.treeC1.pruned <- sqrt(mean((test.pred.treeC1-testData$Response)^2))
MAE.treeC1.pruned <- mean(abs(test.pred.treeC1-testData$Response))
