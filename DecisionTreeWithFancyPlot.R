## Installing the required packages and libraries
library(caret)
library(e1071)
library(rpart)
#install.packages("ROCR")
#install.packages("accuracy")
#install.packages("tree")
library(ROCR)
library(rpart.plot)
## used for ctree function
library(party)
cleanedata <- read.csv("D:\\NEU\\big data sys\\assignments\\mid term proj\\a\\preprocess.csv")
## Analyzing the structure of the data set and summary
## data.frame':
str(cleanedata)
summary(cleanedata)
#convert categorical variables as factors- as decision tree needs them in the form of factors
cleanedata$Response = as.factor(cleanedata$Response)
##Obtaining the number of rows and columns, 1000 and 62 respectively
dim(cleanedata)
#Let us now create our training/calibration-Top 10 percent of the dataset  
sample_index =  sample(1:nrow(cleanedata), size=0.1*nrow(cleanedata))
test_dataset = cleanedata[sample_index,]
dim(test_dataset)
View(test_dataset)
train_dataset = cleanedata[-sample_index,]
dim(train_dataset)

##Use the rpart function to on the traindatset 
treemodel <- rpart(Response~., data = train_dataset, method="class", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))
#Visualization of the tree 
# display the results
printcp(treemodel)
# visualize cross-validation results
plotcp(treemodel) 
# Detailed summary of the splits
summary(treemodel) 
prp(treemodel,type=2,extra=1)
## Plot the classification/Decision tree
plot(treemodel, uniform=TRUE, main="Decision Tree")
text(treemodel, use.n=TRUE, all=TRUE, cex=.8)
#Different plots of the Decision tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
#view1
prp(treemodel, faclen = 0, cex = 0.8, extra = 1)
#view2 -Displays the total count at each node
tot_count <- function(x, labs, digits, varlen)
{paste(labs, "\n\nn =", x$frame$n)}
prp(treemodel, faclen = 0, cex = 0.8, node.fun=tot_count)
#view3- fancy Plot
fancyRpartPlot(treemodel)
##Predicting on the test datset using the built tree
## The table/ confusion matrix to obtain the accuracy of the predicted model 
pred_ctree <- predict(treemodel, type="prob",test_dataset)[,2]
#pred_a<-prediction(pred_ctree,test_dataset$Response)
## perf_a <- performance(pred_a,"tpr","fpr")
# AUC_arbre =performance(pred_a, measure = "auc")@y.values[[1]]
# cat("AUC: ",AUC_arbre,"\n")
# plot(perf_a)
## Table for the tree before pruning and predidicted classification tree
table(pred_ctree,test_dataset$Response)

##Pruning the tree to eiminate the overfiiting problem
##To obtain the optimum cp value that can used later for pruning the tree 
cp <- treemodel$cptable[which.min(treemodel$cptable[,"xerror"]),"CP"]
cp

##Pruning the Model and predicting on the test data
prune_ctree <- prune(treemodel, cp=cp)
##Plottin the pruned classification tree
plot(prune_ctree, uniform=TRUE, main="Pruned Decision Tree")
text(prune_ctree, use.n=TRUE, all=TRUE, cex=.6)
## Predicting using the pruned tree with the predict function 
pred_prunectree <- predict(prune_ctree, type="prob",test_dataset)[,2]
# pred_p<-prediction(pred_prunectree,test_dataset$Response)
# perf_prunectree <- performance(pred_p,"tpr","fpr")
# AUC_arbre =performance(pred_p, measure = "auc")@y.values[[1]]
# cat("AUC: ",AUC_arbre,"\n")
# plot(perf_prunectree)
## Table for the pruned and predidicted classification tree
table(pred_prunectree, test_dataset$Response)

##Building classification tree using the ctree function
ctreemodel <-ctree(Response~.,data=train_dataset)
print(ctreemodel)
plot(ctreemodel)
plot(ctreemodel,type="simple")
resultdfr <- as.data.frame(do.call("rbind", treeresponse(ctreemodel, newdata = test_dataset)))
test_dataset$tscore3<-resultdfr[,2]
predctree <-prediction(test_dataset$tscore3,test_dataset$Class)
perfctree <- performance(predctree,"tpr","fpr")
plot(perf_a,col='red',lty=1,main='Normal Decision Tree VS Pruned Tree vs Ctree');
plot(perf_prunectree,col='green',add=TRUE,lty=2)
plot(perfctree, col='blue',add=TRUE,lty=3);
legend(0.6,0.6,c('simple tree','Prune Tree','Ctree'),col=c('red','green', 'blue'),lwd=3)

##install.packages("C50")
###Using C5.0 function
##treec5<-C5.0(train_dataset)
##plot(treec5)

###Confusion Matix of normal tree modl and pruned model
conf.matrix <- table(treemodel$Response, predict(pred_prunectree,type="prob"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)