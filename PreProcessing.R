###### MIDTERM PROJECT#########################

#### DATA LOADING PROCESS ****************************************************

## Setting the working directory to the local path, to ensure that the files are 
## created in the right directory
setwd("E:/Northeastern/NEU/Semester 4/Big Data Analytics/Assignments/Midterm Project")

###Load the Dataset
## Path is based out of the local system
prudentialData <- read.csv("KaggleData/train.csv",header = TRUE,sep = ",")

## Validating the column names based on the dimension of the dataset
##  colnames(prudentialData)

## To create separate dataframes each, we are trying to create the columns first
##CATEGORICAL COLUMNS
cat.var.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Family_Hist_1", paste("Medical_History_", c(2:9, 11:14, 16:23, 25:31, 33:41), sep=""),"Response")

## Continous COLUMNS
cont.var.names <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
                    "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                    "Family_Hist_5")


## DISCRETE COLUMNS
disc.var.names <- c("Id","Medical_History_1", "Medical_History_10", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
                    paste("Medical_Keyword_", 1:48, sep=""))


## Based on the column names, we are separating them from the dataframe, based 
## on selecting only those columns

## Categorical Dataframe
prudentialData.cat <- prudentialData[, cat.var.names]

## Continous Dataframe
prudentialData.cont <- prudentialData[, cont.var.names]

## Discrete Dataframe
prudentialData.disc <- prudentialData[, disc.var.names]

## Trying to verify the dimension of the created data frames

## **************PURELY FOR VALIDATION PURPOSE **************

#dim(prudentialData.cont)
#dim(prudentialData.disc)
#dim(prudentialData.cat)

#colnames(prudentialData.cont)

#head(prudentialData.cont)
## *********************************************************


### *** Mean COMPUTATION FOR 1 VARIABLE *********************************
### DO NOT RUN THIS CODE AS THIS IS FOR 1 VARIABLE ONLY 
## FOR TEST PURPOSE ONLY *********************************
#impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))  ##replace the N/A records with mean of the day
#library(plyr)
#prudentialData.cont.1 <- ddply(prudentialData.cont, ~ Ins_Age, transform, Family_Hist_4 = impute.mean(Family_Hist_4))
#str(prudentialData.cont.1$Employment_Info_4)

################################################
## CONTINUOUS DATA PREPROCESSIG ***************************************

### Trying to create a function to update all column with Mean values

## Finding all the columns for which we need to update with the mean value
colSums(prudentialData.cont)

## Setting up with the mean value
means <- colMeans(prudentialData.cont,na.rm = TRUE)

## Looping through each column which does not have mean value
for (j in 1:ncol(prudentialData.cont)){ 
  prudentialData.cont[is.na(prudentialData.cont[, j]), j] <- means[j]
}
colSums(is.na(prudentialData.cont))

summary(prudentialData.cat)
## Validation
## Writing it to a CSV to try and validate only for a CSV file

#write.csv(prudentialData.cont,file="Continuous_final.csv")

dim(prudentialData.cont)

#### ****DISCRETE VARIABLE PREPROCESSING  ************************

## Writing it to a csv, which will contain only the discrete data

#write.csv(prudentialData.disc,file="Discrete_Initial.csv")

#Validating if there are any NA in the dataset through R

anyNA(prudentialData.disc)

# Checking the number of columns which have the value of NA
colSums(is.na(prudentialData.disc))

dim(prudentialData.disc)

## Creating a dataframe by elimiating 2 columns Medical_History_10 and
## Medical_History_32 as they have huge number of NA values in the dataset
## Medical_History_32 - 58274 and Medical_History_10 - 58824
## The total number of rows in the dataset are 59381.
## Hence it is safe to elimiate those 2 columns as more than 95% of the values
## are NA

## Applying it manually for the 3 remaining columns

## Medical_History_10
## Medical_History_24

## Medical_History_15
prudentialData.disc$Medical_History_15[is.na(prudentialData.disc$Medical_History_15)] <- median(prudentialData.disc$Medical_History_15, na.rm = TRUE)

## Medical_History_1
prudentialData.disc$Medical_History_1[is.na(prudentialData.disc$Medical_History_1)] <- median(prudentialData.disc$Medical_History_1, na.rm = TRUE)


## Medical_History_24
prudentialData.disc$Medical_History_24[is.na(prudentialData.disc$Medical_History_24)] <- median(prudentialData.disc$Medical_History_24, na.rm = TRUE)

## Validating if all the NA columns are removed
colSums(is.na(prudentialData.disc))

colnames(prudentialData.disc)

# Creating a new dataframe without the columns Medical_History_10 and
## Medical_History_32
prudentialData.disc <- prudentialData.disc[,-c(3,6)]
dim(prudentialData.disc)

## Writing it to a CSV file for final visual validation
#write.csv(prudentialData.disc,file = "Discrete_Final.csv")

## CATEGORICAL PREPROCESS STEPS *******************************

## Library for the function colMaxs
library(matrixStats)

## Writing the initial dataset into a file for visual inspection
#write.csv(prudentialData.cat,file = "categorical_initial.csv")



## Finding out the maximum value of each column.
## This is performed so that we are able to understand how many 1-N conversions
## we would be required to perform

## It is important to convert it into a matrix because
## colMaxs() can only be run for a matrix. 
## We are checking how many columns we need to split it into.
test <- data.matrix(prudentialData.cat,rownames.force = NA)

## Defining a function to convert Factors to integers
as.numeric.factor <- function(x) {seq_along(levels(x))[x]}

## The variable Product_Info_2 has been found to be a factor.
## Converting that to its corresponding numeric values
prudentialData.cat$Product_Info_2 <- as.numeric.factor(prudentialData.cat$Product_Info_2)

# Validating the change in the class
class(prudentialData.cat$Product_Info_2)

## Checking for the max value in each column
colMaxs(test)
# 2(19) 3(38) 7(38) 12(11) 25(648)

colnames(test)
## To check for ranges so that we are able to identify which all columns need to be
## split to 1-N range.
colRanges(test)

## Considering only the one categorical column
## We are selecting these 2 into 1 dataframe because they both have the 
## maximum value as 38

df <- data.frame(prudentialData.cat$Employment_Info_2,prudentialData.cat$Product_Info_3)

##categorical factor with N categories into N binary columns 

## Since the maximum size of those values are the same, so they will 
## fit properly in the model and can be separated accordingly

new_df = data.frame(row.names=rownames(df))
for (i in colnames(df)) {
  for (x in c(1:max(prudentialData.cat$Employment_Info_2))) {
    new_df[paste0(i, "_", x)] = as.numeric(df[i] == x)
  }
}
dim(new_df)

##Converting Product_Info_3 to 1-N transformation
## Performing it for Product_info_2
df1 <- data.frame(prudentialData.cat$Product_Info_2)


##categorical factor with N categories into N binary columns 
new_df1 = data.frame(row.names=rownames(df1))
for (i in colnames(df1)) {
  for (x in c(1:max(prudentialData.cat$Product_Info_2))) {
    new_df1[paste0(i, "_", x)] = as.numeric(df1[i] == x)
  }
}

dim(new_df1)
##Converting Product_Info_3 to 1-N transformation
## Performing it for InsuredInfo_3
df2 <- data.frame(prudentialData.cat$InsuredInfo_3)


##categorical factor with N categories into N binary columns 
new_df2 = data.frame(row.names=rownames(df2))
for (i in colnames(df2)) {
  for (x in c(1:max(prudentialData.cat$InsuredInfo_3))) {
    new_df2[paste0(i, "_", x)] = as.numeric(df2[i] == x)
  }
}


dim(new_df2)

### For columns which have 3 values

## These are the columns which have been identified to have values ranging from 1-3
## For this dataframe, we are trying to create 3 new columns each 
## 1-N conversion

new.categorical.frame <- prudentialData.cat[,c(6,10,18,19,20,21,22,23,24,26,28,30,31,32,33,34,35,36,37,38,39,40,41,44,45,46,48,49,50,51,53,54,55,56,58,59,60)]

dim(new.categorical.frame)

##categorical factor with N categories into N binary columns 
new_df3 = data.frame(row.names=rownames(new.categorical.frame))
for (i in colnames(new.categorical.frame)) {
  for (x in c(1:max(prudentialData.cat$Product_Info_7))) {
    new_df3[paste0(i, "_", x)] = as.numeric(new.categorical.frame[i] == x)
  }
}
dim(new_df3)
new.cat.frame <- cbind(new_df2,new_df1,new_df,new_df3)
dim(new.cat.frame)

#Column 7 - employment_info_2
#Column 10 - Insured_info_3
#Column 3 - Product_info_3
## These are the columns that are split into 1-N
prudentialData.cat <- cbind(prudentialData.cat[,-c(2,3,7,12,6,10,18,19,20,21,22,23,24,26,28,30,31,32,33,34,35,36,37,38,39,40,41,44,45,46,48,49,50,51,53,54,55,56,58,59,60)],new.cat.frame)
dim(prudentialData.cat)



## After all the data cleansing Process
## Trying to combine the complete dataset into one single dataframe

prudentialData <- cbind(prudentialData.cont,prudentialData.disc,prudentialData.cat)


write.csv(prudentialData,file = "preprocess.csv")
dim(prudentialData)


