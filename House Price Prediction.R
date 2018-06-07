#### Setting up the Environment ####
rm(list = ls())
options(scipen = 999)
library(dplyr)
library(data.table)
library(summaryR)
library(reshape2)
library(dummies)


#### Reading the data set ####

train <- read.csv("./Source/train.csv", header = T, check.names = F)
# head(train) summary(train) str(train) View(summaryR(train)) str(train)

#### FUNCTIONS FOR PREPARING DATA #######


### Function for fixing Outliers
fixOutliers <- function(x, col, method = "mean") {
  mn <- mean(x[,col], na.rm = T)
  md <- median(x[,col], na.rm = T)
  P25 <- quantile(x[col], na.rm = T,0.25)
  P75 <- quantile(x[col], na.rm = T,0.75)
  IQR <- P75 - P25
  OutlierUpperLimit <- P75 + 1.5*(IQR)
  OutlierLowerLimit <- P25 - 1.5*(IQR)
  
  for (i in 1:nrow(x)) {
    if (method == "mean") {
      x[i,col] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, mn,x[i,col])
    }
    else {
      x[i,col] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, md,x[i,col])
    }
  }
  return(x)
}


### Function for retriving all numeric columns

getNumericCols <- function(x) {
  keepCols <- c()
for (i in 1:ncol(x)) {
  
  if (class(x[[i]]) == "integer") {
    keepCols <- c(keepCols,names(x[i]))
  }
  else if (class(x[[i]]) == "numeric") {
    keepCols <- c(keepCols,names(x[i]))
  }
  
}
  numData <- train[,keepCols]
  return(numData)
}
  
numericCols <- getNumericCols(train)


### Fixing Outliers for LotFrontage

train <- fixOutliers(train, "LotFrontage", method = "mean")

