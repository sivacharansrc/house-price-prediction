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


### Function for fixing Outliers, and NA's
fixOutliersNAs <- function(x, col, method = "mean", fix = "both") {
  
  ## MODE INFO ORIGINAL FUNCTION
  modeInfo <- function(y) {
    modeData <- unique(y)
    modeData <- modeData[!is.na(modeData)]
    modeData[which.max(tabulate(match(y, modeData)))]
  }
  mn <- mean(x[,col], na.rm = T)
  md <- median(x[,col], na.rm = T)
  mde <- modeInfo(x[,col])
  P25 <- quantile(x[col], na.rm = T,0.25)
  P75 <- quantile(x[col], na.rm = T,0.75)
  IQR <- P75 - P25
  OutlierUpperLimit <- P75 + 1.5*(IQR)
  OutlierLowerLimit <- P25 - 1.5*(IQR)
  if (fix == "na") {
    for (i in 1:nrow(x)) {
      if (method == "mode") {
        x[i,col] <- ifelse(is.na(x[i,col]), mde,x[i,col])
      }
      else if(method == "median") {
        x[i,col] <- ifelse(is.na(x[i,col]), md,x[i,col])
      }
      else {
        x[i,col] <- ifelse(is.na(x[i,col]), mn,x[i,col])
      }
    }   
  }
  else if (fix == "outliers") {
    for (i in 1:nrow(x)) {
      if (method == "mode") {
        x[i,col] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, mde,x[i,col])
      }
      else if(method == "median") {
        x[i,col] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, md,x[i,col])
      }
      else {
        x[i,col] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, mn,x[i,col])
      }
    }     
  }
  else {
    for (i in 1:nrow(x)) {
      if (method == "mode") {
        x[i,col] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit | is.na(x[i,col]), mde,x[i,col])
      }
      else if(method == "median") {
        x[i,col] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit | is.na(x[i,col]), md,x[i,col])
      }
      else {
        x[i,col] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit | is.na(x[i,col]), mn,x[i,col])
      }
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
  
#numericCols <- getNumericCols(train)




linRegData <- train[,c("SalePrice", "LotFrontage", "LotArea", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd",
                 "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "Fireplaces", "1stFlrSF", "2ndFlrSF",
                 "LowQualFinSF","TotRmsAbvGrd", "GarageYrBlt", "GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", 
                 "EnclosedPorch", "3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal")]


