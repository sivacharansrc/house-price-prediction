#### Setting up the Environment ####
rm(list = ls())
options(scipen = 999)
library(dplyr)
library(data.table)
library(summaryR)
library(reshape2)
library(dummies)
library(ggplot2)
library(GGally)

#### Reading the data set ####

train <- read.csv("./Source/train.csv", header = T, check.names = F)
test <- read.csv("./Source/test.csv", header = T, check.names = F)

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
  newCol <- paste0(col,"_fixed")
  if (fix == "na") {
    for (i in 1:nrow(x)) {
      if (method == "mode") {
        x[i,newCol] <- ifelse(is.na(x[i,col]), mde,x[i,col])
      }
      else if(method == "median") {
        x[i,newCol] <- ifelse(is.na(x[i,col]), md,x[i,col])
      }
      else {
        x[i,newCol] <- ifelse(is.na(x[i,col]), mn,x[i,col])
      }
    }   
  }
  else if (fix == "outliers") {
    for (i in 1:nrow(x)) {
      if (method == "mode") {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, mde,x[i,col])
      }
      else if(method == "median") {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, md,x[i,col])
      }
      else {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit, mn,x[i,col])
      }
    }     
  }
  else {
    for (i in 1:nrow(x)) {
      if (method == "mode") {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit | is.na(x[i,col]), mde,x[i,col])
      }
      else if(method == "median") {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit | is.na(x[i,col]), md,x[i,col])
      }
      else {
        x[i,newCol] <- ifelse(x[i,col] < OutlierLowerLimit | x[i,col] > OutlierUpperLimit | is.na(x[i,col]), mn,x[i,col])
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

#### Preparing Data for Linear Regression #####
# nrow(train)

test$SalePrice <- 0
dataPrep <- rbind(train,test)





linRegData <- dataPrep[,c("SalePrice", "LotFrontage", "LotArea", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd",
                 "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "Fireplaces", "1stFlrSF", "2ndFlrSF",
                 "LowQualFinSF","TotRmsAbvGrd", "GarageYrBlt", "GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", 
                 "EnclosedPorch", "3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal")]

View(summaryR(linRegData))
# write.csv(summaryR(linRegData), "./Output/SummaryStats.csv", row.names = F)

#graphics.off()
#par(mar = c(1,1,1,1))
#pairs(linRegData[,1:6])
#plotmatrix(linRegData)
#ggpairs(linRegData[,1:6])

#linRegData %>% cor %>% as.data.frame %>% rownames_to_column(var='var1') %>% gather(var2,value,-var1)

#### Fixing Missing Values ####

linRegData <- fixOutliersNAs(linRegData, "LotFrontage", fix = "na")
linRegData <- fixOutliersNAs(linRegData, "MasVnrArea", fix = "na", method = "mean")
linRegData <- fixOutliersNAs(linRegData, "BsmtFinSF1", fix = "na", method = "median")
linRegData <- fixOutliersNAs(linRegData, "BsmtFinSF2", fix = "na", method = "mean")
linRegData <- fixOutliersNAs(linRegData, "BsmtUnfSF", fix = "na", method = "median")
linRegData <- fixOutliersNAs(linRegData, "TotalBsmtSF", fix = "na", method = "median")
linRegData <- fixOutliersNAs(linRegData, "GarageCars", fix = "na", method = "median")
linRegData <- fixOutliersNAs(linRegData, "GarageArea", fix = "na", method = "mean")



# Mostly Garages are built the same year the houses are built. So, replacing missing values with the Year Built
linRegData <- mutate(linRegData, GarageYrBlt_fixed = if_else(is.na(GarageYrBlt), YearBuilt, GarageYrBlt))
linRegData <- mutate(linRegData, GarageYrBlt_fixed = if_else(GarageYrBlt_fixed == 2207,YearBuilt, GarageYrBlt_fixed))

modelingData <- linRegData[,c("SalePrice", "LotFrontage_fixed", "LotArea", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd",
                              "MasVnrArea_fixed", "BsmtFinSF1_fixed", "BsmtFinSF2_fixed", "BsmtUnfSF_fixed", "TotalBsmtSF_fixed", "Fireplaces", "1stFlrSF", "2ndFlrSF",
                              "LowQualFinSF","TotRmsAbvGrd", "GarageYrBlt_fixed", "GarageCars_fixed", "GarageArea_fixed", "WoodDeckSF", "OpenPorchSF", 
                              "EnclosedPorch", "3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal")]
modelingTrain <- modelingData[1:nrow(train),]
modelingTest <- modelingData[(nrow(train)+1):nrow(modelingData),] # nrow(modelingTest)

#samplingIndex <- sample(1:nrow(modelingTrain), size = (0.7*nrow(modelingTrain)), replace = F)
#trainData <- modelingTrain[samplingIndex,]
#testData <- modelingTrain[-samplingIndex,]

linearModel <- lm(SalePrice ~ ., data = modelingTrain)
summary(linearModel)
modelingTest$SalePrice <- predict.lm(linearModel, modelingTest, na.action = 'na.exclude')

submission <- cbind(test$Id, modelingTest$SalePrice)
write.csv(submission, "./Output/Submission.csv", row.names = F)

nrow(modelingTrain)

View(test)
