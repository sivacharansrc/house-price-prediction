#### Setting up the Environment ####
rm(list = ls())
options(scipen = 999)
library(dplyr)
library(summaryR)
library(reshape2)


#### Reading the data set ####

train <- read.csv("./Source/train.csv", header = T, check.names = F)
# head(train) summary(train) str(train) View(summaryR(train))

train <- data.table(train)

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

train <- within(train, {
  # Relabel MSSubClass
  MSSubClass.Cat <- factor(MSSubClass, levels = 1:16, labels = c("1-STORY 1946 & NEWER ALL STYLES",
                                                                 "1-STORY 1945 & OLDER",
                                                                 "1-STORY W/FINISHED ATTIC ALL AGES",
                                                                 "1-1/2 STORY - UNFINISHED ALL AGES",
                                                                 "1-1/2 STORY FINISHED ALL AGES",
                                                                 "2-STORY 1946 & NEWER",
                                                                 "2-STORY 1945 & OLDER",
                                                                 "2-1/2 STORY ALL AGES",
                                                                 "SPLIT OR MULTI-LEVEL",
                                                                 "SPLIT FOYER",
                                                                 "DUPLEX - ALL STYLES AND AGES",
                                                                 "1-STORY PUD (Planned Unit Development) - 1946 & NEWER",
                                                                 "1-1/2 STORY PUD - ALL AGES",
                                                                 "2-STORY PUD - 1946 & NEWER",
                                                                 "PUD - MULTILEVEL - INCL SPLIT LEV/FOYER",
                                                                 "2 FAMILY CONVERSION - ALL STYLES AND AGES"
  ))
})

train[1:20,.(MSSubClass, MSSubClass.Cat)]
