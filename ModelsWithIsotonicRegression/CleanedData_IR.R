library(readxl)
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)
library(e1071)
library(scoringRules)
library(pROC)
library(BBmisc)
library(fastDummies)

# Full data cleaning procedure, cleaned_data is the cleaned data set

# reading full data set from xlsx file
data<- read_excel("DataFull.xlsx")

# Setting default variables as categorical non default and default
data$Y <- factor(data$Y, levels = c(0,1), labels = c("FALSE", "TRUE"))
# Setting other variables as categorical
data$X2 <- factor(data$X2, levels = c(1,2), labels = c("Male", "Female"))
data$X3 <- factor(data$X3, levels = c(0,1,2,3,4,5,6), labels = c("Other","Graduate", "University", "High School", "Other", "Other", "Other"))
data$X4 <- factor(data$X4, levels = c(0,1,2,3), labels = c("Other", "Married", "Single", "Divorced"))

# Creating the dummy variables
dataWithDummies <- dummy_cols(data, select_columns = c('X2', 'X3', 'X4'),
                      remove_selected_columns = TRUE, remove_first_dummy = TRUE)
names(dataWithDummies)[names(dataWithDummies) == "X3_High School"] <- "X3_High_School"



# data cleaning
intermediary_data <- dataWithDummies[!(data$X12==0 & data$X13==0 & data$X14==0 & data$X15==0  & data$X16==0 
                            & data$X17==0 & data$X18==0 & data$X19==0 & data$X20==0 & data$X21==0 & data$X22==0 & data$X23==0),]
cleaned_data <- intermediary_data[!(intermediary_data$X6<=0 & intermediary_data$X7<=0 & intermediary_data$X8<=0 & intermediary_data$X9<=0  & intermediary_data$X10<=0 
                                    & intermediary_data$X11<=0 & intermediary_data$Y==TRUE),]

set.seed(1)
indxTrain <- createDataPartition(y = cleaned_data$Y,p = 0.70,list = FALSE)
indxTraining <- indxTrain[(1:floor(length(indxTrain)*0.90))]
indxValidate <- indxTrain[((1+floor(length(indxTrain)*0.90)):length(indxTrain))]
training <- cleaned_data[indxTraining,]
validate <- cleaned_data[indxValidate,]
testing <-  cleaned_data[-indxTrain,]
