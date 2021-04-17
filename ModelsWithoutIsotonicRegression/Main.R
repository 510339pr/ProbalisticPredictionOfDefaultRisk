library(beepr)

source('./evMethods.R')
# source('./removedData.R'), use this when analyzing the data points
# that have been removed
source('./cleanedData.R')


# Running machine learning algorithms
source('./NB.R')
source('./LR.R')
source('./KNN.R')
source('./RF.R')
source('./SVM.R')
source('./ANN.R')

# Run after running everything else 
source('./LinearPooling.R')

