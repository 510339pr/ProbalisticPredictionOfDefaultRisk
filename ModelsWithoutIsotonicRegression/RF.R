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
library(SpecsVerification)
library(DescTools)

source('evMethods.R')

# create objects x which holds the predictor variables and y which holds the response variables
x = training[]
y = training$Y

# random forest
set.seed(1)
classifier.rf = randomForest(formula = Y ~., data = training, ntree = 1000)

# probability prediction
our.predict.rf = predict(classifier.rf, newdata = testing, type = "prob")

p_RF = our.predict.rf

# # Brier Score # #   
resp_Evaluation = as.logical(testing$Y) # fill in the true parameter of default
pred_Evaluation = our.predict.rf[, 2] # fill in the predicted default probability  [ ,2] to only take in to account the probability on default
Brier_RF = BrierScore(resp_Evaluation, pred_Evaluation, scaled = FALSE)

# # Spiegelhalter Z test # # 
Spiegel_z_RF = Spiegelhalter_z(resp_Evaluation, pred_Evaluation)

# # log score # # 
Log_Score_RF = logs_binom(resp_Evaluation, 1, pred_Evaluation)
Mean_LogScore_RF = mean(Log_Score_RF)

# # Cox intercept & slope # # 
CoxSlope_Intercept_RF = cox_first_degree(resp_Evaluation, pred_Evaluation)

# # Auroc # #  
par(pty= "s") # shape plot
AUROC_RF = roc(as.numeric(resp_Evaluation), pred_Evaluation, plot = TRUE)

# # Hosmer Lemeshow # #
HL_C_RF = hosmer_lemeshow(resp_Evaluation, pred_Evaluation, g=20 , "C")
HL_H_RF = hosmer_lemeshow(resp_Evaluation, pred_Evaluation, g=20 , "H")

# # Reliability Diagram # # 
Reliability_diagram_RF = ReliabilityDiagram(
  pred_Evaluation,
  as.numeric(resp_Evaluation),
  bins = 10,
  nboot = 500,
  plot = TRUE,
  plot.refin = FALSE,
  cons.probs = 0.95,
  attributes = FALSE,
  handle.na = c("na.fail", "use.pairwise.complete")
)
