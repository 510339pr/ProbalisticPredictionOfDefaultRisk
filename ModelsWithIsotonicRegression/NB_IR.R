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
library(tidyr)
library(reshape2)
library(ggplot2)
library(SpecsVerification)
library('DescTools')

source('./isoReg.R')
source('./evMethods.R')

# create objects x which holds the predictor variables and y which holds the response variables
x = training[]
y = training$Y

fit <- train(x[,-21],y, method = "naive_bayes",BtrControl = trainControl(method = "cv", number = 10))
default_pred_prob <- predict(fit, newdata = testing, type = "prob")
default_pred_label <-  predict(fit, newdata = testing, type = "raw")
nb_iso_recal = iso_recal_func(fit, validate, default_pred_prob[,2], testing)


resp_Evaluation = as.logical(testing$Y) #fill in the true parameter of default
# nb_iso_recal to evaluate recalibrated forecasts
pred_Evaluation = nb_iso_recal

# # # Implementing Evaluation Metrics # # # 

# # Brier Score # # 
Brier_NB <- BrierScore(resp_Evaluation, pred_Evaluation, scaled = FALSE)

# # Spieglhalter Z test # #  
Spiegel_z_NB <- Spiegelhalter_z(resp_Evaluation, pred_Evaluation)

# # log score # # 
Log_Score_NB = logs_binom(resp_Evaluation, 1, pred_Evaluation)
Mean_LogScore_NB = mean(Log_Score_NB)

# # Cox intercept & slope # # 
CoxSlope_Intercept_NB = cox_first_degree(resp_Evaluation, pred_Evaluation)

# # AUROC # #  
par(pty= "s") # Shape plot 
AUROC_NB = roc(as.numeric(resp_Evaluation), pred_Evaluation, plot = TRUE)

# # Hosmer Lemeshow # # 
HL_C_NB = hosmer_lemeshow(resp_Evaluation, pred_Evaluation, g=20 , "C")
HL_H_NB = hosmer_lemeshow(resp_Evaluation, pred_Evaluation, g=20 , "H")

# # reliability diagram # # 
Reliability_diagram_NB = ReliabilityDiagram(
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

