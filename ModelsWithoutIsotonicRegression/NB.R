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
library(DescTools)

# create objects x which holds the predictor variables and y which holds the response variables
x = training[]
y = training$Y

# create Naive Bayes model with Y as the variable to estimate and training data as training also with using Laplace transformation
set.seed(1)
fit <- train(x[,-24],y, method = "naive_bayes",BtrControl = trainControl(method = "cv", number = 10))
default_pred_prob <- predict(fit, newdata = testing, type = "prob")

p_NB = default_pred_prob

# # Brier score # #  
resp_Evaluation = as.logical(testing$Y) #fill in the true parameter of default
pred_Evaluation = default_pred_prob[, 2] #fill in the predicted default probability  [ ,2] to only take in to account the probability on default
Brier_NB <- BrierScore(resp_Evaluation, pred_Evaluation, scaled = FALSE)

# # Spiegelhalter Z test # # 
Spiegel_z_NB <- Spiegelhalter_z(resp_Evaluation, pred_Evaluation)

# # log score # # 
Log_Score_NB = logs_binom(resp_Evaluation, 1, pred_Evaluation)
Mean_LogScore_NB = mean(Log_Score_NB)

# # Cox intercept & slope # # 
CoxSlope_Intercept_NB = cox_first_degree(resp_Evaluation, pred_Evaluation)

# # Auroc # #  
par(pty= "s") # shape plot 
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
