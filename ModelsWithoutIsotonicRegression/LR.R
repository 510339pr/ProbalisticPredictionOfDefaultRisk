library(class)
library(stats)
library(DescTools)
library(lattice)
library(ggplot2)
library(e1071)
library(caret)

source("evMethods.R")

set.seed(1)
modelcv = train( Y ~ .,
                 data = training,
                 method = "glm", 
                 family = binomial, 
                 trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE)
                 )
summary(modelcv)
print(modelcv)

# LR probpred gives chance on default
lrprobpredict = predict(modelcv$finalModel, newdata = testing, type = "response") 

p_LR = lrprobpredict

# # Spieglhalter # # 
Spiegel_z_LR = Spiegelhalter_z(as.logical(testing$Y), lrprobpredict)

# # Cox intercept & slope # # 
CoxSlope_Intercept_LR = cox_first_degree(as.logical(testing$Y), lrprobpredict)

# # Hosmer Lemeshow # # 
HL_C_LR = hosmer_lemeshow(as.logical(testing$Y), lrprobpredict, g=20 , "C")
HL_H_LR = hosmer_lemeshow(as.logical(testing$Y), lrprobpredict, g=20 , "H")

# # Brier score # #  
Brier_LR = BrierScore(as.logical(testing$Y), lrprobpredict, scaled = FALSE)

# # log score # # 
Log_Score_LR = logs_binom(as.logical(testing$Y), 1, lrprobpredict)
Mean_LogScore_LR = mean(Log_Score_LR)

# # Auroc # # 
par(pty= "s") # shape plot 
AUROC_LR = roc(as.numeric(as.logical(testing$Y)), lrprobpredict, plot = TRUE)

# Plot reliability diagram
Reliability_diagram_LR = ReliabilityDiagram(
  lrprobpredict,
  as.numeric(as.logical(testing$Y)),
  bins = 10,
  nboot = 500,
  plot = TRUE,
  plot.refin = FALSE,
  cons.probs = 0.95,
  attributes = FALSE,
  handle.na = c("na.fail", "use.pairwise.complete")
)




