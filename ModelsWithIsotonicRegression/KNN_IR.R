library(class)
library(stats)
library(DescTools)
library(lattice)
library(ggplot2)
library(e1071)
library(caret)

source("evMethods.R")

knnmodel = train( Y ~ .,
                  data = training,
                  method = "knn", 
                  trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE),
                  preProcess = c("center", "scale"),
                  # tune grid is omitted to create the same model used as with the full dataset
                  tuneGrid = expand.grid(k = seq(15, 15, by = 2)) 
)

knnmodel
knnmodel$bestTune
knnmodel$finalModel

# Calculate probability forecasts of KNN model
knnprobpredict = predict(knnmodel, newdata = testing, type = "prob") 
knnprobDefault = knnprobpredict[,2] 
knnprobDefault[1:10]

# Isotonic recalibration
knn_iso_recal = iso_recal_func(knnmodel, validate, knnprobDefault, testing)
# to evaluate recalibrated forecasts
knnprobDefault = knn_iso_recal

# # Spieglhalter Z test # # 
Spiegel_z_KNN = Spiegelhalter_z(as.logical(testing$Y), knnprobDefault)

# # Cox intercept & slope # # 
CoxSlope_Intercept_KNN = cox_first_degree(as.logical(testing$Y), knnprobDefault)

# # Hosmer Lemeshow # # 
HL_C_KNN = hosmer_lemeshow(as.logical(testing$Y), knnprobDefault, g=20 , "C")
HL_H_KNN = hosmer_lemeshow(as.logical(testing$Y), knnprobDefault, g=20 , "H")

# # Brier Score # #  
Brier_KNN = BrierScore(as.logical(testing$Y), knnprobDefault, scaled = FALSE)

# # log score # # 
Log_Score_KNN = logs_binom(as.logical(testing$Y), 1, knnprobDefault)
Mean_LogScore_KNN = mean(Log_Score_KNN)

# # AUROC  # # 
par(pty= "s") # shape plot 
AUROC_KNN = roc(as.numeric(as.logical(testing$Y)), knnprobDefault, plot = TRUE)

# # reliability diagram # # 
Reliability_diagram_KNN = ReliabilityDiagram(
  knnprobDefault,
  as.numeric(as.logical(testing$Y)),
  bins = 10,
  nboot = 500,
  plot = TRUE,
  plot.refin = FALSE,
  cons.probs = 0.95,
  attributes = FALSE,
  handle.na = c("na.fail", "use.pairwise.complete")
)


