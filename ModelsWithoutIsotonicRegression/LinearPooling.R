source("LoglikelihoodFunction.R")
# inputs

thetastart = c(1,1,1/6,1/6,1/6,1/6,1/6,1/6)
p_LR = as.vector(p_LR)
p_RF = as.vector(p_RF)

p_LR_val = p_LR[1:4045]
p_LR_test = p_LR[4046:8091]

p_KNN_val = p_KNN[1:4045]
p_KNN_test = p_KNN[4046:8091]

p_NB_val = p_NB[1:4045]
p_NB_test = p_NB[4046:8091]

p_RF_val = p_RF[1:4045]
p_RF_test = p_RF[4046:8091]

p_SVM_val = p_SVM[1:4045]
p_SVM_test = p_SVM[4046:8091]

p_ANN_val = p_ANN[1:4045]
p_ANN_test = p_ANN[4046:8091]

y_original_val = y_original[1:4045]
y_original_test = y_original[4046:8091]


training_LR = as.vector(p_LR_train)
training_KNN = p_knn_train
training_NB = p_NB_train
training_RF = as.vector(p_RF_train)
training_SVM = p_SVM_train
training_ANN = p_ANN_train


# optimazation function 
set.seed(1)
ContrOptimal = 
  constrOptim(f = log_like,
            theta = thetastart,
            method = "Nelder-Mead",
            ui =rbind(c(0,0,1,1,1,1,1,1), c(0,0,-1,-1,-1,-1,-1,-1), c(0,0,1,0,0,0,0,0), c(0,0,0,1,0,0,0,0),c(0,0,0,0,1,0,0,0),c(0,0,0,0,0,1,0,0),c(0,0,0,0,0,0,1,0),c(0,0,0,0,0,0,0,1)),
            ci = c(1,-1,0,0,0,0,0,0) - 1e-6,
            x1 = p_LR_val,
            x2 = p_KNN_val,
            x3 = p_NB_val,
            x4 = p_RF_val,
            x5 = p_SVM_val,
            x6 = p_ANN_val,
            y = y_original_val,
            )

# amount forecasts
probfromLOP = rep(0, NROW(y_original_test))
for (j in 1:NROW(y_original_test)) {
  probfromLOP[j] = pbeta(ContrOptimal$par[3]*p_LR_test[j] + ContrOptimal$par[4] * p_KNN_test[j]+ ContrOptimal$par[5] * p_NB_test[j] + ContrOptimal$par[6] * p_RF_test[j] + ContrOptimal$par[7] * p_SVM_test[j] + ContrOptimal$par[8] * p_ANN_test[j],
                         ContrOptimal$par[1], 
                         ContrOptimal$par[2])
}

# # Brier Score # # 
brierScore(probfromLOP, y_original_test)

# # Calculate Spieglhalter Z test # # 
Spiegelhalter_z(y_original_test, probfromLOP)

# # Cox intercept & slope # # 
CoxSlope_Intercept = cox_first_degree(y_original_test, probfromLOP)

# # Hosmer Lemeshow # # 
HL = hosmer_lemeshow(y_original_test, lrprobpredict, g=20 , "H")

# # log score # # 
logs_binom(y_original_test, 1, probfromLOP) #size is number of trials of binomial distibution
Log_Score = logs_binom(y_original_test, 1, probfromLOP)
Mean_LogScore = mean(Log_Score)
Mean_LogScore

# # Auroc # # 
par(pty= "s") #zet plot frame naar een vierkant
AUROC = roc(y_original_test, probfromLOP, plot = TRUE)

# # reliability diagram # # 
ReliabilityDiagram(
  probfromLOP,
  y_original_test,
  bins = 10,
  nboot = 500,
  plot = TRUE,
  plot.refin = FALSE,
  #cons.probs = NA,
  attributes = FALSE,
  handle.na = c("na.fail", "use.pairwise.complete")
)


