# # # loading Stuff # # # 
library(caret) # confusion matrix
library(e1071) # SVM
library(beepr)

source("evaluationMetrics.R")
source("evMethods.R")

# # # End Data processes # # # 

# # # # tuning SVM # # # 
# # Only necessary to verify that parameters for svmfit are optimal
# brierScores = c(1:4)
# costValues = c(0.1,1,10,100)
# parameterValues = c(0.1,1,10,100)
# for(i in 1:4){
#   set.seed(1)
#   brierScores[i] = tuningSVM2(data = training,nFolds = 4,c=costValues[i],paramValue = 100,kern = radial,parameter = g)
# }

# # # performing SVM # # # 
set.seed(1)
svmfit=svm(Y ~., kernal=radial, data=training, cost=10, g=1, probability=T)

# predictions
ypred=predict(svmfit,testing,probability = T)
df = data.frame(ypred,attr(ypred, "probabilities"))

y_original = as.vector(as.numeric(testing$Y)-1)
p_default = as.vector(df$TRUE.)

p_SVM = p_default

# # # Implementing Evaluation Metrics For SVM # # # 

# # Brier Score # # 
Brier_SVM = BrierScore(y_original,p_default)

# # Spiegel Halters Z Test # # 
Spiegel_z_SVM  = Spiegelhalter_z(y_original,p_default)

# # Cox intercept & slope # # 
CoxSlope_Intercept_SVM = cox_first_degree(y_original,p_default)

# # hosmer lemeshow # # 
HL_C_SVM = hosmer_lemeshow(y_original,p_default,g=10,"C")
HL_H_SVM = hosmer_lemeshow(y_original,p_default,g=10,"H")

# # log score # # 
Mean_LogScore_SVM  = mean(logs_binom(y_original,1,p_default))

# # Auroc # # 
par(pty= "s")
AUROC_SVM = roc(y_original, p_default, plot = TRUE)

# # Reliability diagram # # 
Reliability_diagram_SVM = ReliabilityDiagram(
  p_default,y_original, 
  bins = 10,
  nboot = 500,
  plot = TRUE,
  plot.refin = FALSE,
  cons.probs = 0.95,
  attributes = FALSE,
  handle.na = c("na.fail", "use.pairwise.complete")
  )
