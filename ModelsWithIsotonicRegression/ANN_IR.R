library(neuralnet)
library(readxl)
library(beepr)
library(caret) 

source("EvaluationMetrics.R")
source("evMethods.R")

# # # Data processing # # #  
cleaned_normalized_data <- cleaned_data

# normalize data
cleaned_normalized_data[,1] = normalize(cleaned_data[,1])
cleaned_normalized_data[,5] = normalize(cleaned_data[,5])
for(i in 1:20){
    cleaned_normalized_data[,i] = normalize(cleaned_data[,i])
}
trainData = cleaned_normalized_data[ indxTraining,]
validate_ann <- cleaned_normalized_data[indxValidate, ]
testData <- cleaned_normalized_data[-indxTrain, ]
# # # End Data processing # # #  

# # # tuning ANN # # # 
# the tuning is ommited for computational efficienfy. The best tuned parameters are used as can be seen in the report.
# tuningANN(data=trainData,nFolds = 2,nodes = 2,repet=2)

# # # performing ANN # # # 
set.seed(3)
network = neuralnet(Y~.,data=trainData, hidden=1, err.fct = "ce", linear.output = F,rep = 2)

# running the next line will plot the network
# plot(network)


# Calculating the probability forecasts
prob=neuralnet::compute(network,testData[,-21])
df = data.frame(prob)

# evaluation information 
y_original = as.vector(as.numeric(testData$Y)-1)
p_default = as.vector(df$net.result.2)

# Isotonic recalibration
ann_iso_recal = iso_recal_func_ANN(network, validate_ann, p_default, testData)
# to evaluate recalibrated forecasts
p_default = ann_iso_recal

# # # Implementing Evaluation Metrics # # # 

# # Brier Score # # 
Brier_ANN = BrierScore(y_original,p_default)

# # Spiegelhalters Z Test # # 
Spiegel_z_ANN = Spiegelhalter_z(y_original,p_default)

# # cox-first degree # # 
CoxSlope_Intercept_ANN = cox_first_degree(y_original,p_default)

# # Hosmer Lemeshow # # 
HL_C_ANN = hosmer_lemeshow(y_original,p_default,g=10,"C")
HL_H_ANN = hosmer_lemeshow(y_original,p_default,g=10,"H")

# # log score # # 
Mean_LogScore_ANN = mean(logs_binom(y_original,1,p_default))

# # Auroc # # 
par(pty= "s")
AUROC_ANN = roc(y_original, p_default, plot = TRUE)

# # Reliability diagram # # 
Reliability_diagram_ANN = ReliabilityDiagram(
  p_default,y_original, 
  bins = 10,
  nboot = 500,
  plot = TRUE,
  plot.refin = FALSE,
  cons.probs = 0.95,
  attributes = FALSE,
  handle.na = c("na.fail", "use.pairwise.complete")
)

