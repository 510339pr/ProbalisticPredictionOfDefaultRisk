# # # Loading stuff # # #
library(neuralnet)
library(readxl)
library(caret)

# load evaluation techniques
source("EvaluationMetrics.R")
source("evMethods.R")

# # # Data processing # # #  

testing_normalized = testing
training_normalized = training
for(i in 1:20){
  testing_normalized[,i] = normalize(testing[,i])
  training_normalized[,i] = normalize(training[,i])
}
trainData = training_normalized
testData = testing_normalized

# # # End Data processing # # # 

# # # # tuning ANN # # # 
# # Only necessary to verify that parameters for ANN are optimal
# set.seed(4)
# score=tuningANN(data=trainData,nFolds = 2,nodes = 2,repet=2)

# # # performing ANN # # # 
set.seed(3)
network = neuralnet(Y~.,data=trainData, hidden=1, err.fct = "ce", linear.output = F,rep = 4)
plot(network)

prob=neuralnet::compute(network,testData[,-21])

df = data.frame(prob)

# evaluation information 
y_original = as.vector(as.numeric(testData$Y)-1)
p_default = as.vector(df$net.result.2)

p_ANN = p_default

# # Brier Score # # 
Brier_ANN = BrierScore(y_original,p_default)

# # Spiegel Halters Z test # # 
Spiegel_z_ANN = Spiegelhalter_z(y_original,p_default)

# # Cox intercept & slope # # 
CoxSlope_Intercept_ANN = cox_first_degree(y_original,p_default)

# # hosmer lemeshow # # 
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

