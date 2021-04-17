# # By running this main_IR method the results of the models with isotonic 
# # regression for the purpose of recalibration can be obtained

# before running set the working directory to source file directory
source('./evMethods.R')
source('./datacleaning_procedure_IR.R')
source('./isoReg.R')

# run each model & obtain the evaluation
source('./NB_IR.R')
source('./LR_IR.R')
source('./KNN_IR.R')
source('./RF_IR.R')
source('./SVM_IR.R')
source('./ANN_IR.R')
