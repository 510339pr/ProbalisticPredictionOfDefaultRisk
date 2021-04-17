source('iso_func_draw.R')


iso_recal_func = function(model_fit, validate_data, test_res, test){
  
  # predict validation datasets estimates with svm
  val_estimates = predict(model_fit, validate_data, type = "prob")
  probabilities <- val_estimates[,2]
  val_estimates_norm = as.data.frame(probabilities)
  train_re_mtx = cbind(y=as.numeric(validate$Y)-1, yhat=val_estimates_norm[,1])
  
  
  iso_train_mtx = train_re_mtx[order(train_re_mtx[,2]),]
  
  # create calibration model
  calib.model <- isoreg(iso_train_mtx[,2], iso_train_mtx[,1])
  stepf_data = cbind(calib.model$x, calib.model$yf)
  step_func = stepfun(stepf_data[,1], c(0,stepf_data[,2]))
  
  # recalibrate and measure on test set
  exp2_iso_recal <- step_func(test_res)
  
  return(exp2_iso_recal)
}

iso_recal_func_ANN = function(model_fit, validate_data, test_res, test){
  
  # predict validation datasets estimates with svm
  val_estimates = neuralnet::compute(model_fit,validate_data[,-21])
  df_hulp_knn = data.frame(val_estimates)
  
  probabilities <- as.vector(df_hulp_knn$net.result.2)
  val_estimates_norm = as.data.frame(probabilities)
  train_re_mtx = cbind(y=as.numeric(validate$Y)-1, yhat=val_estimates_norm[,1])
  
  
  iso_train_mtx = train_re_mtx[order(train_re_mtx[,2]),]
  
  # create calibration model
  calib.model <- isoreg(iso_train_mtx[,2], iso_train_mtx[,1])
  stepf_data = cbind(calib.model$x, calib.model$yf)
  step_func = stepfun(stepf_data[,1], c(0,stepf_data[,2]))
  
  # recalibrate and measure on test set
  exp2_iso_recal <- step_func(test_res)

  return(exp2_iso_recal)
}

iso_recal_func_SVM = function(model_fit, validate_data, test_res, test){
  
  # predict validation datasets estimates with svm
  val_estimates = predict(model_fit, validate_data, probability = T)
  df_iso_hulp <- data.frame(val_estimates,attr(val_estimates, "probabilities"))
  probabilities <- as.vector(df_iso_hulp$TRUE.)
  val_estimates_norm = as.data.frame(probabilities)
  train_re_mtx = cbind(y=as.numeric(validate_data$Y)-1, yhat=val_estimates_norm[,1])
  
  
  iso_train_mtx = train_re_mtx[order(train_re_mtx[,2]),]
  
  # create calibration model
  calib.model <- isoreg(iso_train_mtx[,2], iso_train_mtx[,1])
  stepf_data = cbind(calib.model$x, calib.model$yf)
  step_func = stepfun(stepf_data[,1], c(0,stepf_data[,2]))
  
  # recalibrate and measure on test set
  exp2_iso_recal <- step_func(test_res)
  
  return(exp2_iso_recal)
}
