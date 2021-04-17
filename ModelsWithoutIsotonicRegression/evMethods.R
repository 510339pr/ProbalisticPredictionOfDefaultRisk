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

# Spiegelhalter Z test
# y is 'real' realized variable
# prob is forecasted default probability
Spiegelhalter_z = function(y, prob){
  alpha = 0.05 #significance level for test
  z_score = sum((y-prob)*(1-2*prob))/sqrt(sum(((1-2*prob)^2)*prob*(1-prob)))
  print(z_score)
  if (abs(z_score) > qnorm(1-alpha/2)){
    print('reject null. NOT calibrated')
  } else{
    print('fail to reject. calibrated')
  }
  cat('z score: ', z_score, '\n')
  cat('p value: ', 1-pnorm(abs(z_score)), '\n')
  Pval = 1-pnorm(abs(z_score))
  return(data.frame(z_score, Pval))
}

# Cox intecept & slope
cox_first_degree = function(y, prob){
  dat <- data.frame(e = prob, o = y)
  dat$e[dat$e == 0] = 0.0000001
  dat$e[dat$e == 1] = 0.9999999
  dat$e
  dat$logite <- logit(dat$e)
  mfit = glm(formula = o~I(logite), 
             family = binomial(link = "logit"), dat)
  slope = mfit$coefficients[2]
  intercept = mfit$coefficients[1]
  return(list(slope = slope, intercept = intercept))
}

# Hosmer Lemeshow
# Y is realized default
# prob is default probability
# stat_type is: H or C for Hosmer Lemeshow C or H stat
# g is number of bins, in this research g = 10
hosmer_lemeshow = function(y, prob, g, stat_type){
  mtx = cbind(y, y_not = 1- y, prob, prob_not = 1-prob)
  mtx = as.data.frame(mtx)
  mtx = mtx[order(mtx$prob),]
  n <- length(prob)/g
  nr <- nrow(mtx)
  
  ## C statistics, same number of instances in each bin
  if (stat_type == 'C'){
    split_mtx = split(mtx, rep(1:ceiling(nr/n), each=n, length.out=nr))
  }else{ ### H statistics, equal intervals
    split_mtx = split(mtx, cut(mtx$prob, seq(0,1,1/g), include.lowest=TRUE))
    split_mtx = split_mtx[sapply(split_mtx, nrow)>0]
  }
  
  H_stat = 0
  for (i in 1:length(split_mtx)){
    obs = sum(split_mtx[[i]]$y == TRUE)
    exp = sum(split_mtx[[i]]$prob)
    obs_not = sum(split_mtx[[i]]$y == FALSE)
    exp_not = sum(split_mtx[[i]]$prob_not)
    
    if (exp == 0 || exp_not == 0){
      next
    }
    
    bin_sum = ((obs - exp)**2)/exp + ((obs_not - exp_not)**2)/exp_not
    
    H_stat = H_stat + bin_sum
  }
  PVAL = 1 - pchisq(H_stat, g - 2)
  
  cat('PVALUE', PVAL, '\n')
  cat('stat', H_stat, '\n')
  
  return (data.frame(H_stat, PVAL))
}


