# Brier Score

# x : the classifications -> thus 0 or 1
# p : the probability that we classify something as 1
brierScore = function(p, x){
  return(sum((x-p)^2)*(1/length(p)))
}

# normalizing data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

tuningSVM2 = function(data,nFolds,c,paramValue, kern, parameter){
  
  yourData = data
  
  # Randomly shuffle the data
  yourData = yourData[sample(nrow(yourData)),]
  
  # Create equally size folds
  folds = cut(seq(1,nrow(yourData)),breaks=nFolds,labels=FALSE)
  
  # save Brier score
  errorVector = c(1:nFolds)
  
  # Perform cross validation
  for(i in 1:nFolds){
    
    # Segment your data by fold using the which() function 
    testIndexes = which(folds==i,arr.ind=TRUE)
    testingData = yourData[testIndexes, ]
    trainingData = yourData[-testIndexes, ]
    
    # perform svm
    svmfit=svm(Y ~., kernal=kern, data=trainingData, cost=c, parameter=paramValue, probability=T)
    
    ypred=predict(svmfit,testingData,probability = T)
    df = data.frame(ypred,attr(ypred, "probabilities"))
    
    # Brier score
    y_original = as.vector(as.numeric(testingData$Y)-1)
    p_default = as.vector(df$TRUE.)
    brierScore(p_default,y_original)
    errorVector[i]= brierScore(p_default,y_original)
    
  }
  
  return(mean(errorVector))
  
}

tuningANN = function(data,nFolds,nodes, repet){
  
  yourData = data
  
  # Randomly shuffle the data
  yourData = yourData[sample(nrow(yourData)),]
  
  # Create equally size folds
  folds = cut(seq(1,nrow(yourData)),breaks=nFolds,labels=FALSE)
  
  # save Brier score
  errorVector = c(1:nFolds)
  
  # Perform cross validation
  for(i in 1:nFolds){
    
    # Segment your data by fold using the which() function 
    testIndexes = which(folds==i,arr.ind=TRUE)
    testingData = yourData[testIndexes, ]
    trainingData = yourData[-testIndexes, ]
    
    # perform ANN
    network = neuralnet(Y~.,data=trainingData, hidden=nodes, err.fct = "ce", linear.output = F,rep = repet)
    
    prob=neuralnet::compute(network,testingData[,-24])
    df = data.frame(prob)
    
    # Brier score
    y_original = as.vector(as.numeric(testingData$Y)-1)
    p_default = as.vector(df$net.result.2)
    brierScore(p_default,y_original)
    errorVector[i]= brierScore(p_default,y_original)
    
  }
  
  return(mean(errorVector))
  
}

