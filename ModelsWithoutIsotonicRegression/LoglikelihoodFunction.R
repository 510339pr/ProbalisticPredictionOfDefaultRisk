# theta is the vector with all parameters, x represent the weights  
# y is the default value
log_like = function(theta,x1,x2,x3,x4,x5,x6,y) {
   n = NROW(y);
   alpha = theta[1];
   beta = theta[2];
   w1 = theta[3];
   w2 = theta[4];
   w3 = theta[5];
   w4 = theta[6];
   w5 = theta[7];
   w6 = theta[8];
   vectorofone = replicate(n,1)
   functie = rep(n,1);
   loglike = replicate(n,1)

   for (i in 1:n) {
     functie[i] = pbeta(w1*x1[i] + w2*x2[i] + w3*x3[i] + w4*x4[i] + w5*x5[i] + w6*x6[i],alpha,beta)
     loglike[i] = y[i] * log(functie[i]) +   log( (vectorofone[i] - functie[i])) * (vectorofone[i]-y[i])
   }
   som = sum(loglike)

   return(-som)
}
