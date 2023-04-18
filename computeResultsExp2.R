computeResultsExp2<-function(result, alpha4, S, a1, b1){
  
  
  Bias0<-matrix(NA,length(a1),length(b1))
  MSE0<-matrix(NA,length(a1),length(b1))
  Variance0<-matrix(NA,length(a1),length(b1))
  
  
  Bias<-matrix(NA,length(a1),length(b1))
  MSE<-matrix(NA,length(a1),length(b1))
  Variance<-matrix(NA,length(a1),length(b1))
  c<-matrix(NA,length(a1),length(b1))

  
  for (i in 1:length(a1)) {
    for (j in 1:length(b1)) {
      
    gamma0<-result[,i,j,1]
    
    
    Bias0[i,j]<-(1/S)*sum(gamma0-alpha4)
    
    MSE0[i,j]<-(1/S)*sum((gamma0-alpha4)^2) 
    
    Variance0[i,j]<-MSE0[i,j]-Bias0[i,j]^2
    
    
    gamma<-result[,i,j,2]
    
    
    Bias[i,j]<-(1/S)*sum(gamma-alpha4)
    
    MSE[i,j]<-(1/S)*sum((gamma-alpha4)^2) 
    
    Variance[i,j]<-MSE[i,j]-Bias[i,j]^2
    
    c<-(1/S)*sum(result[,i,j,3])
    
    }
  }
    
  
  
  return(list(Bias0=Bias0, MSE0=MSE0, Variance0=Variance0, Bias=Bias, MSE=MSE, Variance=Variance, c=c))
  
}
