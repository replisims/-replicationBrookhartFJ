#<<<<<<< HEAD
computeResults<-function(result, alpha4, S){
  
  
  Bias0<-c()
  MSE0<-c()
  Variance0<-c()
  
  
  Bias<-c()
  MSE<-c()
  Variance<-c()
  c<-c()
  for (model in 1:8) { #problems with model 8
    
    
    gamma0<-result[,(3*model) -2]
    
    
    Bias0[model]<-(1/S)*sum(gamma0-alpha4)
    
    MSE0[model]<-(1/S)*sum((gamma0-alpha4)^2) 
    
    Variance0[model]<-MSE0[model]-Bias0[model]^2
    
    
    gamma<-result[,(3*model) -1]
    
    
    Bias[model]<-(1/S)*sum(gamma-alpha4)
    
    MSE[model]<-(1/S)*sum((gamma-alpha4)^2) 
    
    Variance[model]<-MSE[model]-Bias[model]^2
    
    c[model]<-(1/S)*sum(result[,3*model])
    
  }
  
  
  return(list(Bias0=Bias0, MSE0=MSE0, Variance0=Variance0, Bias=Bias, MSE=MSE, Variance=Variance, c=c))
  
}
    
# =======
# computeResults<-function(result, alpha4, S){
#   
#   
#   Bias<-c()
#   MSE<-c()
#   Variance<-c()
#   c<-c()
#   for (model in 1:8) {
#     
#     gamma<-result[,(2*model) -1]
#     
#     
#     Bias[model]<-(1/S)*sum(gamma-alpha4)
#     
#     MSE[model]<-(1/S)*sum((gamma-alpha4)^2) 
#     
#     Variance[model]<-MSE-Bias^2
#     
#     c[model]<-(1/S)sum(result[,2*model])
#     
#   }
#   
#   
#   return(list(Bias=Bias, MSE=MSE, Variance=Variance, c=c))
# >>>>>>> bf47da6a1fdb87610a2366c79e6f55cd26b6b46f
# }