

Method_exp1<- function(SimData, model){
  
  
  Y<-SimData$Y
  A<-SimData$A
  
  X1<-SimData$X1
  X2<-SimData$X2
  X3<-SimData$X3
  
  
  
  
  
  if(model==1){
    
    PS <- glm(A ~ X1, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
  }else if(model==2){
    
    
    
    PS <- glm(A ~ X2, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
    
  }else if(model==3){
    
    
    PS <- glm(A ~ X3, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
  }else if(model==4){
    
    PS <- glm(A ~ X1 + X2, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
  }else if(model==5){
    
    PS <- glm(A ~ X1 + X3, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
  }else if(model==6){
    
    
    PS <- glm(A ~ X2 + X3, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
    
    
  }else if(model==7){
  
    
    PS <- glm(A ~ X1 + X2 +X3, family = binomial(link = "probit"), data = SimData)
    
    PS <- PS$fitted.values
    
    
  }
  
  
  
  if(model<8){
  
  #Method 1: Stratification
  quintiles<-quantile(PS, probs = seq(0,1,0.2), names = F)
  
  
  strata<-list()
  EEstrata<-c()
  for (i in 1:5) {
    rows<-which(PS>=quintiles[i] & PS<quintiles[i+1])
    strata[[i]]<-SimData[rows,]
    modelStrata<-glm(formula = Y ~ 1 + A, family=poisson(link=log), data = strata[[i]])
    EEstrata[i]<-modelStrata$coefficients[length(modelStrata$coefficients)]
  }
  
  gamma0<-mean(EEstrata)
  
  }else{
    
  gamma<-0  
    
  }
  
  
  #Method 2 Spline
  
  
  bPS<-splines2::bSpline(PS, knots = quantile(PS, names = F)[2:4] ) #bs(PS, knots=quantile(PS, names = F)[2,4])
  
  k<-NCOL(bPS)
  
  colnames(bPS)<- paste0("B",1:k)
  
  
  SimData<-cbind(SimData,bPS)
  
    
  outmodel<-glm(formula = Y ~ 1 + B1 + B2 + B3 + B4 +B5 +B6 + A, family=poisson(link=log), data = SimData)
  
  if(model==8){
    
    outmodel<-glm(formula = Y ~ 1 + A, family=poisson(link=log), data = SimData)
    
  }  
  
  
  
  gamma1<-outmodel$coefficients[length(outmodel$coefficients)]
    
  c<-DescTools::Cstat(outmodel)  
  
  output<-list(gamma0=gamma0, gamma1=gamma1, c=c)
  
  return(output)
  
}

# =======
# 
# Method_exp1<- function(SimData, model){
#   
#   
#   Y<-SimData$Y
#   A<-SimData$A
#   
#   X1<-SimData$X1
#   X2<-SimData$X2
#   X3<-SimData$X3
#   
#   
#   
#   
#   
#   if(model==1){
#     
#     PS <- glm(A ~ X1, family = binomial(link = "probit"), data = SimData)
#     
#     PS <- PS$fitted.values
#     
#     
#     
#   }else if(model==2){
#     
#     
#     
#     PS <- glm(A ~ X2, family = binomial(link = "probit"), data = SimData)
#     
#     PS <- PS$fitted.values
#     
#     
#     
#     
#   }else if(model==3){
#     
#     
#     PS <- glm(A ~ X3, family = binomial(link = "probit"), data = SimData)
#     
#     PS <- PS$fitted.values
#     
#     
#     
#   }else if(model==4){
#     
#     PS <- glm(A ~ X1 + X2, family = binomial(link = "probit"), data = SimData)
#     
#     PS <- PS$fitted.values
#     
#     
#     
#   }else if(model==5){
#     
#     PS <- glm(A ~ X1 + X3, family = binomial(link = "probit"), data = SimData)
#     
#     PS <- PS$fitted.values
#     
#     
#     
#   }else if(model==6){
#     
#     
#     PS <- glm(A ~ X2 + X3, family = binomial(link = "probit"), data = SimData)
#     
#     PS <- PS$fitted.values
#     
#     
#     
#     
#   }else if(model==7){
#   
#     
#     PS <- glm(A ~ X1 + X2 +X3, family = binomial(link = "probit"), data = SimData)
#     
#     PS <- PS$fitted.values
#     
#     
#   }else if(model==8){
#     
#     PS <- glm(A ~ 1, family = binomial(link = "probit"), data = SimData)
#     
#     PS <- PS$fitted.values
#     
#   }  
#   
#   
#   
#   
#   bPS<-splines2::bSpline(PS, knots = quantile(PS, names = F)[2:4] ) #bs(PS, knots=quantile(PS, names = F)[2,4])
#   
#   k<-NCOL(bPS)
#   
#   colnames(bPS)<- paste0("B",1:k)
#   
#   
#   SimData<-cbind(SimData,bPS)
#   
#     
#   outmodel<-glm(formula = Y ~ 1 + B1 + B2 + B3 + B4 +B5 +B6 + A, family=poisson(link=log), data = SimData)
#   
#   gamma<-outmodel$coefficients[length(outmodel$coefficients)]
#     
#   c<-0#MISSING  
#   
#   output<-list(gamma=gamma, c=c)
#   
#   return(output)
#   
# }
# 
# >>>>>>> bf47da6a1fdb87610a2366c79e6f55cd26b6b46f
