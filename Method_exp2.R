

Method_exp2<- function(SimData){
  
  
  Y<-SimData$Y
  A<-SimData$A
  
  X1<-SimData$X1
  X2<-SimData$X2
  X3<-SimData$X3
  
  
  PS <- glm(A ~ X1 , family = binomial(link = "probit"), data = SimData)
  
  PS <- PS$fitted.values
  
  quintiles<-quantile(PS, probs = seq(0,1,0.2), names = F)
  
  
  #Method 1: Stratification
  strata<-list()
  EEstrata<-c()
  for (i in 1:5) {
    rows<-which(PS>=quintiles[i] & PS<quintiles[i+1])
    strata[[i]]<-SimData[rows,]
    model<-glm(formula = Y ~ 1 + A, family=poisson(link=log), data = strata[[i]])
    EEstrata[i]<-model$coefficients[length(model$coefficients)]
  }
  
  gamma0<-mean(EEstrata)

  
  
  #Method 2 Spline
  
  bPS<-splines2::bSpline(PS, knots = quantile(PS, names = F)[2:4] ) #bs(PS, knots=quantile(PS, names = F)[2,4])
  
  k<-NCOL(bPS)
  
  colnames(bPS)<- paste0("B",1:k)
  
  
  SimData<-cbind(SimData,bPS)
  
  model2<-glm(formula = Y ~ 1 + B1 + B2 + B3 + B4 +B5 +B6 + A, family=poisson(link=log), data = SimData)
  gamma1<-model2$coefficients[length(model2$coefficients)]
  
  
  c<-0 #DescTools::Cstat(model2)
  
  output<-c(gamma0, gamma1, c=c)
  
  return(output)
  
}

#=======

