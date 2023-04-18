#<<<<<<< HEAD
S<-1000 #number of repetitions

Experiment<-1



#n<-c(500,2500)#for exp1 and 2

result<-list()
resultExp2<-list()



#data<-MyDataGeneration(500, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5)
  



if(Experiment==1){
  
  # b0=0
  # b1=0.5
  # b2=0
  # b3=0.75
  # a0=0.5
  # a1=4
  # a2=1 
  # a3=0
  # a4=0.5
  
  ResultExp1<-list()
  ResultSensitivity<-list()
  for (j in 1:2) {
    
  n<-c(500,2500)[j]
  
  result<-matrix(NA, nrow = S, ncol=3*8)
  for (i in 1:S) {
    data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5)
    
    
    for (model in 1:8) { #errors in model 8
      outMethod<-Method_exp1(data, model=model)
      result[i,3*model-2]<-outMethod$gamma0
      result[i,3*model-1]<-outMethod$gamma1
      result[i,3*model]<-outMethod$c
    }
    
    
   
    
    
  }
  
  ResultExp1[[j]]<-computeResults(result, alpha4=0.5, S=1000) 
  
  
  
  if(n==500){
    
    #modif 1
    std1<-1.5
    
    
    resultS1<-matrix(NA, nrow = S, ncol=3*8)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std1=std1)
      
      
      for (model in 1:8) { #errors in model 8
        outMethod<-Method_exp1(data, model=model)
        resultS1[i,3*model-2]<-outMethod$gamma0
        resultS1[i,3*model-1]<-outMethod$gamma1
        resultS1[i,3*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[1]]<-computeResults(resultS1, alpha4=0.5, S=1000) 
    
    
    #modif 2
    std1<-0.5
    
    
    resultS2<-matrix(NA, nrow = S, ncol=3*8)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std1=std1)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS2[i,3*model-2]<-outMethod$gamma0
        resultS2[i,3*model-1]<-outMethod$gamma1
        resultS2[i,3*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[2]]<-computeResults(resultS2, alpha4=0.5, S=1000) 
    
    #modif 3
    std2<-1.5
    
    
    resultS3<-matrix(NA, nrow = S, ncol=3*8)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std2=std2)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS3[i,3*model-2]<-outMethod$gamma0
        resultS3[i,3*model-1]<-outMethod$gamma1
        resultS3[i,3*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[3]]<-computeResults(resultS3, alpha4=0.5, S=1000) 
    
    
    #modif 4
    std2<-0.5
    
    
    resultS4<-matrix(NA, nrow = S, ncol=3*8)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std2=std2)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS4[i,3*model-2]<-outMethod$gamma0
        resultS4[i,3*model-1]<-outMethod$gamma1
        resultS4[i,3*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[4]]<-computeResults(resultS4, alpha4=0.5, S=1000) 
    
    #modif 5
    std3<-1.5
    
    resultS5<-matrix(NA, nrow = S, ncol=3*8)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std3=std3)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS5[i,3*model-2]<-outMethod$gamma0
        resultS5[i,3*model-1]<-outMethod$gamma1
        resultS5[i,3*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[5]]<-computeResults(resultS5, alpha4=0.5, S=1000) 
    
    #modif 6
    std3<-0.5
    
    
    resultS6<-matrix(NA, nrow = S, ncol=3*8)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std3=std3)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS6[i,3*model-2]<-outMethod$gamma0
        resultS6[i,3*model-1]<-outMethod$gamma1
        resultS6[i,3*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[6]]<-computeResults(resultS6, alpha4=0.5, S=1000) 
    
    #modif 7
    
    a4<-0.25
    
    resultS7<-matrix(NA, nrow = S, ncol=3*8)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=a4)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS7[i,3*model-2]<-outMethod$gamma0
        resultS7[i,3*model-1]<-outMethod$gamma1
        resultS7[i,3*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[7]]<-computeResults(resultS7, alpha4=a4, S=1000) 
    
    #modif 8
    
    a4<-1
    
    
    resultS8<-matrix(NA, nrow = S, ncol=3*8)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=a4)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS8[i,3*model-2]<-outMethod$gamma0
        resultS8[i,3*model-1]<-outMethod$gamma1
        resultS8[i,3*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[8]]<-computeResults(resultS8, alpha4=a4, S=1000) 
    
    #modif 9
    b0<- -1
    
    resultS9<-matrix(NA, nrow = S, ncol=3*8)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=b0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS9[i,3*model-2]<-outMethod$gamma0
        resultS9[i,3*model-1]<-outMethod$gamma1
        resultS9[i,3*model]<-outMethod$c
      }
      
        
      
    }
    
    ResultSensitivity[[9]]<-computeResults(resultS9, alpha4=0.5, S=1000) 
    
  }
  

  
  
  }
  
  
  
  
  
  
  
  
  
  
  
  
}


  
  
if(Experiment==2){
  a1<-seq(0, 0.2, 0.01)
  b1<-seq(0, 1.25, 0.05)
  data<-MyDataGeneration(500, b0=0, b1=0, b2=0, b3=0, a0=0, a1=a1, a2=0, a3=0, a4=0.5)
  
  result[[i]]<-Method_exp2(data, model=1)
}else{
  stop("Wrong number")
}




getResults(result) #bias, variance, mse
=======
S<-1000 #number of repetitions

Experiment<-1



#n<-c(500,2500)#for exp1 and 2

result<-list()




#data<-MyDataGeneration(500, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5)
  



if(Experiment==1){
  
  # b0=0
  # b1=0.5
  # b2=0
  # b3=0.75
  # a0=0.5
  # a1=4
  # a2=1 
  # a3=0
  # a4=0.5
  
  ResultExp1<-list()
  ResultSensitivity<-list()
  for (j in 1:2) {
    
  n<-c(500,2500)[j]
  
  result<-matrix(NA, nrow = S, ncol=16)
  for (i in 1:S) {
    data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5)
    
    
    for (model in 1:8) {
      outMethod<-Method_exp1(data, model=model)
      result[i,2*model-1]<-outMethod$gamma
      result[i,2*model]<-outMethod$c
    }
    
    
   
    
    
  }
  
  resultExp1[[j]]<-computeResults(result, alpha4=0.5, S=1000) 
  
  
  
  if(n==500){
    
    #modif 1
    std1<-1.5
    
    
    resultS1<-matrix(NA, nrow = S, ncol=16)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std1=std1)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS1[i,2*model-1]<-outMethod$gamma
        resultS1[i,2*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[1]]<-computeResults(resultS1, alpha4=0.5, S=1000) 
    
    
    #modif 2
    std1<-0.5
    
    
    resultS2<-matrix(NA, nrow = S, ncol=16)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std1=std1)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS2[i,2*model-1]<-outMethod$gamma
        resultS2[i,2*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[2]]<-computeResults(resultS2, alpha4=0.5, S=1000) 
    
    #modif 3
    std2<-1.5
    
    
    resultS3<-matrix(NA, nrow = S, ncol=16)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std2=std2)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS3[i,2*model-1]<-outMethod$gamma
        resultS3[i,2*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[3]]<-computeResults(resultS3, alpha4=0.5, S=1000) 
    
    
    #modif 4
    std2<-0.5
    
    
    resultS4<-matrix(NA, nrow = S, ncol=16)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std2=std2)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS4[i,2*model-1]<-outMethod$gamma
        resultS4[i,2*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[4]]<-computeResults(resultS4, alpha4=0.5, S=1000) 
    
    #modif 5
    std3<-1.5
    
    resultS5<-matrix(NA, nrow = S, ncol=16)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std3=std3)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS5[i,2*model-1]<-outMethod$gamma
        resultS5[i,2*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[5]]<-computeResults(resultS5, alpha4=0.5, S=1000) 
    
    #modif 6
    std3<-0.5
    
    
    resultS6<-matrix(NA, nrow = S, ncol=16)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5, std3=std3)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS6[i,2*model-1]<-outMethod$gamma
        resultS6[i,2*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[6]]<-computeResults(resultS6, alpha4=0.5, S=1000) 
    
    #modif 7
    
    a4<-0.25
    
    resultS7<-matrix(NA, nrow = S, ncol=16)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=a4)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS7[i,2*model-1]<-outMethod$gamma
        resultS7[i,2*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[7]]<-computeResults(resultS7, alpha4=a4, S=1000) 
    
    #modif 8
    
    a4<-1
    
    
    resultS8<-matrix(NA, nrow = S, ncol=16)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=a4)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS8[i,2*model-1]<-outMethod$gamma
        resultS8[i,2*model]<-outMethod$c
      }
      
      
      
    }
    
    ResultSensitivity[[8]]<-computeResults(resultS8, alpha4=a4, S=1000) 
    
    #modif 9
    b0<- -1
    
    resultS9<-matrix(NA, nrow = S, ncol=16)
    for (i in 1:S) {
      data<-MyDataGeneration(n, b0=b0, b1=0.5, b2=0, b3=0.75, a0=0.5, a1=4, a2=1, a3=0, a4=0.5)
      
      
      for (model in 1:8) {
        outMethod<-Method_exp1(data, model=model)
        resultS9[i,2*model-1]<-outMethod$gamma
        resultS9[i,2*model]<-outMethod$c
      }
      
        
      
    }
    
    ResultSensitivity[[9]]<-computeResults(resultS9, alpha4=0.5, S=1000) 
    
  }
  

  
  
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}


#else 

Experiment<-2
resultExp2_n500<-array(data = NA, dim = c(S,length(a1),length(b1),3))
resultExp2_n2500<-array(data = NA, dim = c(S,length(a1),length(b1),3))
  
if(Experiment==2){
  a1<-seq(0, 0.2, 0.01)
  b1<-seq(0, 1.25, 0.05)
  
  for (k in 1:2) {

    n<-c(500,2500)[k]    
  
    for (s in 1:S) {
      for (i in 1:length(a1)) {
        for (j in 1:length(b1)) {
          data<-MyDataGeneration(n, b0=0, b1=b1[j], b2=0, b3=0, a0=0, a1=a1[i], a2=0, a3=0, a4=0.5)
          ifelse(n==500,resultExp2_n500[s,i,j,]<-Method_exp2(data), resultExp2_n2500[s,i,j,]<-Method_exp2(data) )
          #resultExp2[[i*length(b1)+j]]<-Method_exp2(data) 
        }
      }
    }
  }
  
}else{
  stop("Wrong number")
}



resExp2_500<-computeResultsExp2(result=resultExp2_n500, 0.5, S=1000, a1, b1)
  
resExp2_2500<-computeResultsExp2(result=resultExp2_n2500, 0.5, S=1000, a1, b1)  





# getResults(result) #bias, variance, mse
# >>>>>>> bf47da6a1fdb87610a2366c79e6f55cd26b6b46f
