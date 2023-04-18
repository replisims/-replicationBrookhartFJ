#plots


library(ggplot2)

#plot 1

#build data frame

Values<-c(colMeans(resExp2_500$Variance0), colMeans(resExp2_500$Variance), colMeans(resExp2_2500$Variance0), colMeans(resExp2_2500$Variance))
#here we make a decision, use colMeans.


Measure<-c(rep("var0",26), rep("var1",26), rep("var0",26), rep("var1",26))

n<- c(rep(500,52), rep(2500, 52))


b1<-c(seq(0, 1.25, 0.05),seq(0, 1.25, 0.05))


Measure_n<-c(rep("var0_n500",26), rep("var1_n500",26), rep("var0_n2500",26), rep("var1_n2500",26))


df<-data.frame(Values, Measure, n, b1, Measure_n)




ggplot(data=df, aes(x=b1, y=Values, linetype=Measure_n)) + geom_line()




#plot 2

a1<-c(rep(seq(0, 0.2, 0.01),19),rep(seq(0, 0.2, 0.01),19))

Values<-c(as.vector(resExp2_500$MSE[,-(20:26)]/(resExp2_500$MSE0[,-(20:26)])), as.vector(resExp2_2500$MSE[,-(20:26)])/(resExp2_2500$MSE0[,-(20:26)]))
#Here we make a decision, use rowMeans

n<-c(rep(500,399), rep(2500,399))

b1<-c(rep(0,21),rep(0.05,21),rep(0.1,21),rep(0.15,21),rep(0.2,21),rep(0.25,21),rep(0.3,21),rep(0.35,21),rep(0.4,21),rep(0.45,21),rep(0.5,21),
      rep(0.55,21),rep(0.6,21),rep(0.65,21),rep(0.7,21),rep(0.75,21),rep(0.8,21),rep(0.85,21),rep(0.9,21))

b1<-c(b1,b1)  
  #c(seq(0, 0.9, 0.05),seq(0, 0.9, 0.05)) # c(seq(0, 1.25, 0.05),seq(0, 1.25, 0.05))



df2<-data.frame(Values, a1, b1, n)



ggplot(data=df2, aes(x=a1, y=b1 )) + geom_contour(aes(z = Values)) + geom_text_contour(aes(z = value), colour = "black" ) + facet_grid(. ~ n)
                                                    




#plot 3






a1<-seq(0, 0.2, 0.01)