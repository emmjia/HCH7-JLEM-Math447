# creating data table
A = rep(c("-","+","-","+","-","+","-","+"), times = 3)
B = rep(c("-","-","+","+","-","-","+","+"), times = 3)
C = rep(c("-","-","-","-","+","+","+","+"), times = 3)
Rep = rep(c(1, 2, 3), each = 8)
yield = c(22,32,35,55,44,40,60,39,31,43,34,47,45,37,50,41,25,29,50,46,38,36,54,47)
#dataframe
cutting.speed.long = data.frame(A, B, C, Rep, yield)

# defining coded
coded=function(x) #a function to code variable x
{
  ifelse(x=="+", 1, -1)
}
# coding A and B
#for (j in 1:3)
 # cutting.speed.long[, j]=as.numeric(coded(cutting.speed.long[, j]))

#confounding ABC in RepI
cutting.speed.longRep1=cutting.speed.long[cutting.speed.long$Rep == 1,]
A=coded(cutting.speed.longRep1$A)
B=coded(cutting.speed.longRep1$B)
C=coded(cutting.speed.longRep1$C)
cutting.speed.longRep1$Block=ifelse(A * B * C < 0, 1, 2)
cutting.speed.longRep1=cutting.speed.longRep1[order(cutting.speed.longRep1$Block),]
#confounding ABC in RepII
cutting.speed.longRep2=cutting.speed.long[cutting.speed.long$Rep == 2,]
A=coded(cutting.speed.longRep2$A)
B=coded(cutting.speed.longRep2$B)
C=coded(cutting.speed.longRep2$C)
cutting.speed.longRep2$Block=ifelse(A * B * C < 0, 1, 2)
cutting.speed.longRep2=cutting.speed.longRep2[order(cutting.speed.longRep2$Block),]
#confounding ABC in RepIII
cutting.speed.longRep3=cutting.speed.long[cutting.speed.long$Rep == 3,]
A=coded(cutting.speed.longRep3$A)
B=coded(cutting.speed.longRep3$B)
C=coded(cutting.speed.longRep3$C)
cutting.speed.longRep3$Block=ifelse(A * B * C < 0, 1, 2)
cutting.speed.longRep3=cutting.speed.longRep3[order(cutting.speed.longRep3$Block),]
partialConfounding.abc=rbind(cutting.speed.longRep1, cutting.speed.longRep2, cutting.speed.longRep3)
partialConfounding.abc$Rep=factor(partialConfounding.abc$Rep)
partialConfounding.abc$Blocks=factor(paste(partialConfounding.abc$Rep,
                                       partialConfounding.abc$Block,sep = "-"))
#ANOVA
cutting.confoundedabc.aov = aov(yield ~ Blocks + A * B * C, partialConfounding.abc)
summary(cutting.confoundedabc.aov)

#Regular
cutting.regular.aov = aov(yield ~  A*B*C, cutting.speed.long); summary(cutting.regular.aov)
cutting.regular.lm = lm(yield ~ A*B*C, cutting.)
#Residual Analysis for cutting.confounded.aov
cutting.confoundedabc.lm = lm(yield ~ Blocks + A * B * C, partialConfounding.abc)
res=partialConfounding.abc$yield-fitted(cutting.confoundedabc.lm)
qqPlot(res)
plot(fitted(cutting.confoundedabc.lm), res)#Good Model
