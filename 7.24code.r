# creating data table
A = rep(c("-","+","-","+","-","+","-","+"), times = 3)
B = rep(c("-","-","+","+","-","-","+","+"), times = 3)
C = rep(c("-","-","-","-","+","+","+","+"), times = 3)
Rep = rep(c("I", "II", "III"), each = 8)
yield = c(22,32,35,55,44,40,60,39,31,43,34,47,45,37,50,41,25,29,50,46,38,36,54,47)
#dataframe
cutting.speed.long = data.frame(A, B, C, Rep, yield)

# defining coded
coded=function(x) #a function to code variable x
{
  ifelse(x=="+", 1, -1)
}

#for (j in 1:3)
 # cutting.speed.long[, j]=as.numeric(coded(cutting.speed.long[, j]))

#confounding ABC in RepI
cutting.speed.longRep1=cutting.speed.long[cutting.speed.long$Rep == "I",]
A=coded(cutting.speed.longRep1$A)
B=coded(cutting.speed.longRep1$B)
C=coded(cutting.speed.longRep1$C)
cutting.speed.longRep1$Block=ifelse(A * B * C < 0, 1, 2)
cutting.speed.longRep1=cutting.speed.longRep1[order(cutting.speed.longRep1$Block),]
#confounding AB in RepII
cutting.speed.longRep2=cutting.speed.long[cutting.speed.long$Rep == "II",]
A=coded(cutting.speed.longRep2$A)
B=coded(cutting.speed.longRep2$B)
cutting.speed.longRep2$Block=ifelse(A * B > 0, 1, 2)
cutting.speed.longRep2=cutting.speed.longRep2[order(cutting.speed.longRep2$Block),]
#confounding BC in RepIII
cutting.speed.longRep3=cutting.speed.long[cutting.speed.long$Rep == "III",]
B=coded(cutting.speed.longRep3$B)
C=coded(cutting.speed.longRep3$C)
cutting.speed.longRep3$Block=ifelse(B * C > 0, 1, 2)
cutting.speed.longRep3=cutting.speed.longRep3[order(cutting.speed.longRep3$Block),]
partialConfounding=rbind(cutting.speed.longRep1, cutting.speed.longRep2, cutting.speed.longRep3)
partialConfounding$Rep=factor(partialConfounding$Rep)
partialConfounding$Blocks=factor(paste(partialConfounding$Rep,
                                       partialConfounding$Block,sep = "-"))
#ANOVA
cutting.confounded.aov = aov(yield ~ Blocks + A * B * C, partialConfounding)
summary(cutting.confounded.aov)

#Residual Analysis for cutting.confounded.aov
cutting.confounded.lm = lm(yield ~ Blocks + A * B * C, partialConfounding)
res=partialConfounding$yield-fitted(cutting.confounded.lm)
qqPlot(res)
plot(fitted(cutting.confounded.lm), res)#Good Model
