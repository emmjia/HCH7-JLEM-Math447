#############################################
##     Blocking a replicated design         #
#############################################
#Yield data (pp. 234)
yield=read.table("data/yield.txt", header = TRUE)
summary(aov(Yield ~ Rep + A * B, yield)) #analysis of design with blocking
summary(aov(Yield ~ A * B, yield)) # analysis of design without blocking

#recode the data: x=1 at level +; X=-1 at level -
coded=function(x) #a function to code variable x
{
  ifelse(x=="+", 1, -1)
}
###############################################
##     confounding a 2^K desgin in 2 blocks ### 
###############################################
#filtration rate data (eg. 6.2/7.2 page 257/310)
filtration=read.table("data/filtration.txt", header = TRUE)
for (j in 1:4)
  filtration[, j]=as.numeric(coded(filtration[, j]))
#factors have already been converted from "-","+" to -1, +1 coding
#Note that ABCD cannot be estimated, because it is confounded with blocks
filtration$Block=filtration$A * filtration$B *filtration$C * filtration$D
summary(lm(Rate ~ Block + A * B * C * D, filtration))
#Note that the estimates identical to the ones obstained when there was no block effect
summary(lm(Rate ~ A * B * C * D, filtration)) 

#reduced model
#Note that all effects in the reduced model can be estimated and tested.
summary(aov(Rate ~ Block + A + C + D + A * C + A * D, filtration))


#############################################
##     partical confounding                 #
#############################################
#2^3 design, 2 replicates, each in 2 blocks, with ABC confounded with blocks in Rep I, 
#and AB confounded in Rep II (plasma etching tool data)
plasma=read.table("data/plasma.txt", header = TRUE)
plasmaLong=reshape(plasma, varying = c("Rep1", "Rep2"), v.names = "Rate",
                   direction = "long", timevar = "Rep")
plasmaLongRep1=plasmaLong[plasmaLong$Rep == 1,]
A=coded(plasmaLongRep1$A)
B=coded(plasmaLongRep1$B)
C=coded(plasmaLongRep1$C)
plasmaLongRep1$Block=ifelse(A * B * C < 0, 1, 2)
plasmaLongRep1=plasmaLongRep1[order(plasmaLongRep1$Block),]
plasmaLongRep2=plasmaLong[plasmaLong$Rep == 2,]
A=coded(plasmaLongRep2$A)
B=coded(plasmaLongRep2$B)
plasmaLongRep2$Block=ifelse(A * B > 0, 1, 2)
plasmaLongRep2=plasmaLongRep2[order(plasmaLongRep2$Block),]
partialConfounding=rbind(plasmaLongRep1, plasmaLongRep2)
partialConfounding$Rep=factor(partialConfounding$Rep)
partialConfounding$Blocks=factor(paste(partialConfounding$Rep,
                                       partialConfounding$Block,sep = "-"))
summary(aov(Rate ~ Blocks + A * B * C, partialConfounding))
#Note larger standard errors for partially confounded effects (*sqrt(2)):
summary(lm(Rate ~ Blocks + coded(A) * coded(B) * coded(C),
             partialConfounding))

