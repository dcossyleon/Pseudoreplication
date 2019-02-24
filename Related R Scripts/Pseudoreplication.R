#Pseudoreplication June 27, 2018

I=20 #number of litters
J=5 #number of individuals
diff=100 #treatment effect
sd.litter=1 #difference between litters
sd.res=0.2 #difference within litter

litter.ID=rep(c(1:I), each=J)
animal.ID=rep(c(1:J), times=I)
treat.ID=rep(c(0,1), each=I*0.5*J)
  
mean.0=rep(rnorm(n=0.5*I, mean=0, sd=sd.litter), each=J)
mean.1=rep(rnorm(n=0.5*I, mean=diff, sd=sd.litter), each=J)
y=rnorm(n=(I*J), mean=c(mean.0, mean.1), sd=sd.res)

#Create Df
data <- as.data.frame(cbind(litter.ID, animal.ID, treat.ID, y), row.names = NULL)

