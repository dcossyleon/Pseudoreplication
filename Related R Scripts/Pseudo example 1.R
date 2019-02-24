#Example 1, modelling eggs: real effect, but not-significant when you do it right

I=12 #number of litters
J=5 #number of individuals/litter
diff=0.4 #treatment effect
sd.litter=1 #difference between litters
sd.res=1 #difference within litter

litter.ID=rep(c(1:I), each=J)
animal.ID=rep(c(1:J), times=I)
treat.ID=rep(c(0,1), each=I*0.5*J)

mean.0=rep(rnorm(n=0.5*I, mean=0, sd=sd.litter), each=J)
mean.1=rep(rnorm(n=0.5*I, mean=diff, sd=sd.litter), each=J)
y=rnorm(n=(I*J), mean=c(mean.0, mean.1), sd=sd.res)

#Create Df
data <- as.data.frame(cbind(litter.ID, animal.ID, treat.ID, y), row.names = NULL)

results <- lm(y~treat.ID, data=data)
summary(results)


#Plot, with SEM
  plot(litter.ID, y)
  
  sem <- function(x){
    sd(x)/sqrt(length(x))
  }
  
  avg <- aggregate( y ~ litter.ID, data, mean)
  err <- aggregate( y ~ litter.ID, data, sem)
  
  new <- merge(avg, err, by="litter.ID")
    new$group <- rep(c(0,1), each=I*0.5)
  
  plot(new$litter.ID, new$y.x,
       ylim=range(c(new$y.x-new$y.y, new$y.x+new$y.y)),
       pch=20, xlab="Litter ID", ylab="Mean +/- SEM")
  
  arrows(new$litter.ID, new$y.x-new$y.y, new$litter.ID, new$y.x+new$y.y, length=0.05, angle=90, code=3)
  
  #Plot as individual animals
  plot(animal.ID, y)
  

#The right way
  results_right <- lm(y.x~group, data=new) #y.x is the avg
  summary(results_right)

#Plot
plot(new$litter.ID, new$y.x)
new$err_right <- sem(new$y.x[1:6])

new <- merge(avg, err, by="litter.ID")
new$group <- rep(c(0,1), each=I*0.5)

plot(new$litter.ID, new$y.x,
     ylim=range(c(new$y.x-new$y.y, new$y.x+new$y.y)),
     pch=19, xlab="Litter ID", ylab="Mean +/- SEM")

arrows(new$litter.ID, new$y.x-new$y.y, new$litter.ID, new$y.x+new$y.y, length=0.05, angle=90, code=3)


#Intra-class correlation
intra.corr=sd.litter^2/(sd.litter^2+sd.res^2)
design_effect = 1 + (J-1)*intra.corr
N_eff <- (I*J)/design_effect
