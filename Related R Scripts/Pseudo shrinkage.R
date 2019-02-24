#Pseudo shrinkage, litter size increase, effect size decrease

I=8 #number of litters
J=rep(c(2,2,5,7), 2) #number of individuals in litter
diff=2
#diff=c(10, 4, 2, 1)
sd.litter=c(0.2)
sd.animal=5

  litter.ID=rep(c(1:I), times=J)
  animal.ID=sequence(J) #Cheap way to do this....
  treat.ID=rep(c(0,1), each=0.5*length(animal.ID)) #wont work if groups dont have parallel litter sizes
  
  #treat 0
  litter.0=rep(rnorm(n=0.5*I, mean=0, sd=sd.litter), times=J[1:c(0.5*I)])
  
  #treat 1
  litter.1=rep(rnorm(n=0.5*I, mean=diff, sd=sd.litter), times=J[1:c(0.5*I)])
  
  y=rnorm(n=sum(J), mean=c(litter.0, litter.1), sd=sd.animal)

#Create Df
data <- as.data.frame(cbind(litter.ID, animal.ID, treat.ID, y), row.names = NULL)

results <- lm(y~treat.ID, data=data)
summary(results)


#Plot
plot(litter.ID, y)

sem <- function(x){
 sd(x)/sqrt(length(x))
}

avg <- aggregate( y ~ litter.ID, data, mean)
err <- aggregate( y ~ litter.ID, data, sem)

new <- merge(avg, err, by="litter.ID")

plot(new$litter.ID, new$y.x,
     ylim=range(c(new$y.x-new$y.y, new$y.x+new$y.y)),
     pch=20, xlab="Litter ID", ylab="Mean +/- SEM")

arrows(new$litter.ID, new$y.x-new$y.y, new$litter.ID, new$y.x+new$y.y, length=0.05, angle=90, code=3)

