#Pseudo Extreme Example: Reaction TImes

I=2 #number of litters
J=rep(c(5), 2) #number of individuals in litter
diff=1
#diff=c(10, 4, 2, 1)
sd.litter=6
sd.animal=2

  litter.ID=rep(c(1:I), times=J)
  animal.ID=sequence(J) #Cheap way to do this....
  treat.ID=rep(c(0,1), each=0.5*length(animal.ID)) #wont work if groups dont have parallel litter sizes
  
  #treat 0
  litter.0=rep(rnorm(n=0.5*I, mean=50, sd=sd.litter), times=J[1:c(0.5*I)])
  
  #treat 1
  litter.1=rep(rnorm(n=0.5*I, mean=50+diff, sd=sd.litter), times=J[1:c(0.5*I)])
  
  y=rnorm(n=sum(J), mean=c(litter.0, litter.1), sd=sd.animal)

#Create Df
data <- as.data.frame(cbind(litter.ID, animal.ID, treat.ID, y), row.names = NULL)
  #write.csv(data, file="eyeblink_sim.csv", quote = FALSE)


results <- lm(y~treat.ID, data=data)
summary(results)


#Plot
plot(y~jitter(treat.ID,0.05), xlab="Sex", ylab="Reaction time (ms)", xaxt="n", ylim=c(40,62), xlim=c(-01,2))


#Intra-class correlation
ICC=var(data$bg.res)/(var(data$bg.res)+var(data$wg.res))

#ICC=.9
#alpha=sqrt(ICC*(sd.animal^2)/(1-ICC))
#alpha

design_effect = 1 + (J-1)*ICC
N_litter <- (J)/design_effect
N_litter

N_eff=sum(N_litter)
N_eff

N_eff.per.group=N_eff*0.5
N_eff.per.group


#resids
g0 <- rep(mean(data[1:5,4]), 5)
g1 <- rep(mean(data[6:10, 4]), 5)
gm <- c(g0,g1)
data$wg.res <- data$y-gm

data$bg.res <- data$y-mean(data$y)

