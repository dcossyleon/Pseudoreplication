# VPA Example Litters/ Pseudoreplication

library(nlme)

I=12 #total number of litters
J=rep(c(2,2,4,5,6,7), 2) #number of individuals/litter
diff= 1 #0.5 #treatment effect
sd.litter=1 #difference between litters
sd.res=0.5 #difference within litter

litter.ID=rep(c(1:I), times=J)
animal.ID=sequence(J)
treat.ID=rep(c(0,1), each=0.5*length(animal.ID)) #each group with parallel litter sizes

mean.0=rep(rnorm(n=0.5*I, mean=0, sd=sd.litter), times=J[1:c(0.5*I)])
mean.1=rep(rnorm(n=0.5*I, mean=diff, sd=sd.litter), times=J[1:c(0.5*I)])
y=rnorm(n=sum(J), mean=c(mean.0, mean.1), sd=sd.res) #sample 

#Create Df
data <- as.data.frame(cbind(litter.ID, animal.ID, treat.ID, y), row.names = NULL)
      #VPA1 <- data #sd.litter =1, sd.res=0.5, diff=0.5
    #write.csv(VPA1, file="~/Documents/New R Projects/VPA1.csv", quote=FALSE)

results <- lm(y~treat.ID, data=data)
summary(results)

  
  sem <- function(x){
    sd(x)/sqrt(length(x))
  }
  
  avg <- aggregate( y ~ litter.ID, data, mean)
  err <- aggregate( y ~ litter.ID, data, sem)
  new <- merge(avg, err, by="litter.ID")
    colnames(new) <- c("litter.ID", "litter.avg", "sem")
    new$group <- rep(c(0,1), each=I*0.5)
    

#The right way: Use 1 AVG value instead/ group, N=12
  results_right <- lm(litter.avg~group, data=new)
  summary(results_right)

  
#Plot wrong
  plot(litter.ID, y, xlab="Litter ID, N=52", ylab="Mean", ylim=range(c(data$y-1), c(data$y+1)), cex=1.5, cex.lab=1.5, cex.axis=1.5, xaxt="none")
  axis(1, seq(1,12,1), cex.axis=1.5)
  #LABEL ALL TICKS
  #with SEM
  plot(new$litter.ID, new$litter.avg,
       ylim=range(c(data$y-1), c(data$y+1)),
       pch=20, xlab="Litter ID", ylab="Mean +/- SEM", cex=1.5, cex.lab=1.5, cex.axis=1.5, xaxt="none")
  
  arrows(new$litter.ID, new$litter.avg-new$sem, new$litter.ID, new$litter.avg+new$sem, length=0.05, angle=90, code=3)
  axis(1, seq(1,12,1), cex.axis=1.5)

#Plot mean values/ group only
  plot(new$litter.ID, new$litter.avg)
    #new$err_right.0 <- sem(new$litter.avg[1:6]) #Std Error should not be calculated here?
    #new$err_right.1 <- sem(new$litter.avg[7:12])
  
  plot(new$litter.ID, new$litter.avg,
       ylim=range(c(data$y-1), c(data$y+1)),
       pch=20, xlab="Litter ID, N=12", ylab="Mean", cex=1.5, cex.lab=1.5, cex.axis=1.5, xaxt="none")
        axis(1, seq(1,12,1), cex.axis=1.5)
  
library(ICC)
        ICCbare(x=litter.ID, y=y, data=data) #using MCMC estimation
        ICCest(x=litter.ID, y=y, data=data, alpha = 0.05) #using 1-way ANOVA
  
#ICC
  #resids <- 
  g0 <- rep(new$litter.avg), )
  g1 <- rep(mean(data[6:10, 4]), I*0.5)
  gm <- c(g0,g1) 
  data$wg.res <- data$y-gm  #ob- group mean
  data$bg.res <- c(data$y[1:26]-(mean(data$y[1:26])), data$y[27:52]-(mean(data$y[27:52])))  #ob - grand mean
  
  

  
  
  ICC=var(data$bg.res)/(var(data$bg.res)+var(data$wg.res))
  ICC.VPA = sd.litter^2/((sd.litter)^2+(sd.res)^2)
  
  
  
  N_eff.per.group=N_eff*0.5
  N_eff.per.group
  
  
  #resids
  g0 <- rep(mean(data[1:5,4]), I*0.5)
  g1 <- rep(mean(data[6:10, 4]), I*0.5)
  gm <- c(g0,g1)
  data$wg.res <- data$y-gm
  
  data$bg.res <- data$y-mean(data$y)


#Shrunken Values
  library(nlme)
  fit <- lme(fixed=y~treat.ID, random=~1|litter.ID, data=data)
  summary(fit)
  
  
  ranef(fit) #distance from the intercept (each litter's distance from its group mean)
  d<- coef(fit) #litter shrunken means (need to add the Beta value (treat.ID) to get the shrunken means for individuals that received treat.1)
  fixef(fit) #To get the Beta value
  #my values should match coef(fit), but after adding for treat.1
  

  (s <- d[7:12,1]) #VAlues that need the Beta value added to them to get their shrunken mean
  (s1 <- s+rep(1.119782,6)) #Adding the Beta value
  (f <- d[1:6, 1]) #getting the shrunken values that don't need anything added to them
  (d1 <- c(f,s1)) #Combines to get litter shrunken means
  

#There's a more efficient way to do the steps above:
  shrunken.litter.means <- c(d$`(Intercept)`[1:6], d$`(Intercept)`[7:12] + d$treat.ID[7:12])
  new$shrunken.litter.means <- shrunken.litter.means #adds to df with the SEM etc.
  
#Shrunken Plot
  plot(new$litter.ID, shrunken.litter.means, ylim=range(-2, 1.5), 
       pch=20, xlab="Litter ID, N=12", ylab="Shrunken Mean", cex=1.5, cex.lab=1.5, cex.axis=1.5, xaxt="none")
  axis(1, seq(1,12,1), cex.axis=1.5)
  segments(0.75,mean(d1[1:6]),6.25,mean(d1[1:6]), col="red", lty=2, lwd=2)
  segments(6.75, mean(d1[7:12]), 12.25, mean(d1[7:12]), col="red", lty=2, lwd=2)
  

  plot(new$litter.ID, d1, ylim=range(-2, 1.5), 
          pch=20, xlab="Litter ID, N=12", ylab="Shrunken Mean", cex=1.5, cex.lab=1.5, cex.axis=1.5, xaxt="none")
  axis(1, seq(1,12,1), cex.axis=1.5)
  segments(0.75, mean(d1[1:6]), 6.25,mean(d1[1:6]), col="red", lty=2, lwd=2)
  segments(6.75, mean(d1[7:12]), 12.25, mean(d1[7:12]), col="red", lty=2, lwd=2)
  
#Un-shrunken Average Plot
  plot(new$litter.ID, new$litter.avg,
       ylim=range(-2, 1.5),
       pch=20, xlab="Litter ID, N=12", ylab="Mean", cex=1.5, cex.lab=1.5, cex.axis=1.5, xaxt="none")
  axis(1, seq(1,12,1), cex.axis=1.5)
  
  segments(0.75,mean(d1[1:6]),6.25,mean(d1[1:6]), col="red", lty=2, lwd=2)
  segments(6.75, mean(d1[7:12]), 12.25, mean(d1[7:12]), col="red", lty=2, lwd=2)
  