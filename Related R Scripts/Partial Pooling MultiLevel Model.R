≠#July 2, 2018
#Partial Pooling from multi-level model

#Multi-level estimate for a given litter

library(nlme)

I=12 #number of litters
J=rep(c(2,2,4,5,6,7), 2) #number of individuals/litter
diff=0.6 #treatment effect
sd.litter=1 #difference between litters
sd.res=0.5 #difference within litter

litter.ID=rep(c(1:I), times=J)
animal.ID=sequence(J)
treat.ID=rep(c(0,1), each=0.5*length(animal.ID)) #each group with parallel litter sizes

mean.0=rep(rnorm(n=0.5*I, mean=0, sd=sd.litter), times=J[1:c(0.5*I)])
mean.1=rep(rnorm(n=0.5*I, mean=diff, sd=sd.litter), times=J[1:c(0.5*I)])
y=rnorm(n=sum(J), mean=c(mean.0, mean.1), sd=sd.res)

data <- as.data.frame(cbind(litter.ID, animal.ID, treat.ID, y), row.names = NULL)


avg <- aggregate( y ~ litter.ID, data, mean)
err <- aggregate( y ~ litter.ID, data, sem)
new <- merge(avg, err, by="litter.ID")
colnames(new) <- c("litter.ID", "litter.avg", "sem")
new$group <- rep(c(0,1), each=I*0.5)

data$litter.averages <- rep(new$litter.avg, J)
data$wg.resids <- data$y-data$litter.averages #litter ob - litter mean
    #data$wg.resids.sq <- data$wg.resids^2 #residuals squared
    #wg.ss <- sum(wg.resids.sq[data$treat.ID==t])#within group sum of squares
Var.j <- var(data$wg.resids)


treat.avg <- aggregate(y ~ treat.ID, data, mean)
mean.treat <- treat.avg$y
new$group.avg <- rep(mean.treat, each=I*0.5)
new$litter.N <- J
new$bg.resid <- new$litter.avg-new$group.avg #litter mean - group mean
    #new$bg.resid.sq <- new$bg.resid^2 #square the diff
    #Var.all <- sum(new$litter.N[new$group==t]*new$bg.resid.sq[new$group==t])/((I*0.5)-1)#between group Var
Var.all <- var(new$bg.resid)

t=1 #specifies group

#all <- data$y[data$treat.ID==t] #subsets treat group
#mean.all <- mean(all)
#mean.all

multilevel.est.0 <- function(x){
  j <- data$y[data$litter.ID==x] #select correct obs by litter
  n.j <- length(j) #sample size of j litter
  mean.j <- mean(j) #mean of observations in this litter
  true.litter.j.est <- ((n.j/Var.j)*mean.j + (1/Var.all)*mean.all) / ((n.j/Var.j) + (1/Var.all))
  return(true.litter.j.est)
}

results.0<- do.call(rbind, lapply(1:(I*0.5), function(x) multilevel.est.0(x))) #df of all treat.0 multilevel estimates
results.0


multilevel.est.1 <- function(x){
  j <- data$y[data$litter.ID==x] #select correct obs by litter
  n.j <- length(j) #sample size of j litter
  mean.j <- mean(j) #mean of observations in this litter
  true.litter.j.est <- ((n.j/Var.j)*mean.j + (1/Var.all)*mean.all) / ((n.j/Var.j) + (1/Var.all))
  return(true.litter.j.est)
}



results.1<- do.call(rbind, lapply((I*0.5+1):I, function(x) multilevel.est.1(x))) #df of all treat.0 multilevel estimates
results.1

#combine treat groups
new$shrunk <-  c(results.0, results.1)

#Regress
d.fit <- summary(lm(shrunk~group, data=new))
d.fit

#Compare to lme model
  fit <- lme(fixed=y~treat.ID, random=~1|litter.ID, data=data)
  summary(fit)
  ranef(fit) #distance from the intercept (each litter's distance from its group mean)
  d<- coef(fit) #litter shrunken means (need to at the Beta value (treat.ID) to get it for treat.1)
  fixef(fit)
  #my values should match coef(fit), but after adding for treat.1
  
  
 m.sh0 <- coef(fit)[1:6,1]
 m.sh1 <- (coef(fit)[,1][7:12]+coef(fit)[,2])[7:12]
 fit.sh.mean<- c(m.sh0, m.sh1) #model's shrunken litter means
  
 
 #Calc new residuals
 sh.mean.0 <- mean(new$shrunk[1:6])
 sh.mean.1 <- mean(new$shrunk[7:12])
 new$sh_mean <-rep(c(sh.mean.0, sh.mean.1), each=6)
 new$sh_resid <- new$shrunk-new$sh_mean
 new$fit.sh.mean <- fit.sh.mean #add model shrunken means to 'new'
 
 