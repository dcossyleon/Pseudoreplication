#Pseudo Extreme Example: Reaction Times

I=4 #number of groups
J=5 #number of trials/observations (or individuals per group/litter/etc.)
diff=5
#diff=c(10, 4, 2, 1) If you were simulating multiple groups, you could specify diff between groups this way
sd.subject=8.44
sd.trials=1.42

  (subject.ID=rep(c(1:I), each=J))
  (trials.ID=rep(sequence(J), times=I))
  (sex=rep(c(0,1), each=I*J*0.5)) #Assumes balanced design
 
  
  #Generate the mean for each group
  (subject.0= rep(rnorm(n=0.5*I, mean=40, sd=sd.subject), each=J)) #Female 1 mean
  (subject.1= rep(rnorm(n=0.5*I, mean=40+diff, sd=sd.subject), each=J)) #Male mean
  
  (y.blink=rnorm(n= J*I, mean=c(subject.0, subject.1), sd=sd.trials))
  
  
  
  
#Create Df
blink.data <- as.data.frame(cbind(subject.ID, trials.ID, sex, y.blink), row.names = NULL)
  #write.csv(data, file="eyeblink_sim.csv", quote = FALSE)

blink.data$subject.ID <- c(rep(0.5,J), rep(1,J), rep(1.5, J), rep(2, J))
blink.data$y.blink[6:15] <- c(44.05611, 42.25860, 41.04987, 45.07797, 44.80733, 59.30283, 57.63582, 58.51698, 60.00853, 56.72297) 

plot(y.blink~jitter(subject.ID,0.05), data=blink.data, 
     xlab="Sex", ylab="Reaction time (ms)", xaxt="n", 
     cex= 1.5, cex.lab= 1.5, cex.axis= 1.75, #Text and circle size
     ylim= c(min(y.blink) -.5, max(y.blink) +5.5), 
     xlim=c(min(subject.ID)-0.5, max(subject.ID+0.5)))

ICCest(x=subject.ID, y=y.blink, data= blink.data, alpha=0.05)

#Read-in pre-prepared dataset with 4 subjects
#blink.data <- read.csv(file="~/Documents/Stats/Pseudoreplication/eyeblink_sim.csv")

blink.results <- lm(y.blink~sex, data=blink.data)
summary(blink.results)


#Plot
plot(y.blink~jitter(subject.ID,0.05), data=blink.data, xlab="Sex", ylab="Reaction time (ms)", xaxt="n", 
     ylim= c(min(y.blink) -5, max(y.blink) +5), xlim=c(-1,5))


#Intra-class correlation
#calc group (ie. subject) means
mean.0 <- rep(mean(blink.data[1:5,4]), J[1])
mean.1 <- rep(mean(blink.data[6:10, 4]), J[2])
group.mean <- c(mean.0,mean.1) #group mean: means for male subject, mean for female subject
blink.data$group.mean <- group.mean

#Calculate residual (Deviance) from means
within.group.resid <- blink.data$within.group.resid <- (blink.data$y.blink-group.mean) #from within-group residual
between.group.resid <- (blink.data$group.mean-mean(blink.data$y.blink))[c(1,6)] #group mean deviance from grand mean,
between.group.resid

#sum of squares
within.group.sumsquares <- sum(within.group.resid^2)
between.group.sumsquares <- sum(between.group.resid^2)

#divide by n-1, to convert to variance components
(wg.var <- wg.ss/9)
(bg.var <- bg.ss/1)

ICC.correct <-bg.var /(wg.var+bg.var)
ICC.correct

ICC.blink=var(blink.data$bg.res)/(var(blink.data$bg.res)+var(blink.data$wg.res)) #wrong

#Compare to ICC function
library(ICC)
ICCest(x=subject.ID, y=y.blink, data=blink.data, alpha=0.05)

#ICC=.9
#alpha=sqrt(ICC*(sd.trials^2)/(1-ICC))
#alpha

design_effect = 1 + (J-1)*ICC
N_subject <- (J)/design_effect
N_subject

N_eff=sum(N_subject)
N_eff

N_eff.per.group=N_eff*0.5
N_eff.per.group


#What's going on with my ICC
tmpbb <- anova(aov(y.blink ~ as.factor(subject.ID), data = blink.data))
num.df <- tmpbb$Df[1]
denom.df <- tmpbb$Df[2]
MSa <- tmpbb$"Mean Sq"[1]
MSw <- var.w <- tmpbb$"Mean Sq"[2]
a <- length(unique(blink.data$subject.ID))


tmp.outj <- aggregate(y.blink ~ as.factor(subject.ID), data = blink.data, FUN = length)$y
k <- (1/(a - 1)) * (sum(tmp.outj) - (sum((tmp.outj)^2)/sum(tmp.outj)))
var.a <- (MSa - MSw)/k
r <- var.a/(var.w + var.a)

#ANOVA "by hand"
mean.0 <- rep(mean(blink.data$y.blink[1:5]), times=5)
mean.1 <- rep(mean(blink.data$y.blink[6:10]), times=5)
blink.data$group.mean <- c(mean.0,mean.1) #group mean values


grand.mean = mean(blink.data$y.blink)
blink.data$grand.mean <- rep(grand.mean, 10)
resid.group.mean.grand.mean <- blink.data$group.mean-blink.data$grand.mean

groupSS <- sum(resid.group.mean.grand.mean^2)
groupSSmean <- groupSS/(2-1) #divided by df (g)


#aov
blink.anova <- aov(y.blink ~ as.factor(subject.ID), data = blink.data)
summary(blink.anova)

plot(y.blink~jitter(sex,0.05), xlab="Sex", ylab="Reaction time (ms)", xaxt="n", xlim=c(-0.5,1.5), cex=1.5, cex.lab=1.5, cex.axis=1.5, ylim=c(min(y.blink)-5, max(y.blink)+5))

