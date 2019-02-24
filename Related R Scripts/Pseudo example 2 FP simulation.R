#Pseudo example 2, simulation

I=12 #number of litters
J=rep(c(2,2,4,5,6,7), 2) #number of individuals/litter
sim.diff=0 #null effect
sd.litter=1 #difference between litters
sd.res=0.5 #difference within litter

pseud <- function(){
  litter.ID=rep(c(1:I), times=J)
  animal.ID=sequence(J)
  treat.ID=rep(c(0,1), each=0.5*length(animal.ID)) #each group with parallel litter sizes
  
  s.mean.0=rep(rnorm(n=0.5*I, mean=0, sd=sd.litter), times=J[1:c(0.5*I)])
  s.mean.1=rep(rnorm(n=0.5*I, mean=sim.diff, sd=sd.litter), times=J[1:c(0.5*I)])
  s.y=rnorm(n=sum(J), mean=c(s.mean.0, s.mean.1), sd=sd.res)
  
  #Create Df
  s.data <- as.data.frame(cbind(litter.ID, animal.ID, treat.ID, s.y), row.names = NULL)
  
  #Type I errors
  p<- summary(lm(s.y~treat.ID, data=s.data))$coefficient[8]
  return(p)
}

sim.results<- do.call(rbind, lapply(1:1000, function(x) pseud())) #df of 1000 pvalues
head(sim.results)
sim.df <- as.data.frame(sim.results)
colnames(sim.df)=c("P-value from study")

#Calculate Type I Error rate
prop <- function(x){
  pr <- length(x[x<0.05])/1000 #1000--> number of rows
  return(pr)
}
type <- apply(sim.results, 2, prop)
type

hist(sim.results, 
     ylim=range(0,max(500)),
     xlab= "Reported p-value",
     ylab= "Number of Simulated Studies", 
     main="Incorrectly Rejecting the Null Hypothesis")



#GGPLOT SINGLE HISTOGRAM
ggplot(data=sim.df, aes(sim.df$`P-value from study`)) + 
  geom_histogram(binwidth=0.05, col="black", 
                 aes(fill=..count..)) +
  labs(title="Incorrectly Rejecting the Null Hypothesis", x="P-Value from study", y = "Number of simulated studies")+
  theme_classic()


colors <- c(rep("lightsteelblue3",1), rep("white",20))

ggplot(data=sim.df, aes(sim.df$`P-value from study`)) + 
  geom_histogram(fill=colors, binwidth=0.05, color="black") +
  labs( x="Saved P-value", y = "Number of studies")+
  theme_bw()+
  scale_x_continuous( expand = c(0, 0)) +
  scale_y_continuous( expand = c(0, 0)) +
  theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank())




