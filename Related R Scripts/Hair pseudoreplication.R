#___Hair isotopes___
#scenario, 50 individuals, each with varying #of hairs


I=50 #number of individuals
J=c(2,4,6,8) #number of hairs
diff=0
sd.ind=0
sd.hair=0.2
sd.error=0.2

ind.ID=rep(c(1:I), each=J)
hair.ID=rep(c(1:J), times=I)
sex=rep(c(0,1), each=I*0.5*J)

pseud <- function(J){
  ind.ID=rep(c(1:I), each=J)
  hair.ID=rep(c(1:J), times=I)
  sex=rep(c(0,1), each=I*0.5*J)
  
  #treat 0
  ind.0=rep(rnorm(n=0.5*I, mean=0, sd=sd.ind), each=J)
  hair.0=rnorm(n=J*0.5*I, mean=0, sd=sd.hair)
  error.0=rnorm(n=J*0.5*I, mean=0, sd=sd.error)
  response.0 <- ind.0+hair.0+error.0
  
  #treat 1
  ind.1=rep(rnorm(n=0.5*I, mean=diff, sd=sd.ind), each=J)
  hair.1=rnorm(n=J*0.5*I, mean=diff, sd=sd.hair) #mean=
  error.1=rnorm(n=J*0.5*I, mean=diff, sd=sd.error)
  response.1 <- ind.1+hair.1+error.1
  
  response <- c(response.0, response.1)
  df <- as.data.frame(cbind(ind.ID, hair.ID, sex, response))
  
  #Type I errors
  p<- summary(lm(response~sex, data=df))$coefficient[8]
  return(p)
}

results<- do.call(rbind, lapply(1:1000, function(x) pseud())) #df of 1000 pvalues
results

results<- t(do.call(cbind, mclapply(1:1000, function(x) do.call(rbind, lapply(J, function(x) pseud(x))), mc.cores = 4, mc.cleanup = TRUE)))

#Calculate Type I Error rate
prop <- function(x){
  pr <- length(x[x<0.05])/1000 #1000--> number of rows
  return(pr)
}
type <- apply(results, 2, prop)
type

  
  