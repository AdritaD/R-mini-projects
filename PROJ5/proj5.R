# use install.packages("boot") to first install 
# the package and then load it
library(boot);

# for generating data: true mean
lambda <- 0.01   
#sample mean
mu<-(1/lambda)
#value of alpha
alpha<-0.05
# sample size
N    <- 5                 


###########################
# Nonparametric Bootstrap #
###########################
x<-function(data,indices)
{
  result <- mean(data[indices])
  return(result)
}

mean_CI.bootstrap<-function(n,lambda,alpha){
  data<-rexp(n=N,rate=lambda)
  mean.npar.boot<-boot(data,x,R=999,sim='ordinary',stype="i")
  result<-boot.ci(mean.npar.boot,conf = 1-alpha)
  return(result$percent[c(4,5)])
}

ci_bootStrap.mat<-replicate(5000,mean_CI.bootstrap(n,lambda,alpha))
mean((mu>=ci_bootStrap.mat[1,])*(mu<=ci_bootStrap.mat[2,]))



###################
# Z statistic CI #
###################

conf.int <- function(mu,N) {
  y <-rexp(N, lambda)
  sd<-sd(y)
  ci <- mean(y) + c(-1, 1) * qnorm(1 - (alpha/2)) * sd/sqrt(N)
  return(ci)
}


ci.mat <- replicate(5000, conf.int(mu,N))
mean((mu>=ci.mat[1,])*(mu<=ci.mat[2,]))

z<-qnorm(1 - (alpha/2))
SE<-sd/(sqrt(N))
N=((z*sd)/(2*SE))^2
N
