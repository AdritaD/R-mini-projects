#input array
x <- c(4.79,10.89,6.54,22.15);
#length of input
n=length(x);


#negative of log likelihood function. as log likelihood by default gives minimization result.
neg.loglik.fun <- function(theta)
{
  
  result <- n*log(theta)-((theta-1)*(sum(log(x))))
  return(-result)
}


#optim function to optimise the log likelihood
ml.est<-optim(theta <- 0,neg.loglik.fun, hessian=TRUE)
ml.est

SE<- sqrt(diag(solve(ml.est$hessian)));
SE


alpha=0.05;
CI <- ml.est$par + c(-1, 1) * qnorm(1-(alpha/2)) *SE
CI
