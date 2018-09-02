#2c)MC estimation of E(X), Var(X),P(X<=2) for sample size 1000
  #possible values if X
  x<-1:4
  
  #sample size n
  n<-1000
  
  #taking a sample of the given distribution
  a=sample(x,n,replace=TRUE)
  
  #calculating mean of distribution
  m=mean(a)
  
  #calculating sd of distribution
  sd=sqrt(var(a))
  
  #calculating p(x<=2) of distribution
  a<-rnorm(a,m,sd)
  y<-sum(a[a<=2])
  prob=y/n
  
  #replicating final result 1000 times 
  z=replicate(5,c(mean(a),var(a),prob))
  z
  
  
  

  
  #2d)MC estimation of E(X), Var(X),P(X<=2) for sample size 5000
  #possible values if X
  x<-1:4
  
  #sample size n
  n<-5000
  
  #taking a sample of the given distribution
  a1=sample(x,n,replace=TRUE)
  
  #calculating mean of distribution
  m1=mean(a1)
  
  #calculating sd of distribution
  sd1=sqrt(var(a1))
  
  #calculating p(x<=2) of distribution
  a1<-rnorm(a1,m1,sd1)
  y1<-sum(a1[a1<=2])
  prob1=y1/n
  
  #replicating final result 5000 times 
  p=replicate(5,c(mean(a1),var(a1),prob))
  p
  
  
  
  
  #2d)MC estimation of E(X), Var(X),P(X<=2) for sample size 10000
  #possible values if X
  x<-1:4
  
  #sample size n
  n<-10000
  
  #taking a sample of the given distribution
  a2=sample(x,n,replace=TRUE)
  
  #calculating mean of distribution
  m2=mean(a2)
  
  #calculating sd of distribution
  sd2=sqrt(var(a2))
  
  #calculating p(x<=2) of distribution
  a2<-rnorm(a2,m2,sd2)
  y2<-sum(a2[a2<=2])
  prob=y2/n
  
  #replicating final result 10000 times 
  q=replicate(5,c(mean(a2),var(a2),prob))
  q
  
  
  
  
  
  
  
  #2b)finding qqplot of a (n,p) pair
    # n=100 p=0.5
    
  #take a random sample, take it's mean. replicate this 500 times 
    
    p1 <- replicate(500, mean(x=rbinom(100, 1, 0.5)))
    qqnorm(p1)
    
    
    
    
    #2C)
    par(mfrow=c(2,2))
    p1 <- replicate(500, mean(rbinom(10, 1, 0.1)))
    p2 <- replicate(500, mean(rbinom(10, 1, 0.25)))
    p3 <- replicate(500, mean(rbinom(10, 1, 0.5)))
    p4 <- replicate(500, mean(rbinom(10, 1, 0.75)))
    p5 <- replicate(500, mean(rbinom(10, 1, 0.9)))
    p6 <- replicate(500, mean(rbinom(30, 1, 0.1)))
    p7 <- replicate(500, mean(rbinom(30, 1, 0.25)))
    p8 <- replicate(500, mean(rbinom(30, 1, 0.5)))
    p9 <- replicate(500, mean(rbinom(30, 1, 0.75)))
    p10 <- replicate(500, mean(rbinom(30, 1, 0.9)))
    p11 <- replicate(500, mean(rbinom(50, 1, 0.1)))
    p12 <- replicate(500, mean(rbinom(50, 1, 0.25)))
    p13 <- replicate(500, mean(rbinom(50, 1, 0.5)))
    p14 <- replicate(500, mean(rbinom(50, 1, 0.75)))
    p15 <- replicate(500, mean(rbinom(50, 1, 0.9)))
    p16 <- replicate(500, mean(rbinom(100, 1, 0.1)))
    p17 <- replicate(500, mean(rbinom(100, 1, 0.25)))
    p18 <- replicate(500, mean(rbinom(100, 1, 0.5)))
    p19 <- replicate(500, mean(rbinom(100, 1, 0.75)))
    p20 <- replicate(500, mean(rbinom(100, 1, 0.9)))
    qqnorm(p1)
    qqnorm(p2)
    qqnorm(p3)
    qqnorm(p4)
    qqnorm(p5)
    qqnorm(p6)
    qqnorm(p7)
    qqnorm(p8)
    qqnorm(p9)
    qqnorm(p10)
    qqnorm(p11)
    qqnorm(p12)
    qqnorm(p13)
    qqnorm(p14)
    qqnorm(p15)
    qqnorm(p16)
    qqnorm(p17)
    qqnorm(p18)
    qqnorm(p19)
    qqnorm(p20)
    
    
  
  
    
    
    
  
  