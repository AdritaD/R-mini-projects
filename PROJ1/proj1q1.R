#isim<--5
rm(list=ls(all=TRUE))
z<- matrix(data=0,nrow=5,ncol=3)
s<-matrix(data=0,nrow=5,ncol=3)
t<- matrix(data=0,nrow=5,ncol=3)
for(isim in 1:5)
{
  x<-1:4
  n<-1000
  a=sample(x,n,replace=TRUE)
  m=mean(a)
  sd=sqrt(var(a))
  a<-rnorm(a,m,sd)
  y<-sum(a[a<=2])
  prob=y/n
  z[isim,]<-c(mean(a),var(a),prob)
  
}
  #sample size 5000
for(isim in 1:5)
{
  x<-1:4
  n<-5000
  a1=sample(x,n,replace=TRUE)
  m1=mean(a1)
  sd1=sqrt(var(a1))
  a1<-rnorm(a1,m1,sd1)
  y1<-sum(a1[a1<=2])
  prob=y1/n
  c(mean(a1),var(a1),prob)
  s[isim,]<-c(mean(a1),var(a1),prob)
  
}
  
  #sample size 10000
  for(isim in 1:5)
  {
    x<-1:4
  n<-10000
  a2=sample(x,n,replace=TRUE)
  m2=mean(a2)
  sd2=sqrt(var(a2))
  a2<-rnorm(a2,m2,sd2)
  y2<-sum(a2[a2<=2])
  prob=y2/n
  c(mean(a2),var(a2),prob)
  t[isim,]<-c(mean(a2),var(a2),prob)
  
  }

z
s
t

  