# use install.packages("boot") to first install 
# the package and then load it
library(boot);

#i)read file college.csv
advertisement<-read.csv("C:/Users/adrit/Desktop/utd/sem2/stats for ds/project/PROJ4/Advertising.csv");

#ii)scatterplots of sales vs TV
plot(advertisement$sales,advertisement$TV)

#iii)scatterplots of sales vs radio
plot(advertisement$sales,advertisement$radio)

#iv)population correlation of sales vs TV
c1<-cor(advertisement$sales,advertisement$TV) 

#finding bootstrap samples for correlations between sales and TV
x<-function(advertisement,X)
{
  y<-sample(advertisement$X,replace=T)
  result1<-advertisement$TV[y]
  result2<-advertisement$sales[y]
  c3<-cor(result2,result1)
  return(c3)
}
correlation.npar.boot<-boot(advertisement,x,R=999,sim='ordinary',stype="i")
#Call to find estimate ,bootstrap bias and SE for correlations between sales and TV
boot(data = advertisement, statistic = x, R = 999, sim = "ordinary", stype = "i")

#Percentile bootstrap method for correlations between sales and TV
sort(correlation.npar.boot$t)[c(25, 975)]


#v)population correlation of sales vs radio
c2<-cor(advertisement$sales,advertisement$radio)
#finding bootstrap samples for correlations between sales and TV
x1<-function(advertisement,X)
{
  y1<-sample(advertisement$X,replace=T)
  result2<-advertisement$sales[y1]
  result3<-advertisement$radio[y1]
  c4<-cor(result2,result3)
  return(c4)
}
correlation1.npar.boot<-boot(advertisement,x1,R=999,sim='ordinary',stype="i")

#Call to find estimate, bootstrap bias and SE for correlations between sales and radio
boot(data = advertisement, statistic = x1, R = 999, sim = "ordinary", stype = "i")

#Percentile bootstrap method for correlations between sales and radio
sort(correlation1.npar.boot$t)[c(25, 975)]

