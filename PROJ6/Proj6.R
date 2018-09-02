crime<-read.csv("C:/Users/adrit/Desktop/utd/sem2/stats for ds/project/PROJ6/crime.csv")
head(crime)
str(crime)
attach(crime)

crime.reg <- lm (murder.rate ~  poverty + high.school + college + single.parent + unemployed + metropolitan + region)
summary(crime.reg)


anova(crime.reg)
confint(crime.reg)

fitted(crime.reg)
resid(crime.reg)



#is poverty significant
fit1 <- update(crime.reg, . ~ . - poverty)
anova(crime.reg,fit1)

#is high school significant
fit2 <- update(crime.reg, . ~ . - high.school)
anova(fit2, crime.reg)


#is college significant
fit3 <- update(crime.reg, . ~ . - college)
anova(crime.reg,fit3)

#is single parent significant
fit4 <- update(crime.reg, . ~ . - single.parent)
anova(crime.reg,fit4)

#is unemployed significant
fit5 <- update(crime.reg, . ~ . - unemployed)
anova(crime.reg,fit5)

#is region significant
fit6 <- update(crime.reg, . ~ . - region)
anova(crime.reg,fit6)

#is metropolitan significant
fit7 <- update(crime.reg, . ~ . - metropolitan)
anova(crime.reg,fit7)


#reduced model:
reduced<-lm (murder.rate ~  single.parent + region + metropolitan)

summary(reduced)
anova(reduced, crime.reg)





summary(crime)
mean(single.parent)
mean(metropolitan)
regionTable <- table(region)
names(regionTable[which.max(regionTable)])
beta_0  = -8.44469 
beta_single.parent <- 0.47472
beta_metropolitan <- 0.03627
gamma_regionNortheast <- -2.29258   
gamma_regionSouth <- 0.51237    
gamma_regionWest  <- -0.24384
x_single.parent = mean(single.parent)
x_beta_metropolitan = mean(metropolitan)
x_gamma_regionSouth =1
x_gamma_regionWest = 0
x_gamma_regionNortheast =0

Y= beta_0 + beta_single.parent * x_single.parent + 
  beta_metropolitan * x_beta_metropolitan + 
  gamma_regionNortheast * x_gamma_regionNortheast + 
  gamma_regionSouth * x_gamma_regionSouth + 
  gamma_regionWest   * x_gamma_regionWest
Y


#AIC:
x<-glm(reduced)
summary(x)

#forward AIC:
x<-step(lm(murder.rate~1), reduced ~ poverty + high.school + college + single.parent + unemployed + metropolitan + region,direction = "forward")

#backward AIC:
reduced.n<-nrow(crime)
step(lm(murder.rate ~  poverty + high.school + college + single.parent + unemployed + metropolitan + region),
     direction = "backward", k=log(reduced.n))


#comparing forward and reduced
anova(reduced,x)

