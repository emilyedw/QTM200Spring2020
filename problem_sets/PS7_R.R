#PS7
MexicoMuniData<-read.csv("MexicoMuniData.csv")
View(MexicoMuniData)
poisson_PAN<- glm(PAN.visits.06~competitive.district+marginality.06+PAN.governor.06, data = MexicoMuniData, family = "poisson")
summary(poisson_PAN)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.1441  -0.3596  -0.1742  -0.0783  15.2935  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)           -3.9304     0.1747 -22.503   <2e-16 ***
  competitive.district  -0.4594     0.3276  -1.402    0.161    
marginality.06        -2.0981     0.1210 -17.343   <2e-16 ***
  PAN.governor.06       -0.2073     0.1660  -1.249    0.212    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

Null deviance: 1433.83  on 2392  degrees of freedom
Residual deviance:  963.57  on 2389  degrees of freedom
(4 observations deleted due to missingness)
AIC: 1255.9

Number of Fisher Scoring iterations: 7

#The test statistic and p value for competitive.district are -1.402 and 0.161, respectively. Therefore, fail to reject the null hypothesis that whether or not a district is a safe seat affects the number of times that PAN presidential candidates visit.

exp(coef(poisson_PAN))
#The number of times the PAN presidential candidate visited an area decreased by a multiplicative factor of 0.1226841 as poverty increased. The number of times the PAN presidential candidate visited an area decreased by a multiplicative factor of 0.8127638 when the district had a PAN affiliated governor.  

num_visits<-exp(-3.9304+ (-0.4594*1)+(-2.0981*0)+(-0.2073*1))
num_visits
SOLUTION: 0.01008103. The average number of times the PAN presidential candidate will visit a district with these criteria is 0.01008103.

install.packages("lme4")
library(lme4)
View(sleepstudy)
View(sleep)

pooled<- lm(Reaction~Days, data = sleepstudy)
summary(pooled)

un_pooled<- lm(Reaction~Days+factor(Subject)-1,data = sleepstudy)
summary(un_pooled)
sleepstudy$UPF<- fitted.values(un_pooled)

un_pooled_time<- lm(Reaction~Days:factor(Subject)-1,data = sleepstudy)
sleepstudy$UPT<- fitted.values(un_pooled_time)

un_pooled_day_patient<- lm(Reaction~Days+Subject+Days:factor(Subject), data = sleepstudy)
sleepstudy$UPDP<- fitted.values(un_pooled_day_patient)

semi_pooled <- lmer(Reaction~Days+(Days|Subject), data = sleepstudy)
summary(semi_pooled)
