install.packages("car")
library(car)
data("Prestige")
help("Prestige")

#a)
# Recode professionals as 1, and blue and white as 0
Prestige$professional<- ifelse(Prestige$type=="prof", 1, 0)
Prestige$professional

#b)
# Use the lm function to run a linear model with prestige as the outcome variable (y), and income, professional, and their interactions as the explanatory variables (x)
Prestige_regress <- lm(prestige ~ income + professional+ income:professional, data = Prestige)
Prestige_regress
summary(Prestige_regress)

Call:
lm(formula = prestige ~ income + professional + income:professional,
data = Prestige)

Residuals:
Min      1Q  Median      3Q     Max
-14.852  -5.332  -1.272   4.658  29.932

Coefficients:
Estimate Std. Error t value Pr(>|t|)
(Intercept)         21.1422589  2.8044261   7.539 2.93e-11 ***
income               0.0031709  0.0004993   6.351 7.55e-09 ***
professional        37.7812800  4.2482744   8.893 4.14e-14 ***
income:professional -0.0023257  0.0005675  -4.098 8.83e-05 ***
---

Residual standard error: 8.012 on 94 degrees of freedom
(4 observations deleted due to missingness)
Multiple R-squared:  0.7872,    Adjusted R-squared:  0.7804
F-statistic: 115.9 on 3 and 94 DF,  p-value: < 2.2e-16

#c)
# Y =  beta0 + beta1 *x1 +beta2 *x2 + beta3*x1*x2
# beta0 = intercept
# beta1 = slope of the relationship between income and prestige
# x1 = income
# beta2 = slope of the relationship between professional and prestige
# x2 = professional
# beta3 = interaction between income and professional
# SOLUTION: Y = 21.1422589 + 0.0031709*x1 + 37.7812800*x2 + -0.0023257*x1*x2

#d)
# If all other variables are held constant, with every $1 increase in income, there is an average increase in the prestige score of 0.0031709.

#e)
# If all other variables are held constant, being a professional rather than a white collar or blue collar worker increases the presteige score by 37.7812800 on average.

#f)
# Calculate the prestige score when x1 = 0 and x2 = 1
(21.1422589) + (0.0031709*0) + (37.7812800*1) + (-0.0023257*0*1)= 58.92354
# Calculate the prestige score when x1 = 1000 and x2 = 1
(21.1422589) + (0.0031709*1000) + (37.7812800*1) + (-0.0023257*1000*1) = 59.76874
# Calculate the difference between the outcomes
59.76874-58.92354 = 0.8452
# SOLUTION: When professional is held constant at 1, a $1000 increase in income increases the prestige score by 0.8452 on average.

#g)
# Calculate the prestige score when x1=6000 and x2=0
(21.1422589) + (0.0031709*6000) + (37.7812800*0) + (-0.0023257*6000*0) = 40.16766
# Calculate the prestige score when x1=6000 and x2=1
(21.1422589) + (0.0031709*6000) + (37.7812800*1) + (-0.0023257*6000*1) = 63.99474
# Calculate the difference between the outcomes
63.99474- 40.16766 = 23.82708
# SOLUTION: When income is held constant at $6000, being a professional rather than a white collar or blue collar worker increases the presteige score by 23.82708 on average.
#a)
# Ho: There is no relationship between having yard signs and vote share (beta = 0)
# Ha: There is a relationship between having yard signs and vote share (beta does not equal 0)
# Calculate the test statistic using the equation: (beta-0)/standard error
test_statistic<- (0.042-0)/0.016
# test_statistic = 2.625
# Calculate the p value using the test_statistic = 2.625 and degrees of freedom = n-1 (n=30)
p_value<- 2*pt(2.625,df=29, lower.tail = F)
p_value = 0.01368397
# Given a significance level of 0.05, reject the null hypothesis that there is no relationship between having yard signs and vote share (0.01368397<0.05).

#b)
# Ho: There is no relationship between being adjacent to yard signs and vote share (beta = 0)
# Ha: There is a relationship between being adjacent to yard signs and vote share (beta = 0)
# Calculate the test statistic using the equation: (beta-0)/standard error
test_statistic_b <- (0.042-0)/0.013
test_statistic_b = 3.230769
# Calculate the p value using the test_statistic = 3.230769 and degrees of freedom = n-1 (n=76)
p_value_b<- 2*pt(3.230769,df=75,lower.tail = F)
p_value_b = 0.001834303
# Given a significance level of 0.05, reject the null hypothesis that there is no relationship between being adjacent to yard signs and vote share (0.001834303<0.05).

#c) Given that there are no yard signs in the precint or adjacent to the precint, the proportion of the vote that went to Cuccinelli is 0.302.
#d)
# The R squared value is 0.094. This means that the model explains 9.4% of the variation in vote share. This suggests that other variables might have a stronger prediction of the variability  within vote share than the presence of signs at/adjacent to the presinct.
