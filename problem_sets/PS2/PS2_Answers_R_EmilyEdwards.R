#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("/Users/eedwards/Desktop/GitHub/QTM200Spring2020/problem_sets/PS2")

# please please work

# Create a matrix to store the data for Question 1
Q1_data <-matrix(c(14,6,7, 27,7,7,1, 15, 21, 13, 8, 42),nrow=3,byrow = TRUE)
rownames(Q1_data)<-c("Upper class","Lower class", "Total columns")
colnames(Q1_data)<-c("Not Stopped","Bribe Requested","Stopped/given warning","Total rows")
# Calculate the X^2 statistic using the formula: [(observed-expected)/expected]. Observed values were the values reported in the data set. Expected values are calculated using the formula: [(row total/grand total)*column total]. 
expected1<- (27/42)*21
expected2<- (27/42)*13
expected3<-(27/42)*8
expected4<-(15/42)*21
expected5<-(15/42)*13
expected6<-(15/42)*8
chi2_1 <-((14-expected1)^2)/expected1
chi2_2 <-((6-expected2)^2)/expected2
chi2_3<-((7-expected3)^2)/expected3
chi2_4<-((7-expected4)^2)/expected4
chi2_5<-((7-expected5)^2)/expected5
chi2_6<-((1-expected6)^2)/expected6
chi2_statistic<-sum(chi2_1,chi2_2,chi2_3,chi2_4,chi2_5,chi2_6)
chi2_statistic
# SOLUTION: The chi square statistic is 3.791168

# Calculate the p-value for the test statistic. Degrees of freedom (df) is calculated using the formula: df = (rows-1)*(columns-1)
df<- (2-1)*(3-1)
pchisq(3.791168,df= df,lower.tail = FALSE)
# The p-value is 0.1502306.
# SOLUTION: The p-value is greater than the significance level (0.1502306>0.1). Therefore, fail to reject the null hypothesis (Ho= there is no association between class and bribery). There is not sufficient evidence to conclude that class and bribery are associated. 

# Calculate the standardized residuals using the formula: z = [(f.observed-f.expected)/se]. Calculate the standerd error (se) using the formula: sqrt[f.expected*(1-row prop)*(1-column prop)]
residual1<- (14-expected1)/(sqrt(expected1*(1-(27/42))*(1-(21/42))))
residual2<-(6-expected2)/sqrt(expected2*(1-(27/42)*(1-(13/42))))
residual3<-(7-expected3)/sqrt(expected3*(1-(27/42)*(1-(8/42))))
residual4<-(7-expected4)/sqrt(expected4*(1-(15/42)*(1-(21/42))))
residual5<-(7-expected5)/sqrt(expected5*(1-(15/42)*(1-(13/42))))
residual6<-(1-expected6)/sqrt(expected6*(1-(15/42)*(1-(8/42))))

# Standardized residuals show how far away each observed value is from the expected value. Based on the results, the"Not Stopped" individuals for both upper class and lower class individuals had observed values closest to the expected values (0.3220306 and -0.2014441, respectively). The lower class individuals who were stopped/given a warning deviated the most from the expected values (standard residual = -1.303106). The high standard residual for the lower class individuals who were stopped/given a warning might suggest that the null hypothesis (there is no association between class and bribery) is not true for this group. However, the residuals were still too low to reject the null hypothesis. 

# Null hypothesis (Ho):The reservation policy has no effect on the nuber of new or repaired drinking water facilities in the villages. 
# Alternative hypothesis (Ha): The reservation policy has an effect on the nuber of new or repaired drinking water facilities in the villages. 

# Bivariate regression by hand
regressMat <- as.data.frame(matrix(c(women$reserved,women$water), nrow = 322,byrow = FALSE))
colnames(regressMat)<- c("reserved","water")
regressMat
# Calculate sums and means
mean_reserved<- mean(regressMat$reserved)
mean_water<- mean(regressMat$water)
sum_reserved<-sum(regressMat$reserved)
sum_water<- sum(regressMat$water)
# Calculate beta (slope) and alpha (y intercept)
beta <- sum((women$reserved-mean_reserved)*(women$water-mean_water))/sum((women$reserved-mean_reserved)^2)
alpha <- mean_water-(beta*mean_reserved)
# Alpha (intercept) equals 14.73832, beta (slope) equals 9.252423
# Check with lm() in R
lm(women$water~women$reserved, data=regressMat)
# SOLUTION: # Alpha (intercept) equals 14.73832, beta (slope) equals 9.252423. (when checked with the lm() function, the R output was: Intercept =   14.738  women$reserved  = 9.252. This is consistent with the results I calculated). 

# The alpha (9.252) gives us the slope of the linear relationship. For every increase of 1 in regards to the GP being reserved for women, the number of new or repaired drinking-water facilities in the village increases by 9.252. If the GP being reserved for women is 0 (x=0), the y-intercept (beta) is 14.738. This is the number of new or repaired drinking-water facilities in the village if there were no reservations for women on the GP. 

# Summarize the fruitfly data, and use a histogram to examine the distribution
summary(fruitfly)
No          type      lifespan         thorax          sleep      
Min.   : 1   Min.   :1   Min.   :16.00   Min.   :0.640   Min.   : 1.00  
1st Qu.: 7   1st Qu.:2   1st Qu.:46.00   1st Qu.:0.760   1st Qu.:13.00  
Median :13   Median :3   Median :58.00   Median :0.840   Median :20.00  
Mean   :13   Mean   :3   Mean   :57.44   Mean   :0.821   Mean   :23.46  
3rd Qu.:19   3rd Qu.:4   3rd Qu.:70.00   3rd Qu.:0.880   3rd Qu.:29.00  
Max.   :25   Max.   :5   Max.   :97.00   Max.   :0.940   Max.   :83.00  
hist(fruitfly$lifespan, main = "Distribution of Lifespan", xlab = "Lifespan (days)")
# There is an approximately normal distribution of fruitflies based on their lifespan. The distribution is centered at the mean value of 57.44 days. 

# Plot lifespan vs thorax and calculate the correlation coefficient
plot(fruitfly$thorax,fruitfly$lifespan, xlab = "Length of thorax (mm)", ylab = "Lifespan (days)")
cor(fruitfly$thorax,fruitfly$lifespan,method = "pearson")
# The correlation coeffieient is  0.6364835. There is a moderate, positive linear association between the two variables. 

# Run a regression for the variables thorax and lifespan
lm1<- lm(fruitfly$lifespan~fruitfly$thorax)
lm1
# The slope is 144.33. This meand that for every 1mm increase in length of thorax, the lifespan of fruitflies increases by 144.33 days. 

# Use cor.test to test for the significance of the linear relationship between the two variables.
cor.test(fruitfly$lifespan,fruitfly$thorax)
Pearson's product-moment correlation

data:  fruitfly$lifespan and fruitfly$thorax
t = 9.1521, df = 123, p-value = 1.497e-15
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
0.5188709 0.7304479
sample estimates:
cor 
0.6364835 

# The p-value is 1.497e-15, which is less than a significance level of a=0.05 (1.497e-15<0.05). Since there is such a small p-value, reject the null hypothesis (Ho: there is no correlation between the length of the thorax and the lifespan of fruitflies). 

# Calculate the confidence interval for the slope at 0.90 significance using the formula: slope +/- zscore*standard error.
summary(lm1)
slope = 144.33
zscore = 1.645
standard error = 15.77
upper <- 144.33 + (1.645*15.77)
lower <-144.33 - (1.645*15.77)
# SOLUTION: The 90% confidence interval for the slope is (118.3884,170.2717)

# Use the command confint() to calculate the confidence interval for the slope at 0.90
confint(lm1, level = 0.90)
# SOLUTION: The 90% confidence interval for the slope is (118.19616, 170.4700)

# Calculate the prediction interval and confidence interval
new_fruitfly <- fruitfly
new_fruitfly$thorax<- 0.8
prediction_interval <- predict(lm(fruitfly$lifespan~fruitfly$thorax), newdata = new_fruitfly, se.fit = T, interval = "prediction", level= 0.90)
confidence_interval <- predict(lm(fruitfly$lifespan~fruitfly$thorax), newdata = new_fruitfly, se.fit = T, interval = "confidence", level= 0.90)

#Expected value of lifespan for an individual with thorax = 0.8
# from prediction interval: fit = 54.41478 lwr = 31.775371 upr = 77.05419
# from confidence interval: fit = 54.41478 lwr = 52.32539 upr = 56.50416

# Graph the confidence interval
plot(fruitfly$thorax,fruitfly$lifespan, xlab="Thorax Length (mm)", ylab="Lifespan (days)")
lines(fruitfly$thorax, fitted(lm1), col="blue")

#The blue line represents the fitted regression. I was unable to figure out how to graph the confidence intervals. 


