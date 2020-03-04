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

lapply(c("faraway"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS5")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)


# Plot the resitual vs the fitted values
plot(model1, which=1)
# The values of y conditioned on each value of x do not appear to have the same standard deviation at each x value. There is much greater variance around 60. As a result, the constant variance assumption appears to be violated.

# Use a qqplot to graph the distribution of the studentized residuals conditional on the fitted values of the regression model1
plot(model1, which=2)
#Since the values do not all follow the same linear trend, there is evidence that the values of the studentized residuals conditioned on the fitted values do not follow a normal distribution. 


# Calculate the hat values
hat <- lm.influence(model1)$hat
# Plot the hat values
plot(hat)
# Use the sum function to determine the number of predictors in "hat." This is the k value
sum(hat)
# The k value is 5. There are 47 observations. Use 2(5+1)/47 and 3(5+1)/47 as a guide for high leverage
abline(h=2*(5+1)/47, lty=2)
abline(h=2*(5+1)/47, lty=2)
# Two points have high leverage using the threshold 2(5+1)/47 and 3(5+1)/47. They have potential to greatly influence the fitted model.

outlierTest(model1)
rstudent unadjusted p-value Bonferroni p
24 6.016116         4.1041e-07   1.9289e-05
# The p-value is smaller than the significance level of 0.05, so reject the null hypothesis that there are no outliers. 


plot(hatvalues(model1),rstudent(model1), type = "n")
cook<- sqrt(cooks.distance(model1))
points(hatvalues(model1),rstudent(model1),cex=10*cook/max(cook))
abline(h=c(-2,0,2), lty=2)
abline(v=c(2,3)*3/45, lty=2)
identify(hatvalues(model1),rstudent(model1), row.names(gamble))

plot(model1, which=2)
# Point 24 has the largest influence on the dataset because of its large Cook's distance and standardized residuals. 
