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
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Calculate the mean of the sample, y, and save it as sample.mean
mean(y)
sample.mean<-mean(y)
#Calculate the standard deviation of the sample, y, and save it as sample.sd
sd(y)
sample.sd<-sd(y)
# Calculate the standard error of the mean by dividing the standard deviation by the square root of the sample size. Save as sample.stderror
sample.sd/(sqrt(25))
sample.stderror<-sample.sd/(sqrt(25))
#Calculate the t statistic for a 90% confidence interval using 25 df. Save as sample.t
qt(1-.1/2,df=25)
sample.t<-qt(1-.1/2,df=25)
# Calculate the confidence interval using the formula mean +/- tvalue * standard error
sample.mean+(sample.t*sample.stderror)
sample.mean-(sample.t*sample.stderror)
# SOLUTION: (93.96711,102.9129). With 90% certainty, the true mean of the students' IQ falls between 93.96711 and 102.9129

#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Ho: there is no difference between the average IQ of students in the counselor's school and the average IQ of all students in the country (mean = 100)
# Ha: The average IQ of students in the counselor's school is higher than the average IQ of all students in the country (mean > 100) *this is a one-sided test
# The significance level is 0.05 (alpha=0.05)
# Calculate the mean of the sample, y, and save it as sample.mean
mean(y)
sample.mean<-mean(y)
#Calculate the standard deviation of the sample, y, and save it as sample.sd
sd(y)
sample.sd<-sd(y)
# Calculate the standard error of the mean by dividing the standard deviation by the square root of the sample size. Save as sample.stderror
sample.sd/(sqrt(25))
sample.stderror<-sample.sd/(sqrt(25))
# Calculate the test statistic by subtracting the population mean from the sample mean and then dividing by the standard error.
(sample.mean-100)/sample.stderror
# Calculate the p value given the test statistic of -0.5957439 and 24 degrees of freedom
qt(abs(-0.5957439), df=24)
# SOLUTION: p= 0.2450352, so fail to reject the null hypothesis. There is not enough evidence to reject the claim that the student's at the counselor's school have the same average IQ as the average IQ of all students.

#####################
# Problem 3
#####################

y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)

expenditure <- read.table("expenditure.txt", header=T)

# Use scatter plots to determine the relationship between 2 quantitative variables
# Use a scatter plot to determine the relationship between per capita personal income and per capita expenditure on public education
plot(expenditure$X1,expenditure$Y, main = "Relationship Between Per Capita Personal Income and Per Capita Expenditure on Public Education", xlab = "Per Capita Personal Income", ylab = "Per Capita Expenditure on Public Education")
# SOLUTION: There is a moderate, positive, linear correlation between these two variables (X1 and Y)

# Use a scatter plot to determine the relationship between the number of residents per thousand under 18 years of age and per capita expenditure on public education
plot(expenditure$X2,expenditure$Y, main = "relationship between the number of residents per thousand under 18 years of age and per capita expenditure on public education", xlab = "number of residents per thousand under 18 years of age", ylab = "Per Capita Expenditure on Public Education")
# SOLUTION: There is no correlation between these two variables (X2 and Y)

# Use a scatter plot to determine the relationship between the number of people per thousand residing in urban areas and per capita expenditure on public education
plot(expenditure$X3,expenditure$Y, main = "relationship between the number of people per thousand residing in urban areas and per capita expenditure on public education", xlab = "number of people per thousand residing in urban areas", ylab = "Per Capita Expenditure on Public Education")
#SOLUTION: There is a weak, positive correlation between these two variables (X3 and Y)

# Use a boxplot plot to determine the relationship between the region and per capita expenditure on public education
# First, recode the region as a factor variable
expenditure$Region_Names<- factor(expenditure$Region, levels = c("Northeast","North Central","South","West"))
expenditure$Region_Names[expenditure$Region==1]<- "Northeast"
expenditure$Region_Names[expenditure$Region==2]<- "North Central"
expenditure$Region_Names[expenditure$Region==3]<- "South"
expenditure$Region_Names[expenditure$Region==4]<- "West"
# Next, plot the relationship between Region_Names and Y
plot(expenditure$Region_Names,expenditure$Y, main = "relationship between the region and per capita expenditure on public education", xlab = "region", ylab = "Per Capita Expenditure on Public Education")
# SOLUTION: On average, the West has the highest per capita expenditure on public education

# Use a scatter plot to determine the relationship between per capita personal income and per capita expenditure on public education. Color code based on region.
plot(expenditure$X1,expenditure$Y, main = "Relationship Between Per Capita Personal Income and Per Capita Expenditure on Public Education", xlab = "Per Capita Personal Income", ylab = "Per Capita Expenditure on Public Education", col= c(expenditure$Region))
# SOLUTION: The correlation between per capita personal income and per capita expenditure on public education is strongest for the South and the West. There is little correlation for North Central and Northeast.
