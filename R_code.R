rm(list=ls())
ls()

x=rnorm(100) 
y=rnorm(100) 
plot(x,y,xlab="this is the x-axis",
     ylab="this is the y-axis", main="Plot of X vs Y")

###########################################################################

## Dataset control

#install.packages("ISLR")
library(ISLR)
summary(Auto)
fix(Auto)
dim(Auto)
names(Auto)
rownames(Auto)
row = nrow(Auto)
column = ncol(Auto)

attach(Auto)

plot(cylinders , mpg, xlab = "this is the x-axis",
     ylab="this is the y-axis", main="Plot of X vs Y")

## quantitative vs qualitative

summary(Auto)
quantitative = subset(Auto[,-c(7:9)])
qualitative = subset(Auto[,c(7:9)])
head(qualitative)

## range, mean and standard deviation 

sapply(quantitative, range)
sapply(quantitative, mean)
sapply(quantitative, sd)

################################################
## PLOTS

#install.packages('ggplot2')
#install.packages('gridExtra')
library(ggplot2)
library(gridExtra)

#Ordinary Scatterplot
dev.new()
Plots = pairs(Auto)

#Scatterplot2
#install.packages('psych')
library(psych)
dev.new()
pairs.panels(Auto)

#ggplot scatterplot

library(GGally)
dev.new()
ggpairs(quantitative) 

dev.new()
ggpairs(Auto, cardinality_threshold = 304)

##ggplot plot

dev.new()
ggplot(Auto, aes(x = mpg, y = cylinders)) +
  geom_point()

dev.new()
ggplot(Auto, aes(x = mpg, y = cylinders)) +
  geom_point(aes(color = factor(cylinders)))

dev.new()
g1 = ggplot(data = quantitative, aes(x = displacement, y = horsepower))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(80,450))
g2 = ggplot(data = quantitative, aes(x = horsepower, y = weight))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(50,250))
g3 = ggplot(data = quantitative, aes(x = weight, y = displacement))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(1800,5000))
grid.arrange(g1, g2, g3, nrow = 3)

##ggplot lm_fit

dev.new()
ggplot(Auto, aes(x = mpg, y = horsepower)) +
  geom_point(aes(color = factor(horsepower))) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)


dev.off()
plot(mpg,horsepower)
fit = lm(mpg~horsepower)
abline(fit, col = 'red')


#############################################################################

## Boxplot

# as.factor() function converts quantitative variables into qualitative variables.
cylinders
cylinders = as.factor(cylinders )
cylinders
# If the variable plotted on the x-axis is categorial, then boxplots will
# boxplot automatically be produced by the plot() function. 
attach(Auto)
plot(cylinders, mpg)
plot(cylinders , mpg , col="red", varwidth =T,horizontal =T)
plot(cylinders , mpg , col="red", varwidth =T, xlab="cylinders ", ylab="MPG")

##ggplot boxplot

dev.new()
ggplot(Auto, aes(x=as.factor(cylinders), y=mpg, fill = 'red')) + 
  geom_boxplot()

## Histogram

par(mfrow=c(1,2))
hist(mpg ,col=2,breaks =15)

dev.new()
ggplot(Auto, aes(x=mpg)) + 
  geom_histogram(colour = 'red')

## scatterplot matrix

pairs(Auto)
pairs(~mpg + displacement + horsepower + weight + acceleration)

identify (horsepower,mpg,name)

###############################################################

### LINEAR REGRESSION

set.seed(1)
X = rnorm(100)
e = rnorm(100, mean = 0, sd = 0.5)
Y = -1 + 0.5*X + e
model = lm(Y~X)

dev.new()
plot(X,Y)
abline(model, col = 'blue', lwd = 3)
abline(Y,X, col = 'red', lwd = 3)
legend(1, -2, legend=c("Least squares line", "Population regression line"),
       col=c("blue", "red"), lty=1:2)

summary(model)
# Residual is the difference between the observed value for y at some particular x
# and the predicted value for y at that same x value.As maximum residual is 0.7041, this means
# that the model predicts Y quite well. Nearly 50 percent of predictions are between 0.2137
# over the true value and 0.1649 under the true value.
# coefficients  ??^0 and ??^1 almost equal to ??0 and ??1, besides from p value, we can state that the significance level met by the estimate.
# Looking at R-squared:  0.8194, we can say that the model fits data quite well, since the model explains nearly 82% of the variation in the dependent variable


## polynomial regression

# If we look at  the R-squared values, they are nearly the same, however if we look at p-value of X^2
# we can see, that it is not statistically significant. So, it is not reasonable to use lm_pol model.

lm_pol = lm(Y ~  X + I(X^2))
summary(lm_pol)
dev.new()
par(mfrow=c(2,2))
plot(lm_pol)

## confidence

confint(model, level = 0.95)
e0 = rnorm(100, mean = 0, sd = 0.5)
Y0 = -1 + 0.5*X + e0
noisy_model = lm(Y0~X)
summary(noisy_model)
confint(noisy_model, level = 0.95)
e1 = rnorm(100, mean = 0, sd = 0.05)
Y1 = -1 + 0.5*X + e1
less_noisy_model = lm(Y1~X)
summary(less_noisy_model)
confint(less_noisy_model, level = 0.95)

#The probability that Y will be within the intervals above is the same and equals 0.95, whether or not the interval is wider or narrower.


##What is the predicted "mpg" associated with a "horsepower" of 98
#What are the associated 95% confidence and prediction intervals
predict(lm_fit_mpg, data.frame(horsepower=c(98)), interval = "confidence")
predict(lm_fit_mpg, data.frame(horsepower=c(98)), interval = "prediction")

######################################################
## Diagnostic plots
lm_fit_mpg = lm(mpg ~ horsepower, data = Auto)

dev.new()
plot(lm_fit_mpg, which=c(1)) 
# We have a clear u shape curve, so the relation is non-linear.

dev.new()
plot(lm_fit_mpg, which=c(2)) 
# Residuals are lined close to the dashed line, which means that residuals are normally distributed

dev.new()
plot(lm_fit_mpg, which=c(3)) 

### Instructor's comment: The variance of the error terms is not constant.
### The residuals are not spread equally along the ranges of predictors.

dev.new()
plot(lm_fit_mpg, which=c(4)) # Cook's Distance

dev.new()
plot(lm_fit_mpg, which=c(5)) #There are points that are influential to the regression results, since datapoints are higher than 0.5

dev.new()
plot(lm_fit_mpg, which=c(6)) # Cook's distance vs Leverage

dev.new()
plot(predict(lm_fit_mpg), rstudent(lm_fit_mpg))  # Studentized Residuals vs Fitted values
abline(h = -3, col = 'red')
abline(h = 3, col = 'red')               
which(abs(rstudent(lm_fit_mpg))>3)           
#there are almost no outliers

dev.new()
hatvalues(lm_fit_mpg)                        # Leverage statistics. 
plot(hatvalues(lm_fit_mpg))                   
abline(h=2*2/397, col = 'red')           
which(hatvalues(lm_fit_mpg)>2*2/397)
abline(h=3*2/397, col = 'red')           
which(hatvalues(lm_fit_mpg)>3*2/397)
# there are a few points with high leverage 



