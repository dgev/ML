# PROBLEM 1

#a

X = rnorm(100)

#b

e = rnorm(100, mean = 0, sd = 0.25)

#c

Y = -1 + 0.5*X + e

#d

# From the plot we can see that there is a linear relationship between X and Y, since points are 
# distributed around the line x=y. As a result correlation between X and Y should be closer to 1.

dev.new()
plot(X,Y)


#e

# Residual is the difference between the observed value for y at some particular x
# and the predicted value for y at that same x value.As maximum residual is 0.7041, this means
# that the model predicts Y quite well. Nearly 50 percent of predictions are between 0.2137
# over the true value and 0.1649 under the true value.
# coefficients  ??^0 and ??^1 almost equal to ??0 and ??1, besides from p value, we can state that the significance level met by the estimate.
# Looking at R-squared:  0.8194, we can say that the model fits data quite well, since the model explains nearly 82% of the variation in the dependent variable

model = lm(Y~X)
summary(model)

#f

dev.new()
plot(X,Y)
abline(model, col = 'blue', lwd = 6)
abline(a = -1, b = 0.5, col = 'red', lwd = 3)
legend(1, -2, legend=c("Least squares line", "Population regression line"),
       col=c("blue", "red"), lty=1:2)

#g 

# If we look at  the R-squared and Adjusted R-squared values, they are nearly the same, however if we look at p-value of X^2
# we can see, that it is not statistically significant. So, it is not reasonable to use lm_pol model.

lm_pol = lm(Y ~  X + I(X^2))
summary(lm_pol)
dev.new()
par(mfrow=c(2,2))
plot(lm_pol)

#h

# We can see that the noisy_model has the highest confidence interval.
# If noise is less the confidence interval is narrower, so as variance increases confidence interval becomes wider.

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


# PROBLEM 2

#a

library(ISLR)

lm_reg = lm(mpg ~ horsepower, data = Auto)
summary(lm_reg)

# I. From small p value we can state that there is a relationship between the predictor
# and the response.
# II. From the R-squared value we can state, that the predictor variable explains nearly 
# 61 percent of the variation in the response variable. 

# III and IV
predict(lm_reg, data.frame(horsepower=c(98)), interval = "confidence")
predict(lm_reg, data.frame(horsepower=c(98)), interval = "prediction")

#b

dev.new()
plot(x = Auto$horsepower, y = Auto$mpg)
abline(lm_reg,  col = 'blue', lwd=3)

#c and d

dev.new()
par(mfrow=c(2,2))
plot(lm_reg)

dev.new()
plot(lm_reg, which=c(1)) 

# Residuals vs Fitted values.  The residuals exhibit a U-shape, which provides a strong indication of non-linearity in the data.

dev.new()
plot(lm_reg, which=c(2)) 

# The points lie close to the dashed line, but not exactly on it, especially at the ends.
# So the errors (residuals) are not normally distributed.

dev.new()
plot(lm_reg, which=c(3)) 

# sqrt(|Standardized Residuals|) vs Fitted values.
# Here we can notice a flat (somewhat horizontal) line with equally spread points, which
# proves the assumption of constant variance of error terms.
# So residuals are spread equally along the ranges of predictors.

dev.new()
plot(lm_reg, which=c(5)) 

# Residuals vs Leverage. 
# In this plot, you can see that the red smoothed line is relatively close to the horizontal dashed 
# line and that all points are within Cook's distance. So, the regression may not be alerted by these points.

dim(na.omit(Auto))
index = complete.cases(Auto)
n = sum(index)
cutoff = (4/(n-length(lm_reg$coefficients)-2))
dev.new()
plot(lm_reg, which=c(4), cook.levels = cutoff) # Cook's Distance
abline(h=cutoff, col = 'red')

# The graph shows that the model has almost 20 influential points, which means that if we delete those observations
# the values of intercept and slopes in the regression model might be changed.

dev.new()
plot(lm_reg, which=c(6)) # Cook's distance vs Leverage

dev.new()
plot(predict(lm_reg), rstudent(lm_reg))  # Studentized Residuals vs Fitted values
abline(h = -3, col = 'red', lty = 2)
abline(h = 3, col = 'blue', lty = 2)               
which(abs(rstudent(lm_reg))>3)           # For identifying outliers.

# We have two outliers.

dev.new()
hatvalues(lm_reg)                        # Leverage statistics. 
plot(hatvalues(lm_reg))                   
abline(h = 2*2/n, col = 'red', lty = 2)           # Often use 2(p+1)/n or 3(p+1)/n threshold
which(hatvalues(lm_reg)>2*2/n)
abline(h=3*2/n, col = 'blue', lty = 2)           # to determine high leverage points.
which(hatvalues(lm_reg)>3*2/n)

# For 3(p+1)/n, we have only 14 observations with high leverage. For 2(p+1)/n, we have approximately 27 observations that are above the line, nevertheless the previous 14 observations
# seem to have unusual leverage rates among others.

#PROBLEM 3

#a

library(ISLR)

attach(Carseats)
lm_fit = lm(Sales ~ Price+Urban+US)

#b

summary(lm_fit)

# We can reject null hypothesis for UrbanYes since p-value is too big.

l_fit = lm(Sales ~ Price+US)

#d

anova(lm_fit, l_fit)
# From F-statistc which is 0.0065 and p-value 0.9357, 
# we can state that these two models are similar. 

#e

confint(l_fit, level = 0.95)

# f

dev.new()
par(mfrow=c(2,2))
plot(l_fit)

# From plots we can see that the errors (residuals) are normally distributed.
# The residuals exhibit somewhat horixontal line, which provides an indication of linearity in the data.
# Here we can notice a flat (somewhat horizontal) line with equally spread points, which
# proves the assumption of constant variance of error terms.

dev.new()
plot(predict(l_fit), rstudent(l_fit))  # Studentized Residuals vs Fitted values
abline(h = -3, col = 'red')
abline(h = 3, col = 'red')               
which(abs(rstudent(l_fit))>3) 

# We have no outliers

dev.new()
hatvalues(l_fit)                        # Leverage statistics. 
plot(hatvalues(l_fit))                   
abline(h=2*3/397, col = 'red')           
which(hatvalues(l_fit)>2*3/400)

abline(h=3*3/397, col = 'red')           
which(hatvalues(l_fit)>3*3/400)

# We have 6 points with high Levarage


# PROBLEM 4

#a

library(ISLR)

dev.new()
pairs(Auto)

library(psych)
dev.new()
pairs.panels(Auto)

#b

cor(subset(Auto[,c(1:8)]))

#c 

lm_fit = lm(mpg ~ .- name, data = Auto)
summary(lm_fit)

# I. Since, p values are almost zero and F statistics is significantly greater than 1, 
# therefore there is a relationship between predictor and response.
# II. Looking at p values, we can state that displacement, weight, origin and year have a statistically significant relationship to the response.
# III. Coefficient shows that each year, increment of 0.75 mpg is possible.

#d, e

dev.new()
par(mfrow=c(2,2))
plot(lm_fit)

dev.new()
plot(lm_fit, which=c(1)) 

# Residuals vs Fitted values.  The residuals exhibit a U-shape, which provides a strong indication of non-linearity in the data.

dev.new()
plot(lm_fit, which=c(2)) 

# The points lie close to the dashed line, only at the ends points are not exactly on this line. 
# So the errors (residuals) are somewhat normally distributed, but not so precisely.
# There may be some deviations near the ends.

dev.new()
plot(lm_fit, which=c(3)) 

# sqrt(|Standardized Residuals|) vs Fitted values.
# Here we can notice a flat (somewhat horizontal) line with equally spread points, which
# proves the assumption of constant variance of error terms.
# So residuals are spread equally along the ranges of predictors.

dev.new()
plot(lm_fit, which=c(5)) 

# Residuals vs Leverage. 
# In this plot, you can see that the red smoothed line is relatively close to the horizontal dashed 
# line and that all points are within Cook's distance. So, the regression may not be alerted by these points.

dim(na.omit(Auto))
index = complete.cases(Auto)
n = sum(index)
cutoff = (4/(n-length(lm_fit$coefficients)-2))
dev.new()
plot(lm_fit, which=c(4), cook.levels = cutoff) # Cook's Distance
abline(h=cutoff, col = 'red')

# The graph shows that the model has almost 23 influential points, which means that if we delete those observations
# the values of intercept and slopes in the regression model might be changed.

dev.new()
plot(lm_fit, which=c(6)) # Cook's distance vs Leverage

dev.new()
plot(predict(lm_fit), rstudent(lm_fit))  # Studentized Residuals vs Fitted values
abline(h = -3, col = 'red', lty = 2)
abline(h = 3, col = 'blue', lty = 2)               
which(abs(rstudent(lm_fit))>3)           # For identifying outliers.

# We have four outliers.

dev.new()
p = length(coefficients(lm_fit))
hatvalues(lm_fit)                        # Leverage statistics. 
plot(hatvalues(lm_fit))                   
abline(h = 2*(p+1)/n, col = 'red', lty = 2)           # Often use 2(p+1)/n or 3(p+1)/n threshold
which(hatvalues(lm_fit)>2*(p+1)/n)
abline(h=3*(p+1)/n, col = 'blue', lty = 2)           # to determine high leverage points.
which(hatvalues(lm_fit)>3*(p+1)/n)

# For 3(p+1)/n, we have only two observations with high leverage, however, the level of leverage for one
# of these points is unusually high. For 2(p+1)/n, we have approximately 20 observations that are above the line, nevertheless the previous two observations
# seem to have unusual leverage rates among others.


#f

# Only in lim_fit2 and lim_fit3 variables are statistically significant. But only in lim_fit3 R-squared is 0.85, which means that variables in model 3
# predict mpg quite well compared to other models. From p values, we can see that interactions between displacement, weight and horsepower,
# weight and horsepower also cylinders and displacement are statistically significant.

lm_fit1 = lm(mpg ~ displacement*weight:horsepower, data = Auto)
summary(lm_fit1)
lm_fit2 = lm(mpg ~ displacement*weight*horsepower, data = Auto)
summary(lm_fit2)
lm_fit3 = lm(mpg ~ .-name +displacement:cylinders, data = Auto)
summary(lm_fit3)
lm_fit4 = lm(mpg ~ cylinders*weight*horsepower:acceleration, data = Auto)
summary(lm_fit4)
lm_fit5 = lm(mpg ~ acceleration+weight+horsepower:displacement, data = Auto)
summary(lm_fit5)

#g 

# In the first four models R-squared is nearly 0.86 and adjusted R-squared 0.85, which show that the models predict mpg well. If we look at p-values
# we can say that all variables are statistically significant except cylinders and displacement. It is quite optimal to use these transformations for
# predicting mpg, since our initial model has three variables with great p values.

# In the next two models R-squared is almost 0.83 and adjusted R-squared is 0.82, that are close to the
# corresponding values of our initial model. However, lm_fit_sqr is more preferable compared to lm_fit_sqr1 and lm_fit
# because here all variables are statistically significant except two.

# The last model has the highest R-squared and Adjusted R-squared, nearly 0.87 and all variables are statistically significant,
# so probably this model is the best one among others for predicting mpg.

na.omit(Auto)
lm_fit_log = lm(mpg ~ .-name+log(acceleration), data = Auto)
lm_fit_sqrt = lm(mpg ~ .-name+sqrt(acceleration), data = Auto)
lm_fit_sqr= lm(mpg ~ .-name+I(acceleration^2), data = Auto)
lm_fit_log1 = lm(mpg ~ .-name+log(horsepower), data = Auto)
lm_fit_sqrt1 = lm(mpg ~ .-name+sqrt(horsepower), data = Auto)
lm_fit_sqr1= lm(mpg ~ .-name+I(origin^2), data = Auto)
lm_fit_all = lm(mpg ~ .-name-cylinders-displacement-horsepower+log(horsepower)+sqrt(weight)+I(acceleration^2), data = Auto)

summary(lm_fit_log)
summary(lm_fit_log1)
summary(lm_fit_sqrt)
summary(lm_fit_sqrt1)
summary(lm_fit_sqr)
summary(lm_fit_sqr1)
summary(lm_fit_all)
