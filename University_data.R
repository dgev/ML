rm(list=ls())
ls()

# Problem 1

# 1.1

#install.packages('Ecdat')
library(Ecdat)
attach(University)
y = resgr

dim(na.omit(University)) 
# There are 62 observations and 16 features, since resgr is our response

summary(University)
numerical = subset(University[,-c(15:17)])
categorical = subset(University[,c(15:16)])

    
# 1.2

dev.new()
pairs(University)

# poststudents, acnumbers, acrelnum, crelnum, techn, stfees, acpay, secrpay, admpay, agresrk seem to be correlated with resgr


# 1.3

library(ggplot2)
library(gridExtra)
library(GGally)

ggplot(data = University, aes(x = poststudents, y = resgr))+
  geom_point()+ geom_line(col = 'blue') +
  ggtitle("Poststudents  vs Resgr") + xlab('poststudents') + ylab('resgr')


ggplot(data = University, aes(x = poststudents, y = resgr))+
  geom_point()+
  ggtitle("Poststudents  vs Resgr") + xlab('poststudents') + ylab('resgr')+
  geom_smooth(method = 'lm', se = F, col='red')

# From graph we can see that there could exist linear relationship between the response and the variable


# 1.4

ggplot(data = University, aes(x = acpay, y = resgr))+
  geom_point()+ geom_line(col = 'blue') +
  ggtitle("Acpay  vs Resgr") + xlab('acpay') + ylab('resgr')


ggplot(data = University, aes(x = acpay, y = resgr))+
  geom_point()+
  ggtitle("Acpay  vs Resgr") + xlab('acpay') + ylab('resgr')+
  geom_smooth(method = 'lm', se = F, col='red')

# It seems that there is a linear relationship between resgr and Acpay



####################################
## Problem 2

# 2.1

lm_fit = lm(resgr ~ ., data = University)
summary(lm_fit)

# Since techn, acpay, acrelpay, admpay, agresrk, landbuild have small p-values, therefore they are statistically significant.  

# 2.2

lm_fit_imp = lm(resgr ~ techn + acpay + acrelpay + admpay + agresrk + landbuild, data = University)
summary(lm_fit_imp)

# An Adjusted R-squared values in both cases are close to 1, which indicates that the models explain a large portion of the variance in the response variable. 
# In the previous model Adjusted R-squared was 0.9606 and now it is 0.9554, so in second model Adjusted R-squared slightly decreases, but they are almost the same.


# 2.3

dev.new()
plot(lm_fit_imp, which=c(5)) 

# From the plot we can see that all points are within Cook's distance. 
# Thus, the regression may not be alerted by these points.

which(abs(rstudent(lm_fit_imp))>3) # There are no ouliers

hatvalues(lm_fit_imp)                 
which(hatvalues(lm_fit_imp )>3*7/62) # there are five influential point 3  4  7  9 25 



###########################################
## Problem 3

# 3.1

library(glmnet)

x = model.matrix(resgr ~ . , data = University)
y = resgr

set.seed(1)
set.seed(1)

train=sample(1:nrow(x), nrow(x)/2)
test=(-train) 

trainX = x[train,]
trainY = y[train]
testY=y[test]
testX = x[test,]

grid = 2^seq(10,-2, length = 300)
mod_lasso_cv = cv.glmnet(trainX, trainY, alpha = 1, lambda = grid)
bestlam = mod_lasso_cv$lambda.min


# 3.2

bestlam
# bestlam is 713.2484

pred_lasso = predict(mod_lasso_cv, s = bestlam, newx = testX)
sqrt(mean((pred_lasso - testY)^2)) # RMSE is 3355.801

l_model= glmnet(trainX, trainY, alpha = 1, lambda = 0)
pred_l_model = predict(l_model, s = bestlam, newx = testX)
sqrt(mean((pred_l_model - testY)^2)) # RMSE is 4707.946


# Since, RMSE of Lasso regression is smaller than that of linear, therefore first approach regression is better.

# 3.3

out=glmnet(x,y,alpha=1,lambda=grid)
lasso_coef=predict(out,type="coefficients",s=bestlam)[1:18,]
lasso_coef
lasso_coef[lasso_coef!=0]

model = lm(trainY~trainX)
summary(model)

# techn and agresrk have non-zero coefficients therefore, they are important according to Lasso
# according to linear regression only acrelpay is important, so Lasso's and linear regression's important variables do not match
