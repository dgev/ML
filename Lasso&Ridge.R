# PROBLEM 1

set.seed(1)
X = rnorm(100, sd = 0.5)
e = rnorm(100, sd = 0.25)
Y = 9 + X + 2*X^2 + 3*X^3 + e

# PROBLEM 2

library(glmnet)

set.seed(1) 
x = model.matrix(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) +
                   I(X^7) + I(X^8) + I(X^9) + I(X^10))[, -1]


train = sample (1: nrow(x), nrow(x)/2) 
test=(-train) 
Y.test=Y[test]
grid=10^seq(10,-2,length=100)


ridge_mod = glmnet(x[train,],Y[train],alpha = 0,lambda = grid) 

set.seed(1)
mod_ridge_cv = cv.glmnet(x[train,],Y[train], alpha = 0, lambda = grid)
plot(mod_ridge_cv)
bestlam = mod_ridge_cv$lambda.min
bestlam

# lambda is 0.01

pred_ridge = predict(mod_ridge_cv, s = bestlam, newx = x[test,])
mean((pred_ridge -Y.test)^2)

# MSE is 0.06312662

r_out=glmnet(x,Y,alpha=0) 
ridge_coef = predict (r_out ,type="coefficients",s=bestlam)[1:11,]
ridge_coef

# We can notice that all coefficients are not zero


# PROBLEM 3

lasso_mod = glmnet(x[train,],Y[train],alpha = 1,lambda = grid)

set.seed(1)
mod_lasso_cv = cv.glmnet(x[train,],Y[train], alpha = 1, lambda = grid)
plot(mod_lasso_cv)
bestlam = mod_lasso_cv$lambda.min
bestlam

# lambda is 0.03053856

pred_lasso = predict(mod_lasso_cv, s = bestlam, newx = x[test,])
mean((pred_lasso -Y.test)^2)

# MSE is 0.05663855

l_out=glmnet(x,Y,alpha=1) 
lasso_coef=predict(l_out,type="coefficients",s=bestlam)[1:11,]
lasso_coef

lasso_coef[lasso_coef!=0]

# Here we can see that 4 of the coefficient estimates are equal to zero, so the lasso model with the bestlam
# computed by cross-validation leaves two variables V1, V2 .....

# PROBLEM 4 

# We can see that Ridge regression does not perform variable selection so it includes all variables, so its main advantage is coefficient shrinkage and the reduction
# of model's complexity. However, Lasso regression though shrinking coefficients, it also excluded 8 variables.
# We can also notice that MSE of Lasso is smaller than that of Ridge.
# Lasso regression performs better when there is a small number of predictors, having vital coefficients, whereas Ridge regression performs better, when response
# is a function of many variables, with coefficients of nearly the same size.

