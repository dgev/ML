#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("MASS")


library(gridExtra)
library('MASS')
library("ggplot2")
# a

row = nrow(Boston)
column = ncol(Boston)
# rows are observations and columns are variables
summary(Boston)

# b
# From the plot we can see that crime is correlated with age, black, lstat and anticorrelated with dis and medv
# zn is anticorrelated with indus, nox, age, lstat and correlated with rm, dis and mdv
# indus is correlated with nox, ptratio and anticorrelated with rm, dis, medv
# nox is correlated with age, rad, tax, ptratio, lstat and anticorrelated with dis, medv
# rm is correlated with dis, medv and anticorrelated with ptration, lstat
# age is anticorrelated with dis, black, medv and correlated with lstat
# dis is correlated with black, medv and anticorrelated with lstat
# ptratio is correlated with lstat and anticorrelated with medv
# black is correlated with medv
# lstat is anticorrelated with medv
dev.new()
pairs(Boston)

#c
# From correlation coefficients we can see that crime has somewhat significant correlation with indus, nox, age, lstat
# and anticorrelated with dis, black, medv
cor(Boston$crim, Boston[,c(2:14)])


# d
# In 400 suburbs the crime rate is approximately 3-12, in around 80 suburbs have crime rate of 12-20,
# 20 suburbs have 20-25 crime rate. As we can notice high crime rate is in several suburbs.
# The highest tax rate is in around 20 suburbs, and high rate(approximately 680) in 180 suburbs
# High pupil-teacher ratio (15-25) is in 450 suburbs.
dev.new()
g1 = qplot(Boston$crim, binwidth = 10 , xlab = "Crime rate", ylab="Number of Suburbs")
g2 = qplot(Boston$tax, binwidth = 10 , xlab = "Tax rate", ylab="Number of Suburbs")
g3 = qplot(Boston$ptratio, binwidth = 10, xlab ="Pupil-teacher ratios", ylab="Number of Suburbs")
grid.arrange(g1, g2, g3, nrow = 3)

# e

nrow(subset(Boston, chas == 1))

