#install.packages('ISLR')
library(ISLR)
library(ggplot2)
library(gridExtra)

# a

summary(Auto)
quantitative = subset(Auto[,-c(7:9)])
qualitative = subset(Auto[,c(7:9)])

# b 

sapply(quantitative, range)

# c

sapply(quantitative, mean)
sapply(quantitative, sd)

#d
# 
# Investigate the predictors graphically, using scatterplots or other tools of your choice from ggplot library. Create some plots highlighting the relationships among the predictors. Comment on your findings. 
# Plot the same graphs using the basic plot functionality, for comparison. 

dev.new()
Plots = pairs(Auto)

# From the plot, we can notice that displacement, horsepower and weight are positively correlated with each other, which means if one of the mentioned variables
# inreases the other two also inrease. Besides, all these three variables are anticorrelated to mpg(as they increase mpg decreases)
dev.new()
pairs(~ mpg+displacement+horsepower+weight, Auto)
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

# The plots show that when the number of cylinders increases the amounts of displacement, horsepower and weight also increase. So, we can predict 
# the increase in one variable is dependent on the increase of another. 
dev.new()
pairs(~ cylinders+displacement+horsepower+weight, Auto)
dev.new()
plot(as.factor(Auto$cylinders), Auto$displacement, xlab = 'Cylinders', ylab = 'Displacement')
dev.new()
plot(as.factor(Auto$cylinders), Auto$horsepower, xlab = 'Cylinders', ylab = 'horsepower')
dev.new()
plot(as.factor(Auto$cylinders), Auto$weight, xlab = 'Cylinders', ylab = 'weight')
# By plotting cylinders with acceleration, it turns out that they are anticorrelated. Maximum acceleration
# rate is when the number of cylinders is 4, as the number of cylinders increase, acceleration reduces. 
dev.new()
ggplot(data = quantitative, aes(x = cylinders, y = acceleration))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(0,10))
# From the boxplot we can see that the number of cylinders depends on the origin, and the maximum number of cylinders is produced in America.
dev.new()
plot(Auto$origin, Auto$cylinders, xlab = 'Origin', ylab = 'Cylinders')

#From graphs it is obvious that acceleration is anticorrelated with displacement, horsepower and weight.
dev.new()
g1 = ggplot(data = quant, aes(x = displacement, y = acceleration))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(80,450))
g2 = ggplot(data = quant, aes(x = horsepower, y = acceleration))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(50,250))
g3 = ggplot(data = quant, aes(x = weight, y = acceleration))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(1800,5000))
grid.arrange(g1, g2, g3, nrow = 3)

#e

# In Plots we can see that as the number of cylinders increase the number of mpg decrease, 
# so we can deduce that high number of cylinders does not mean fuel efficiency.
# It is also obvious fom the graph that maximum mpg occurs when the number of cylinders is 4
dev.new()
ggplot(data = quantitative, aes(x = cylinders, y = mpg))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(0,10))

# From the Plots we can see that gas mileage is anticorrolated to displacement, horsepower and weight, 
# which means that if the number of mpg increases the number of displacement, horsepower and weight will decrease
dev.new()
g1 = ggplot(data = quantitative, aes(x = displacement, y = mpg))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(80,450))
g2 = ggplot(data = quantitative, aes(x = horsepower, y = mpg))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(50,250))
g3 = ggplot(data = quantitative, aes(x = weight, y = mpg))+
  geom_point(pch = 9, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(1800,5000))
grid.arrange(g1, g2, g3, nrow = 3)

# The relationship between mpg and the origin is not so clear, but if we draw the boxplot, we can notice that the origin corresponding
# to number 3 has the highest rates of mpg. 3 means Japanese cars, so those have larger mpg compared to 1,2(1. American, 2. European)
dev.new()
plot(Auto$origin, Auto$mpg, xlab = 'Car Origin', ylab = 'Gas Mileage')

# From the Plots and Boxplot it is obvious that cars produced in 1980 are have the highest mpg, but in  general, we can see that the rate of mpg increases year by year
# except some years only. Hence, latest versions of cars tend to use less fuel.
dev.new()
plot(as.factor(Auto$year), Auto$mpg, xlab = 'Car Origin', ylab = 'Gas Mileage')

# In the plot points are distributed around y=x line, so there is a positive correletion between acceleration and mpg. 
dev.new()
ggplot(data = quantitative, aes(x = acceleration, y = mpg))+
  geom_point(pch = 10, col = 'blue') + geom_smooth(method ="lm", se = F, col = 'red')+
  coord_cartesian(xlim = c(5,25))

