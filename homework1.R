## Chapter 1 homework
## Modified by Lucy Lu on 2021/3/3

rm(list=ls()) # Caution: this clears the Environment

## install some useful packages
#install.packages("devtools") #don't have to run this line if it has intalled  
library(devtools)
install_github("ccolonescu/PoEdata")  # Fixed by TA
library(PoEdata)

data("food") 
?food # provides info for the dataset

food
## Queation 2
#(a) Show the first and the bottom six rows of the data
head(food)
tail(food)

#(b) Obtain a scatter plot for each variable.
food_exp = food[,1]
income = food[,2]
plot( food_exp)
plot(income)

#(c) Obtain a boxplot for each variable.
par(mfrow=c(2,1))
boxplot(food_exp, xlab='food_exp')
boxplot(income, xlab='income') #?boxplot

#(d) Obtain a histogram for each variable
par(mfrow=c(2,1))
hist(food_exp,xlab = 'food_exp')
hist(income,xlab = 'income')

#(e) Obtain a scatter plot for food exp against income.
plot(food_exp,income,main='Scatter plot for food exp against income',xlab= 'food_exp',ylab='income')

## Question 3
#  Suppose Y ??? Binomial(100, 0.25). Calculate P(Y ??? 40) using pbinom function in R or other programming language.

# i)
x = 40
pbinom(x, 100, 0.25)

# ii)
p = 0.25;
mu = p;
sig = p*(1-p);
n = 100;
apple = (x - mu*n )/(sqrt(n*sig));
pnorm(apple, 0, 1)