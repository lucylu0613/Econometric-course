# Chapter 1
# Last modified by Huei-Wen Teng on 2021/3/2

rm(list=ls()) # Caution: this clears the Environment

# install some useful packages
#install.packages("devtools")
library(devtools)
install_github("ccolonescu/PoEdata")  # Fixed by TA
library(PoEdata)

# other packages used more often in this course
# Teng: I don't like to use these packages
#library(bookdown)
#library(Knitr)
#library(xtable)
#library(printr)
#library(stargazer)
#library(rmarkdown)

data("andy") 
?andy # provides info for the dataset

andy

# let's look around the data

head(andy)
tail(andy)
nrow(andy)


# andy is a matrix
v1 = andy[,1];
v2 = andy[,2];
v3 = andy[,3];

plot(v1);
plot(v2);
plot(v3);

boxplot(andy)
hist(v1)		# histogram
pairs(andy)		# pairwise scatterplots

# some(andy) # no such function

# creating graphs

# - add another curve to the existing graph:
curve(x^1, from = -2, to = 2, xlab ="x", ylab= "y=x" );
curve(x^2, add = TRUE)

# - 
plot(1: 100, type = 'n') # creates asn empty graph
curve(sqrt(x), from = 0, to = 100, xlab = "x", ylab = "y");

# - 
plot(1:10, type= 'n') # creates asn empty graph
abline(a=9, b = -0.5, h = 3.5, v = 4);

# - 
curve(x^2, from = 0, to = 20)
abline(v = 10)



# bernoulli and binomial distributions
?rbinom				# check rbinom in R
nSample = 1000;
size = 1;
prob = 1/3; 
x = rbinom(nSample, size, prob);
x

table(x)		# check the frequency of outputs
hist(x)

# poisson distribution
#?rpois
#nSample = 1000;
#lambda = 10;
#x = rpois(nSample, lambda)
#x
#table(x)		# check the frequency of outputs
#hist(x)


# uniform distribution
#?runif

#x = runif(nSample, min=0, max =1)
#hist(x)
#plot(density(x))

# exponential and gamma distribution
#?rexp
#?rgamma
#x = rgamma(nSample, 10, 2)
#x
#hist(x)

# normal distribution
?rnorm
x = rnorm(nSample, mean = 10, sd = 10);
plot(x) 	#scatter plot
hist(x)	# scatter plot
density(x)
plot(density(x))  # density plot
qqnorm(x)


xx = rgamma(nSample, 10, 2)
hist(xx)
qqnorm(xx)


xx = runif(nSample, 2, 20)
hist(xx)
qqnorm(xx)



n = 1:1000;
an = 1 + (3*n+2)/(6*n);
plot(n, an)



# An illustration of the central limit theorem
par(mfrow=c(3,2));	# allocate multiple figures
lambda = 1;
mu = lambda;
sig2 = lambda;
n = 5;          ## sample size. Need to show that sumx_n -> Z as n goes to infinity

nRep = 10000;               # number of replicates 
Xbar = rep(0, nRep);
Xbar_n = rep(0, nRep);

for(idx in 1 : nRep)
{
  x = rexp(n, lambda);
  Xbar[idx] = mean(x);
  Xbar_n[idx] = ( Xbar[idx] - mu ) / sqrt( sig2/n);
}

hist(Xbar);hist(Xbar_n);
plot(density(Xbar));
plot(density(Xbar_n));
xx = seq(-3, 3, by = 0.01);
lines(xx, dnorm(xx, 0,1), col = "red")

plot(ecdf(Xbar), do.points= FALSE)
plot(ecdf(Xbar_n), do.points= FALSE)
lines(xx, pnorm(xx), col="red");




## example in the hw

# i)
x = 20
pbinom(x, 100, 0.25)

# ii)
p = 0.25;
mu = p;
sig = p*(1-p);
n = 100;
apple = (x - mu*n )/(sqrt(n)*sig);
pnorm(apple, 0, 1)

