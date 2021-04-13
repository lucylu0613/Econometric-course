# homework3 modified by Lu lucy at 2021/03/17

devtools::install_github("ccolonescu/POE5Rdata") 
library(POE5Rdata)
library(xtable); library(knitr)
# questiion 3-26
data("cps5_small")
?cps5_small

mod <- lm(wage~exper,data = cps5_small) 
smod <- summary(mod)
smod
SStable <- data.frame(xtable(mod))
kable(table, caption="Regression output showing the coefficients")

b2 <- coef(mod)[[2]] # the coefficient on exper
seb <- sqrt(vcov(mod)[2,2]) #standard error of b2
df0 <- df.residual(mod) # degrees of freedom      
c <- 0
alpha <- 0.05
# one tail test
t <- (b2-c)/seb
tcr <- qt(1-alpha, df0) # note: alpha is not divided by 2
t01 <- (b2-c)/seb
tcr01 <- qt(alpha, df0)
curve(dt(x, df0), from = -5, to = 5, col = "orange", 
      xlab = "quantile", ylab = "density", lwd = 2)
abline(v=c(tcr,t), col=c("red", "blue"), lty=c(2, 3))
legend("topleft", legend=c("tcr", "t"), col=c("red", "blue"), lty=c(2, 3))
curve(dt(x, df0), from = -5, to = 5, col = "orange", 
      xlab = "quantile", ylab = "density", lwd = 2)
abline(v=c(tcr01,-t01), col=c("red", "blue"), lty=c(2, 3))
legend("topleft", legend=c("tcr", "t"), col=c("red", "blue"), lty=c(2, 3))


#(c)
totoal <-which(cps5_small$metro==1)
cps2<-cps5_small[total,]
mod1 <- lm(wage~exper,data = cps2)
#mod1
smod1 <- summary(mod1)
#smod1
table <- data.frame(xtable(mod1))
kable(table, caption="Regression output showing the coefficients")

b22 <- coef(mod1)[[2]] # the coefficient on exper
seb1 <- sqrt(vcov(mod1)[2,2]) #standard error of b2
df1 <- df.residual(mod1) # degrees of freedom      
c <- 0
alpha <- 0.01
# one tail test
t1 <- (b22-c)/seb1
tcr1 <- qt(1-alpha, df1) # note: alpha is not divided by 2
t11 <- (b22-c)/seb1
tcr11 <- qt(alpha, df1)

curve(dt(x, df1), from = -5, to = 5, col = "orange", 
      xlab = "quantile", ylab = "density", lwd = 2)
abline(v=c(tcr1,t1), col=c("red", "blue"), lty=c(2, 3))
legend("topleft", legend=c("tcr", "t"), col=c("red", "blue"), lty=c(2, 3))

curve(dt(x, df2), from = -5, to = 5, col = "orange", 
      xlab = "quantile", ylab = "density", lwd = 2)
abline(v=c(tcr11,-t11), col=c("red", "blue"), lty=c(2, 3))
legend("topright", legend=c("tcr", "t"), col=c("red", "blue"), lty=c(2, 3))


#(d)
total2 <-which(cps5_small$metro==0)
cps3<-cps5_small[total2,]
mod2 <- lm(wage~exper,data = cps3) 
smod2 <- summary(mod2)
smod2
table <- data.frame(xtable(mod2))
kable(table, caption="Regression output showing the coefficients")

b23 <- coef(mod2)[[2]] # the coefficient on exper
seb2 <- sqrt(vcov(mod2)[2,2]) #standard error of b2
df2 <- df.residual(mod2) # degrees of freedom      
c <- 0
alpha <- 0.01
# one tail test
t2 <- (b23-c)/seb2
tcr2 <- qt(1-alpha, df2) # note: alpha is not divided by 2

t22 <- (b23-c)/seb2
tcr22 <- qt(alpha, df2)

if ((tcr2<t2)|(t2<-tcr2)){ print("falls again in the rejection region, we can reject the null hypothesis")}else{prnit("not in the rejection region, we can't reject the null hypothesis")}

curve(dt(x, df2), from = -5, to = 5, col = "orange", 
     xlab = "quantile", ylab = "density", lwd = 2)
abline(v=c(tcr2,t2), col=c("red", "blue"), lty=c(2, 3))
legend("topleft", legend=c("tcr", "t"), col=c("red", "blue"), lty=c(2, 3))

curve(dt(x, df2), from = -5, to = 5, col = "orange", 
      xlab = "quantile", ylab = "density", lwd = 2)
abline(v=c(tcr22,-t22), col=c("red", "blue"), lty=c(2, 3))
legend("topleft", legend=c("tcr", "t"), col=c("red", "blue"), lty=c(2, 3))
