# Last modified by Lucy Lu on 2021/3/9

devtools::install_github("ccolonescu/POE5Rdata") 
library(POE5Rdata)

# questiion 2-22
data("star5_small")
?star5_small
star5_small
#(a)
total <-which(star5_small$small==1|star5_small$regular==1)
newstar<-star5_small[total,]
newstar
mod1 <- lm(totalscore~small,data = newstar) 
mod1
summary(mod1)
plot(newstar$small, newstar$totalscore,xlab = 'small',ylab = 'totalscore', col="grey")
lines(fitted(mod1)~newstar$small, col="blue")
#(b)
mod2 <- lm(mathscore~small,data = newstar) 
mod2
summary(mod2)
plot(newstar$small, newstar$mathscore,xlab = 'small',ylab = 'mathscore', col="grey")
lines(fitted(mod2)~newstar$small, col="blue")
mod3 <- lm(readscore~small,data = newstar) 
mod3
summary(mod3)
plot(newstar$small, newstar$readscore,xlab = 'small',ylab = 'readscore', col="grey")
lines(fitted(mod3)~newstar$small, col="blue")
#(c)
total2 <-which(star5_small$aide==1|star5_small$regular==1)
newstar2<-star5_small[total2,]
mod4 <- lm(totalscore~aide,data =newstar2)
mod4
summary(mod4)
plot(newstar2$aide, newstar2$totalscore,xlab = 'aide',ylab = 'totalscore', col="grey")
lines(fitted(mod4)~newstar2$aide, col="blue")
#(d)
mod5 <- lm(mathscore~aide,data =newstar2)
mod5
summary(mod5)
plot(newstar2$aide, newstar2$mathscore,xlab = 'aide',ylab = 'mathscore', col="grey")
lines(fitted(mod5)~newstar2$aide, col="blue")
mod6 <- lm(readscore~aide,data =newstar2)
mod6
summary(mod6)
plot(newstar2$aide, newstar2$readscore,xlab = 'aide',ylab = 'readscore', col="grey")
lines(fitted(mod6)~newstar2$aide, col="blue")



## question 2-25
data("cex5_small")
?cex5_small

 #(a)
hist(cex5_small$foodaway)
summary(cex5_small$foodaway)

#(b)
adv = cex5_small$foodaway[cex5_small$advanced==1]
median(adv)
mean(adv)
col = cex5_small$foodaway[cex5_small$college==1]
median(col)
mean(col)
adco = cex5_small$foodaway[cex5_small$advanced==1|cex5_small$college==1]
median(adco)
mean(adco)

#(c)
hist(log(cex5_small$foodaway), col='grey')
summary(log(cex5_small$foodaway))
#(d)
class(cex5_small)
ordat <- cex5_small[order(cex5_small$income), ]
log_foodaway<-log(cex5_small$foodaway)
log_foodaway
sum(is.infinite(log_foodaway))
log_foodaway[is.infinite(log_foodaway)]<-0
sum(is.infinite(log_foodaway))

mod <- lm(log_foodaway~income,data = ordat) 
mod



#(e)
plot(cex5_small$income, log(cex5_small$foodaway), col="grey")
plot(cex5_small$income, cex5_small$foodaway, col="grey")
lines(exp(fitted(mod))~ordat$income, col="blue", main="Log-linear Model") 

#(f)
attributes(mod)
mod$residuals
plot(mod$residuals,cex5_small$income)
