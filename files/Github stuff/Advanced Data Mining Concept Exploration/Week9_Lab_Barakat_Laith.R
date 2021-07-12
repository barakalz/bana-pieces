library(MASS)
library(tidyverse)
data('mcycle')
glimpse(mcycle)

# Rename the variables for ease of usage
Y <- mcycle$accel
X <- mcycle$times

#Scatterplot
plot(Y~X, xlab="time",ylab="accel")

#Linear Regression
lm_mod <- lm(Y~X, data = mcycle)
summary(lm_mod)
abline(lm_mod, col="blue", lwd = 1)

#quadratic model
quad_mod <- lm(Y~X+I(X^2), data=mcycle) 
summary(quad_mod)
plot(X ,Y ,xlab="time", main = "quad model",ylab="accel",cex=.5)
lines(X,quad_mod$fitted.values, col="blue", lwd = 1)
#testing which model is better
anova(lm_mod,quad_mod)

#fifth deg polynomial
poly_mod <- lm(Y~poly(X,5,raw=T),data=mcycle) 
summary(poly_mod)
poly_mod_summary <- summary(poly_mod)
(poly_mod_summary$sigma)^2 
poly_mod_summary$r.squared
poly_mod_summary$adj.r.squared
AIC(poly_mod)
BIC(poly_mod)
plot(X,Y,xlab="time", main = "5-degree polynomial",ylab="accel",cex=.5)
lines(X,poly_mod$fitted.values, col="blue", lwd = 1)

#Splines
library (splines)
reg_sp <- lm(Y~bs(X),data=mcycle)
summary(reg_sp)
plot(X ,Y ,xlab="time", main =  "Spline",ylab="accel",cex=.5)
lines(X,reg_sp$fitted.values, col="blue", lwd = 1)
conf_interval <- predict(reg_sp, interval="confidence", level = 0.95)
lines(X, conf_interval[,2], col="red", lty=2)
lines(X, conf_interval[,3], col="red", lty=2)
#specifying degree of freedom
reg_sp2 <- lm(Y~bs(X,df=10),data=mcycle) 
plot(X ,Y ,xlab="time", main = "Spline with df=10",ylab="accel",cex=.5)
lines(X,reg_sp2$fitted.values, col="blue", lwd = 1)
conf_interval <- predict(reg_sp2, interval="confidence", level = 0.95)
lines(X, conf_interval[,2], col="red", lty=2)
lines(X, conf_interval[,3], col="red", lty=2)

#First: Natural Spline- pre-specified degree of freedom=4
fit2 <- lm(Y~ns(X,df=4),data=mcycle) 
plot(X, Y, main= "Cubic Spline with df=4", xlab="time", ylab="accel") 
lines(X, fit2$fitted.values)
conf_interval <- predict(fit2, interval="confidence", level = 0.95)
lines(X, conf_interval[,2], col="red", lty=2)
lines(X, conf_interval[,3], col="red", lty=2)
#10 df
fit2 <- lm(Y~ns(X,df=10),data=mcycle) 
plot(X, Y, main= "Cubic Spline with df=10", xlab="time", ylab="accel") 
lines(X, fit2$fitted.values)
conf_interval <- predict(fit2, interval="confidence", level = 0.95)
lines(X, conf_interval[,2], col="red", lty=2)
lines(X, conf_interval[,3], col="red", lty=2)
#20 df
fit2 <- lm(Y~ns(X,df=20),data=mcycle) 
plot(X, Y, main= "Cubic Spline with df=20", xlab="time", ylab="accel") 
lines(X, fit2$fitted.values)
conf_interval <- predict(fit2, interval="confidence", level = 0.95)
lines(X, conf_interval[,2], col="red", lty=2)
lines(X, conf_interval[,3], col="red", lty=2)

#Gen additive models
library(mgcv)
s_gam <- gam(Y ~ s(X),data=mcycle)
summary(s_gam)
plot(s_gam, residuals = TRUE, pch = 1)

install.packages("ISLR")
library(ISLR)
attach(Wage)
dim(Wage)
s_Wage <- Wage[c("age","year","education","wage")]
str(s_Wage)
ggplot(data = Wage, mapping = aes(x = age, y = wage)) + 
  geom_point()
#multiple linear regression model
mul_model <- lm(wage~age+year+education, data=Wage)
summary(mul_model)
gam_mod <- gam(wage~s(age, k=6)+s(year, k=6)+education ,data = Wage)
summary(gam_mod)
plot(gam_mod, se=TRUE ,col="blue")
gam.check(gam_mod)

#GAM Boston
set.seed(1234)
sample_index <- sample(nrow(Boston),nrow(Boston)*0.70)
Boston_train <- Boston[sample_index,]
Boston_test <- Boston[-sample_index,]

Boston.gam <- gam(medv ~ s(crim)+s(zn)+s(indus)+chas+s(nox)
                  +s(rm)+s(age)+s(dis)+rad+s(tax)+s(ptratio)
                  +s(black)+s(lstat),data=Boston_train)
summary(Boston.gam)
plot(Boston.gam, pages=1)
AIC(Boston.gam)
BIC(Boston.gam)
Boston.gam$deviance
#using the predict() function
pred <- predict(Boston.gam,Boston_train)
mean((pred - Boston_train$medv)^2)
pred.out <- predict(Boston.gam,Boston_test)
mean((pred.out - Boston_test$medv)^2)