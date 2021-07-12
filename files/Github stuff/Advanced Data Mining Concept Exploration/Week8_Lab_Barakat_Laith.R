#LOADING
## load Boston data
library(MASS)
data(Boston)
index <- sample(nrow(Boston),nrow(Boston)*0.60)
boston.train <- Boston[index,]
boston.test <- Boston[-index,]

## load credit card data
credit.data <- read.csv("C:/Users/Laith/Documents/SpringSem20/Data Mining II/Lab Notes/8. Other topics/Advanced Tree/data/credit_default.csv", header=T)
## convert categorical variables
credit.data$SEX<- as.factor(credit.data$SEX)
credit.data$EDUCATION<- as.factor(credit.data$EDUCATION)
credit.data$MARRIAGE<- as.factor(credit.data$MARRIAGE)
## random splitting
index <- sample(nrow(credit.data),nrow(credit.data)*0.60)
credit.train = credit.data[index,]
credit.test = credit.data[-index,]

#BAGGING
install.packages("randomForest")
library(randomForest)
boston.bag <- randomForest(medv~., data=boston.train, ntree=100, mtry=ncol(boston.train)-1)
?randomForest
boston.bag
boston.bag.pred <- predict(boston.bag, newdata = boston.test)
mean((boston.test$medv-boston.bag.pred)^2)

##compare with one tree
library(rpart)
boston.tree <- rpart(medv~., data = boston.train)
boston.tree.pred <- predict(boston.tree, newdata = boston.test)
mean((boston.test$medv-boston.tree.pred)^2)

##How many trees do we need
ntree <- c(1, 3, 5, seq(10,200,10))
MSE.test <- (rep(0, length(ntree)))
for(i in 1:length(ntree)){
  boston.bag <- randomForest(medv~., data=boston.train, ntree=ntree[i], mtry=ncol(boston.train)-1)
  boston.bag.pred <- predict(boston.bag, newdata = boston.test)
  MSE.test[i] <- mean((boston.test$medv-boston.bag.pred)^2)
}
MSE.test
plot(ntree, MSE.test, type = 'l')

##outta bag
boston.bag$mse[100]

#RANDOMFOREST
boston.rf <- randomForest(medv~., data = boston.train, mtry = floor(ncol(boston.train)-1)/3, ntree = 500, importance = TRUE)
boston.rf
boston.rf$importance
varImpPlot(boston.rf)
plot(boston.rf$mse, type='l', col=2, lwd=2, xlab = "ntree", ylab = "OOB Error")

boston.rf.pred <- predict(boston.rf, boston.test)
mean((boston.test$medv - boston.rf.pred)^2)

oob.err <- rep(0, 13)
test.err <- rep(0,13)
for(i in 1:13){
  fit <- randomForest(medv~., data = boston.train, mtry = i)
  oob.err[i] <- fit$mse[500]
  test.err[i] <- mean((boston.test$medv-predict(fit, boston.test))^2)
  cat(i, " ")
}
matplot(cbind(test.err, oob.err), pch=15, col = c("red", "blue"), type = "b", ylab = "MSE", xlab = "mtry")
legend("topright", legend = c("test Error", "OOB Error"), pch = 15, col = c("red", "blue"))


#BOOSTING
install.packages("gbm")
library(gbm)
boston.boost<- gbm(medv~., data = boston.train, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 10)
summary(boston.boost)
plot(boston.boost, i="lstat")

##prediction on the testing sample
boston.boost.pred.test <- predict(boston.boost, boston.test, n.trees = 10000)
mean((boston.test$medv-boston.boost.pred.test)^2)

ntree <- seq(100,10000, 100)
pred.mat <- predict(boston.boost, boston.test, n.trees = ntree)
err <- apply((pred.mat-boston.test$medv)^2,2,mean)
plot(ntree, err, type = 'l', col=2, lwd=2, xlab = "n.trees", ylab = "Test MSE")
abline(h=min(test.err), lty=2)



#classification
credit.boost <- gbm(default.payment.next.month~., data = credit.data, distribution = "bernoulli", n.trees = 100, shrinkage = .01, interaction.depth = 8)
##testing performance 
pred.credit.boost <- predict(credit.boost, newdata = credit.test, type = "response", n.trees = 100)
library(ROCR)
pred <- prediction(pred.credit.boost, credit.test$default.payment.next.month)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
