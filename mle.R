library(stats)
library(stats4)
library(caret)
highrisk <-  125840
mediumrisk <- 166444 
lowrisk <-  831160
pij <- 1

x <-sample(highrisk,1000)
d <- sample(mediumrisk,1000)
#from google mobility data, community is divided into PARKS,SHOPS,WORK AND RECREATION
#AND BY MAKING A MARKOV CHAIN,it seems high risk and medium risk people has most interation 
#where high risk people r morevurnerable to get the disease  so selected p=0.40
idx <- createDataPartition(x, p=0.40,list=FALSE)
k <- rnorm(d,n=600)
nll <- function(theta0,theta1) {
  x1 <- as.vector(k)
  y <- as.vector(x[-idx])
  mu = exp(theta0 + x1*theta1)
  -sum(y*(log(mu)) - mu)
}



est <- stats4::mle(minuslog=nll, start=list(theta0=2,theta1=0))
summary(est)

w <- data.frame(y,x1)

lm.fit <-  lm(log(x1)~y, data=w)
pred.lm <- predict(lm.fit, w)
mean(pred.lm)