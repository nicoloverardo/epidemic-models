library(stats)
library(stats4)
library(caret)

# Example
# Turin
dataistat <- read.csv("data/istat/pop_prov_age.csv")
data_to <- dataistat[dataistat$Territorio == "Torino",]
pop <- data_to$Value[data_to$Eta == "Total"]
class_pop <- data_to$Value[data_to$Eta != "Total"]

# We're gonna need to turn this into 4 groups and
# calculate this for each group

# 75-100
highrisk <- class_pop[4]

# 25-50 + 50-75
mediumrisk <- class_pop[2] + class_pop[3]

x <- sample(highrisk, 1000)
d <- sample(mediumrisk, 1000)

#from google mobility data, community is divided into PARKS,SHOPS,WORK AND RECREATION
#AND BY MAKING A MARKOV CHAIN,it seems high risk and medium risk people has most interation 
#where high risk people r morevurnerable to get the disease  so selected p=0.40

idx <- createDataPartition(x, p=0.40, list=FALSE)
k <- rnorm(d)

nll <- function(theta0,theta1) {
  x1 <- as.vector(k)
  y <- as.vector(x[-idx])
  mu = exp(theta0 + x1*theta1)
  -sum(y*(log(mu)) - mu)
}

x1 <- as.vector(k)
y <- as.vector(x[-idx])

# MLE
est <- stats4::mle(minuslog=nll, start=list(theta0=2,theta1=0))
summary(est)

y1<- sample(x1,600)
w <- data.frame(y,y1)

glm.fit <-  glm(log(y)~y1,data=w)
pred.lm <- predict(glm.fit, w)
mean(pred.lm)
