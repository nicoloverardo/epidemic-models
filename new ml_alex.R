library(stats)
library(stats4)
library(caret)

# Example
# Turin
dataistat <- read.csv("C:/Users/ADMIN/Desktop/covid19/New folder (3)/epidemic-models/data/istat/pop_prov_age.csv")
data_to <- dataistat[dataistat$Territorio == "Torino",]
pop <- data_to$Value[data_to$Eta == "Total"]
class_pop <- data_to$Value[data_to$Eta != "Total"]

# We're gonna need to turn this into 4 groups and
# calculate this for each group

# 75-100
highrisk <- class_pop[4]

# 25-50 + 50-75
mediumrisk <- class_pop[2] + class_pop[3]

x <- sample(mediumrisk) ####################################
d <- sample(highrisk)  ##############################3

# Looking at Google Mobility data, the community is divided into 
# PARKS, SHOPS, WORK and RECREATION. By MAKING A MARKOV CHAIN,
# it seems that high risk and medium risk people has most interation.
# High risk people are more vurnerable to get the disease.
# Thus the selection of p=0.40
idx <- createDataPartition(x, p=0.90, list=FALSE)



x1 <- as.vector(x)
y <- as.vector(x[-idx])

# MLE
#est <- stats4::mle(minuslog=nll, start=list(theta0=2,theta1=0))
#summary(est)

max.len = max(length(x1), length(y))
j = c(y, rep(NA, max.len - length(y)))
#j= infectedhigh risk 
w <- data.frame(x1,j)

glm.fit <-  glm((x1)~j,data=w,family = poisson)
pred.lm <- predict(glm.fit, w)
plot(pred.lm)
summary(pred.lm)
