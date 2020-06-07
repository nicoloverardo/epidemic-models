library(stats)
library(stats4)
library(caret)

# THIS PERCENTAGES ARE AN EXAMPLE FOR TORINO
# WILL NEED TO CHANGE ACCORDING TO THE PROVINCE
get_risk_p <- function(groups) 
{
  if (isTRUE(all.equal(groups[1], groups[2], "highrisk")))
  {
    return(0.8)
  }
  else if (isTRUE(all.equal(groups[1], groups[2], "mediumrisk")))
  {
    return(0.7)
  }
  else if (isTRUE(all.equal(groups[1], groups[2], "lowrisk")))
  {
    return(0.2)
  }
  else if ((groups[1] == "highrisk" && groups[2] == "mediumrisk") ||
           (groups[1] == "mediumrisk" && groups[2] == "highrisk"))
  {
    return(0.4)
  }
  else if ((groups[1] == "highrisk" && groups[2] == "lowrisk") ||
           (groups[1] == "lowrisk" && groups[2] == "highrisk"))
  {
    return(0.2)
  }
  else if ((groups[1] == "mediumrisk" && groups[2] == "lowrisk") ||
           (groups[1] == "lowrisk" && groups[2] == "mediumrisk"))
  {
    return(0.2)
  }
}

estimate_contact_matrix <- function(province, groups)
{
  # Load data from Istat csv
  dataistat <- read.csv("data/istat/pop_prov_age.csv")
  data_to <- dataistat[dataistat$Territorio == province,]
  pop <- data_to$Value[data_to$Eta == "Total"]
  class_pop <- data_to$Value[data_to$Eta != "Total"]
  
  # 75-100
  highrisk <- class_pop[4]
  
  # 25-50 + 50-75
  mediumrisk <- class_pop[2] + class_pop[3]
  
  # 0-25
  lowrisk <- class_pop[1]
  
  # Sample size
  N <- 1000
  p <- get_risk_p(groups)
  n1 <- N*p
  n2 <- N-n1
  
  x <- sample(eval(parse(text = groups[1])), N)
  d <- sample(eval(parse(text = groups[2])), N)
  
  idx <- createDataPartition(x, p=p, list=FALSE)
  
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
  est <- stats4::mle(minuslog=nll, start=list(theta0=2, theta1=0))
  summary(est)
  
  y1 <- sample(x1, N-(N*p))
  w <- data.frame(y, y1)
  
  # Fit glm
  glm.fit <- glm(log(y)~y1, data=w)
  
  # Predict glm
  pred.lm <- predict(glm.fit, w)
  
  # Get results
  j <- mean(pred.lm)
  percnt <- j/(N-(N*p)) *100
  
  return(c(j, percnt))
}
