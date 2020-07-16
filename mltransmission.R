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
  dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
  data_prov <- dataistat[dataistat$Territorio == province,]
  pop <- data_prov$Value[data_prov$Eta == "Total"]
  class_pop <- data_prov$Value[data_prov$Eta != "Total"]
  
  # 75-100
  highrisk <- class_pop[3]
  
  # 25-75
  mediumrisk <- class_pop[2]
  
  # 0-25
  lowrisk <- class_pop[1]
  
  x <- sample(mediumrisk)
  d <- sample(highrisk)

  p <- get_risk_p(groups)
  
  idx <- createDataPartition(x, p=p, list=FALSE)

  
  x1 <- as.vector(x)
  y2 <- (d[-idx])
  y <- sample(y2)
  
  lenght = NROW(y)
  k=sample(x1,lenght)
  
  w <- data.frame(k,y)
  glm.fit <-  glm((k)~y,data=w,family = poisson)
  
  return(glm.fit$coefficients)
}
