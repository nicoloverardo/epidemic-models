library(stats)
library(stats4)
library(caret)
library(glm2)
# Example
# Turin
dataistat <- read.csv("https://raw.githubusercontent.com/nicoloverardo/epidemic-models/master/data/istat/pop_prov_age_4_groups.csv")
data_to <- dataistat[dataistat$Territorio == "Torino",]
pop <- data_to$Value[data_to$Eta == "Total"]
class_pop <- data_to$Value[data_to$Eta != "Total"]

# We're gonna need to turn this into 4 groups and
# calculate this for each group

# 75-100
highrisk <- class_pop[4]

# 25-50 + 50-75
mediumrisk <- class_pop[2] + class_pop[3]

#finding how already infected high risk people can infect medium risk people

x <- sample(mediumrisk) #getting sample/combination of population 
d <- sample(highrisk) #getting sample/combination of population 

#p is division between people infected and not infected in a population.
#p=0.40  means 40 percent of population is not infected

idx <- createDataPartition(x, p=0.60, list=FALSE)


#x1  ,simply x, population medium risk
#y  , high risk - non infected population = infected high risk population

x1 <- as.vector(x)
y2 <- (d[-idx])
y <- sample(y2)

#j is y,just to make th elenght of data equal with x1(to make same rows and coloumns for w)
lenght = NROW(y)
#max.len = max(length(y), length(x1))
k=sample(x1,lenght)
# w is a dataframe
# 1 w shows various combinations of infected high risk with population medium risk
# means how many combinations of infected population can come in contact with
#mediun risk people
#for example 100 infected people come in contact with 5000 mediun risk people
#different possible combinations

w <- data.frame(k,y)
# regressing with probility that how different combinations of infected people
#can actually transmit disesase to medium risk people.. from w

######3 i think we need to add a BIAS for every agr gropu to get different ratios
glm.fit <-  glm((k)~y,data=w,family = poisson)

summary(glm.fit)
glm.fit$coefficients
