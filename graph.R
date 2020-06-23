library(deSolve)
library(magrittr)
library(plotly)
# The SIRD model.
# x is a vector of length (#model compartment types)*(#age classes)
# vparameters are gamma, beta and the contact matrix C
calc_deriv <- function(t, x, vparameters)
{
  n_compartment <- 4
  n_age <- length(x)/n_compartment
  
  S <- as.matrix(x[1:n_age])
  I <- as.matrix(x[(n_age+1):(2*n_age)])
  R <- as.matrix(x[(2*n_age+1):(3*n_age)])
  D <- as.matrix(x[(3*n_age+1):(4*n_age)])
  
  I[I<0] <- 0
  with(as.list(vparameters),
       {
         # dS, dI, dR, S, I, R, dD, D and N will be of length n_age
         N <- S+I+R+D
         dS <- -as.matrix(S*beta)*(as.matrix(C)%*%as.matrix(I/N))
         dI <- -dS - gamma*as.matrix(I) - alpha*rho*as.matrix(I)
         dR <- +gamma*as.matrix(I)
         dD <- +alpha*rho*as.matrix(I)
         
         out <- list(c(dS,dI,dR,dD))
       })
}

# Example
# Turin
province <- "Torino"
dataistat <- read.csv("https://raw.githubusercontent.com/nicoloverardo/epidemic-models/master/data/istat/pop_prov_age_3_groups.csv")
data_to <- dataistat[dataistat$Territorio == province,]
pop <- data_to$Value[data_to$Eta == "Total"]
class_percent <- data_to$Percentage[data_to$Eta != "Total"]

# number in each age class
N <- pop*class_percent

n_age <- length(class_percent)

# start with one infected in each class
I_0 <- rep(1, n_age)
S_0 <- N-I_0
R_0 <- rep(0, n_age)
D_0 <- rep(0, n_age)

# Days
D <- 7

# Recovery period
gamma <- 1/D

# Death rate. We'll need to estimate from the data like 
# it's done in the app
alpha <- 0.05

# Days that take an infected individual to die
rho <- 1/6

# We'll need to estimate from the data like 
# it's done in the app
R0 <- 3

#####
# Estimate contact matrix C using our logistic regr.
# source("ml_transmission.R")
# a11 <- estimate_contact_matrix(province, c("highrisk", "highrisk"))[1]
# a12 <- estimate_contact_matrix(province, c("highrisk", "mediumrisk"))[1]
# a13 <- estimate_contact_matrix(province, c("highrisk", "lowrisk"))[1]
# b11 <- estimate_contact_matrix(province, c("mediumrisk", "highrisk"))[1]
# b12 <- estimate_contact_matrix(province, c("mediumrisk", "mediumrisk"))[1]
# b13 <- estimate_contact_matrix(province, c("mediumrisk", "lowrisk"))[1]
# c11 <- estimate_contact_matrix(province, c("lowrisk", "highrisk"))[1]
# c12 <- estimate_contact_matrix(province, c("lowrisk", "mediumrisk"))[1]
# c13 <- estimate_contact_matrix(province, c("lowrisk", "lowrisk"))[1]
# 
# C <- matrix(c(25, 1, 4,
#               3, 1, 24,
#               2, 50, 75),
#             nrow = 3, ncol = 3)
# M <- C
# 
# for (i in 1:n_age){
#   for (j in 1:n_age){
#     M[i,j] <- C[i,j]*class_percent[i]/class_percent[j]
#   }
# }
####

####
# Trying to estimate using socialmixr: (https://cran.r-project.org/web/packages/socialmixr/vignettes/introduction.html). 
# It uses POLYMOD survey data:
# (https://journals.plos.org/plosmedicine/article/file?id=10.1371/journal.pmed.0050074&type=printable).
# They are given at national level. However, by specifying the
# 'survey.pop' parameter, we weight them with the actual population of the province
library("socialmixr")
data(polymod)

# Prepare dataframe for 'survey.pop' param
# as described in the socialmixr docs.
survey_pop <- data_to[1:3, c("Value", "Eta")]
colnames(survey_pop) <- c("population", "lower.age.limit")
survey_pop$lower.age.limit <- c(0, 25, 75)

# Estimate the contact matrix, using also bootstrap n=10
mat <- contact_matrix(polymod, 
                      countries = "Italy",
                      age.limits = c(0, 25, 75, 100),
                      n = 10, 
                      survey.pop = survey_pop,
                      symmetric = TRUE)

# 'mat' will contain a field 'matrices' with n matrices, where
# n is the param we specified above in the bootstrap. 'Reduce'
# will average them.
C <- Reduce("+", lapply(mat$matrices, function(x) {x$matrix})) / length(mat$matrices)
class_percent <- mat$participants$proportion
M <- C

# Get beta from R0 and gamma
eig <- eigen(M)
beta <- R0*gamma/max(Re(eig$values))  
beta <- beta

# Solve the model
vparameters <- c(gamma = gamma, beta = beta, alpha = alpha, rho = rho, C = C)
inits = c(S=S_0,I=I_0,R=R_0, D=D_0)

# S,I and R for various t
vt <- seq(10, 200, 1)  
results <- as.data.frame(lsoda(inits, vt, calc_deriv, vparameters))

# Plots
par(mfrow<-c(1,1))

# I
plot(results$time,
     results$I1/N[1],
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,1),
     lwd=2,
     col=2,
     main="Age-structured SIR (I)")
lines(results$time,
      results$I2/N[2],
      col=3,
      lwd=2)
lines(results$time,
      results$I3/N[3],
      col=4,
      lwd=2)
legend("topright",
       legend=c("0-25","25-75", ">75"),
       col=c(2,3,4),
       lwd=2)

# S
plot(results$time,
     results$S1/N[1],
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,1),
     lwd=2,
     col=2,
     main="Age-structured SIR (S)")
lines(results$time,
      results$S2/N[2],
      col=3,
      lwd=2)
lines(results$time,
      results$S3/N[3],
      col=4,
      lwd=2)
legend("topright",
       legend=c("0-25","25-75", ">75"),
       col=c(2,3,4),
       lwd=2)

# R
plot(results$time,
     results$R1/N[1],
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,1),
     lwd=2,
     col=2,
     main="Age-structured SIR (R)")
lines(results$time,
      results$R2/N[2],
      col=3,
      lwd=2)
lines(results$time,
      results$R3/N[3],
      col=4,
      lwd=2)
legend("topright",
       legend=c("0-25","25-75", ">75"),
       col=c(2,3,4),
       lwd=2)

# D
plot(results$time,
     results$D1/N[1],
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,1),
     lwd=2,
     col=2,
     main="Age-structured SIR (D)")
lines(results$time,
      results$D2/N[2],
      col=3,
      lwd=2)
lines(results$time,
      results$D3/N[3],
      col=4,
      lwd=2)
legend("topright",
       legend=c("0-25","25-75", ">75"),
       col=c(2,3,4),
       lwd=2)

I1 <- results$I1
I2 <- results$I2
I3 <- results$I3

library("rjson")
data <- ("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province-latest.json") 
data1 <- fromJSON(file=data)

p <-
  plot_ly(
    results,
    x = results$time,
    y =  I1+I2+I3,
    type = 'scatter',
    mode = 'line+markers',
    line = list(color = 'rgb(205, 12, 24)', width = 4)
  )
p <- p %>% add_trace(y =I1 , name = 'MEDIUM PREDICTION',mode = 'markers')
p <- p %>% add_trace(y =I2 , name = 'HIGH RISK PREDICTION',mode = 'markers')
p <- p %>% add_trace(y =I3 , name = 'LOW RISK PREDICTION',mode = 'markers')
p <- p %>% add_trace(y =I3 , name = 'LOW RISK PREDICTION',mode = 'markers')


p %>%
  layout(
    title = "Totacl Cases",
    xaxis = list(title = "Time"),
    yaxis = list (title = "Age Groups")
  )
p







xq <- c('High Risk', 'Medium Risk', 'Low Risk')
yq <- c(sum(results$D1), sum(results$D2), sum(results$D3))
text <- c('60 years and above', '30-60 years', '0-30')
data11 <- data.frame(x, y, text)

m <- plot_ly(data11, x = ~xq, y = ~yq, type = 'bar', text = text,
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5)))
m <- m %>% layout(title = "PREDICTED DEATH IN DIFFERENT AGE GROUPS",
                  xaxis = list(title = ""),
                  yaxis = list(title = ""))

m



l <- plot_ly(results, x = ~results$R1, y = ~results$S1, z = ~results$I1,
               marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
l <- l %>% add_markers()
l <- l %>% layout(scene = list(xaxis = list(title = 'High Risk Recovery'),
                                   yaxis = list(title = 'High Risk Suspectible'),
                                   zaxis = list(title = 'High Risk Infected')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Comparison High RISK SIR',
                        xref = 'Comparison',
                        yref = 'Comparison',
                        showarrow = FALSE
                      ))
l
