source("ml_transmission.R")
library(deSolve)

# x is a vector of length (#model compartment types)*(#age classes)
# vparameters are gamma, beta and the contact matrix C
calc_deriv<-function(t, x, vparameters)
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
         # dS, dI, dR, S, I, R and N will be of length n_age
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
dataistat <- read.csv("data/istat/pop_prov_age.csv")
data_to <- dataistat[dataistat$Territorio == "Torino",]
pop <- data_to$Value[data_to$Eta == "Total"]
class_percent <- data_to$Percentage[data_to$Eta != "Total"]
class_percent <- c(class_percent[1], class_percent[2] + class_percent[3], class_percent[4])

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

# Death rate
alpha <- 0.05
# Days that take an infected individual to die
rho <- 1/6


# We'll need a way to estimate R0
R0 <- 3

# Estimate contact matrix C
province <- "Torino"
a11 <- estimate_contact_matrix(province, c("highrisk", "highrisk"))[1]
a12 <- estimate_contact_matrix(province, c("highrisk", "mediumrisk"))[1]
a13 <- estimate_contact_matrix(province, c("highrisk", "lowrisk"))[1]
b11 <- estimate_contact_matrix(province, c("mediumrisk", "highrisk"))[1]
b12 <- estimate_contact_matrix(province, c("mediumrisk", "mediumrisk"))[1]
b13 <- estimate_contact_matrix(province, c("mediumrisk", "lowrisk"))[1]
c11 <- estimate_contact_matrix(province, c("lowrisk", "highrisk"))[1]
c12 <- estimate_contact_matrix(province, c("lowrisk", "mediumrisk"))[1]
c13 <- estimate_contact_matrix(province, c("lowrisk", "lowrisk"))[1]

C <- matrix(c(a11, a12, a13,
              b11, b12, b13,
              c11, c12, c13),
            nrow = 3, ncol = 3)

M <- C

for (i in 1:n_age){
  for (j in 1:n_age){
    M[i,j] <- C[i,j]*class_percent[i]/class_percent[j]
  }
}


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
