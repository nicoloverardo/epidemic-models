library(deSolve)

# x is a vector of length (#model compartment types)*(#age classes)
# vparameters are gamma, beta and the contact matrix C
calc_deriv<-function(t, x, vparameters)
{
  n_compartment <- 3
  n_age <- length(x)/n_compartment
  
  S <- as.matrix(x[1:n_age])
  I <- as.matrix(x[(n_age+1):(2*n_age)])
  R <- as.matrix(x[(2*n_age+1):(3*n_age)])
  
  I[I<0] <- 0
  with(as.list(vparameters),
       {
         # dS, dI, dR, S, I, R and N will be of length n_age
         N <- S+I+R
         dS <- -as.matrix(S*beta)*(as.matrix(C)%*%as.matrix(I/N))
         dI <- -dS - gamma*as.matrix(I)
         dR <- +gamma*as.matrix(I)
         
         out <- list(c(dS,dI,dR))
       })
}

# Example
# Turin
dataistat <- read_csv("pop_prov_age.csv")
data_to <- dataistat[dataistat$Territorio == "Torino",]
pop <- data_to$Value[data_to$Eta == "Total"]
class_percent <- data_to$Percentage[data_to$Eta != "Total"]

# number in each age class
N <- pop*class_percent

n_age <- length(class_percent)

# start with one infected in each class
I_0 <- rep(1, n_age)
S_0 <- N-I_0
R_0 <- rep(0, n_age)

# Days
D <- 7

# Recovery period
gamma <- 1/D

# We'll need a way to estimate R0
R0 <- 3

# Contact matrix. Don't know if we have data for that
# Page 20-21 of Rock K., Brand S., Moir J., Keeling M.J., Dynamics of Infectious Diseases
C <- matrix(c(2.42, 1.26, 0.80, 0.01,
         1.26, 2.16, 0.20, 0.01,
         0.80, 0.20, 0.75, 0.01,
         0.01, 0.01, 0.01, 0.01),
         nrow = 4, ncol = 4)

M <- C

for (i in 1:4){
  for (j in 1:4){
    M[i,j] <- C[i,j]*class_percent[i]/class_percent[j]
  }
}


# Get beta from R0 and gamma
eig <- eigen(M)
beta <- R0*gamma/max(Re(eig$values))  
beta <- beta

# Solve the model
vparameters <- c(gamma = gamma, beta = beta, C = C)
inits = c(S=S_0,I=I_0,R=R_0)

# S,I and R for various t
vt <- seq(10, 200, 1)  
results <- as.data.frame(lsoda(inits, vt, calc_deriv, vparameters))

# Plots
par(mfrow<-c(1,1))

plot(results$time,
     results$I1/N[1],
     type="l",
     xlab="days",
     ylab="Individuals",
     ylim=c(0,1),
     lwd=2,
     col=2,
     main="Age-structured SIR")
lines(results$time,
      results$I2/N[2],
      col=3,
      lwd=2)
lines(results$time,
      results$I3/N[3],
      col=4,
      lwd=2)
lines(results$time,
      results$I4/N[4],
      col=5,
      lwd=2)

# lines(results$time,
#       results$S1/N[1],
#       col=4,
#       lwd=2)
# lines(results$time,
#       results$S2/N[2],
#       col=5,
#       lwd=2)
# 
# lines(results$time,
#       results$R1/N[1],
#       col=6,
#       lwd=2)
# lines(results$time,
#       results$R2/N[2],
#       col=7,
#       lwd=2)

legend("topright",
       legend=c("0-25","25-50", "50-75", ">75"),
       col=c(2,3,4,5),
       lwd=2)
