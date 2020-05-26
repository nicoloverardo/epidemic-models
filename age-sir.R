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
# We'll need to put real data here
pop <- 5000000
class_percent <- c(0.15,0.85)

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
C <- matrix(0, nrow=n_age, ncol=n_age)

C[1,1] <- 20 # young young
C[1,2] <- 8 # young adults
C[2,1] <- 5 # adults young
C[2,2] <- 17 # adults adults

M <- C
M[1,1] <- C[1,1]*class_percent[1]/class_percent[1]
M[1,2] <- C[1,2]*class_percent[1]/class_percent[2]
M[2,1] <- C[2,1]*class_percent[2]/class_percent[1]
M[2,2] <- C[2,2]*class_percent[2]/class_percent[2]


# Get beta from R0 and gamma
eig <- eigen(M)
beta <- R0*gamma/max(Re(eig$values))  
beta <- beta

# Solve the model
vparameters <- c(gamma = gamma, beta = beta, C = C)
inits = c(S=S_0,I=I_0,R=R_0)

# S,I and R for various t
vt <- seq(10, 130, 1)  
results <- as.data.frame(lsoda(inits, vt, calc_deriv, vparameters))

# Plots
par(mfrow<-c(1,1))

# Ensure the plot have the same max value
ymax1 <- max(c(results$I1/N[1],results$I2/N[2]))
ymax2 <- max(c(results$S1/N[1],results$S2/N[2]))
ymax3 <- max(c(results$R1/N[1],results$R2/N[2]))
ymax <- max(c(ymax1,ymax2,ymax3))

plot(results$time,
     results$I1/N[1],
     type="l",
     xlab="days",
     ylab="Sub-pop infected",
     ylim=c(0,ymax),
     lwd=2,
     col=2,
     main="Age-structured SIR")
lines(results$time,
      results$I2/N[2],
      col=3,
      lwd=2)

lines(results$time,
      results$S1/N[1],
      col=4,
      lwd=2)
lines(results$time,
      results$S2/N[2],
      col=5,
      lwd=2)

lines(results$time,
      results$R1/N[1],
      col=6,
      lwd=2)
lines(results$time,
      results$R2/N[2],
      col=7,
      lwd=2)

legend("topright",
       legend=c("I_y","I_a", "S_y", "S_a", "R_y", "R_a"),
       col=c(2,7),
       lwd=2)
