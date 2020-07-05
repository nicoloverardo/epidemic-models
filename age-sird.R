library(deSolve)
library("socialmixr")
source("ml_transmission.R")

sirdModel <- function(province="Torino", D=7, alpha=0.01, rho=1/6, R0=3, days=127, estMethod="Polymod")
{
  # The SIRD model.
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
  
  # Days
  # D=7
  
  # Death rate
  # alpha=0.05
  
  # Days that take an infected individual to die
  # rho=1/6
  
  # R0=3
  # days=122
  # estMethod="Polymod"
  
  # province <- "Torino"
  
  # Recovery period
  gamma <- 1/D
  
  dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
  data_prov <- dataistat[dataistat$Territorio == province,]
  pop <- data_prov$Value[data_prov$Eta == "Total"]
  class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]
  
  # number in each age class
  N <- pop*class_percent
  
  n_age <- length(class_percent)
  
  # start with one infected in each class
  I_0 <- rep(1, n_age)
  S_0 <- N-I_0
  R_0 <- rep(0, n_age)
  D_0 <- rep(0, n_age)
  
  
  if (estMethod == "Polymod")
  {
    # Trying to estimate the contact matrix using socialmixr: 
    # (https://cran.r-project.org/web/packages/socialmixr/vignettes/introduction.html). 
    # It uses POLYMOD survey data:
    # (https://journals.plos.org/plosmedicine/article/file?id=10.1371/journal.pmed.0050074&type=printable).
    data(polymod)
    
    # Prepare dataframe for 'survey.pop' param
    # as described in the socialmixr docs.
    survey_pop <- data_prov[1:3, c("Value", "Eta")]
    colnames(survey_pop) <- c("population", "lower.age.limit")
    survey_pop$lower.age.limit <- c(0, 25, 75)
    set.seed(1234)
    
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
  }
  else if (estMethod == "GLM")
  {
    # Estimate contact matrix C using our logistic regr.
    
    a11 <- estimate_contact_matrix(province, c("highrisk", "highrisk"))[1]
    a12 <- estimate_contact_matrix(province, c("highrisk", "mediumrisk"))[1]
    a13 <- estimate_contact_matrix(province, c("highrisk", "lowrisk"))[1]
    b11 <- estimate_contact_matrix(province, c("mediumrisk", "highrisk"))[1]
    b12 <- estimate_contact_matrix(province, c("mediumrisk", "mediumrisk"))[1]
    b13 <- estimate_contact_matrix(province, c("mediumrisk", "lowrisk"))[1]
    c11 <- estimate_contact_matrix(province, c("lowrisk", "highrisk"))[1]
    c12 <- estimate_contact_matrix(province, c("lowrisk", "mediumrisk"))[1]
    c13 <- estimate_contact_matrix(province, c("lowrisk", "lowrisk"))[1]
    
    C <- matrix(c(a11,a12,a13,
                  b11,b12,b13,
                  c11,c12,c13),
                nrow = 3, ncol = 3)
    M <- C
    
    for (i in 1:n_age){
      for (j in 1:n_age){
        M[i,j] <- C[i,j]*class_percent[i]/class_percent[j]
      }
    }
  }
  else
  {
    stop("Estimation method not known. Please choose Polymod or GLM.")
  }
  
  
  # Get beta from R0 and gamma
  eig <- eigen(M)
  beta <- R0*gamma/max(Re(eig$values))  
  #beta <- beta
  
  # Solve the model
  vparameters <- c(gamma = gamma, beta = beta, alpha = alpha, rho = rho, C = C)
  inits = c(S=S_0,I=I_0,R=R_0, D=D_0)
  
  # S,I and R for various t
  vt <- seq(1, days, 1)
  results <- as.data.frame(ode(y=inits, times=vt, func=calc_deriv, parms=vparameters))
  
  return(results)
}


# -----------------------------
# TIME DEPENDENT BETA

sirdModelTD <- function(province="Torino", D=7, alpha=0.01,
                        rho=1/6, days=127, estMethod="Polymod",
                        R_0_start = 5, k = 0.1, x0 = 20,
                        R_0_end = 0.8)
{
  # The SIRD model.
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
           dS <- -as.matrix(S*Beta(t))*(as.matrix(C)%*%as.matrix(I/N))
           dI <- -dS - gamma*as.matrix(I) - alpha*rho*as.matrix(I)
           dR <- +gamma*as.matrix(I)
           dD <- +alpha*rho*as.matrix(I)

           out <- list(c(dS,dI,dR,dD))
         })
  }
  
  # Days
  #D=7
  
  # Death rate
  #alpha=0.05
  
  # Days that take an infected individual to die
  #rho=1/6
  
  #R0=3
  #days=127
  #estMethod="Polymod"
  
  #province <- "Torino"
  
  # Recovery period
  gamma <- 1/D
  
  dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
  data_prov <- dataistat[dataistat$Territorio == province,]
  pop <- data_prov$Value[data_prov$Eta == "Total"]
  class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]
  
  # number in each age class
  N <- pop*class_percent
  
  n_age <- length(class_percent)
  
  # start with one infected in each class
  I_0 <- rep(1, n_age)
  S_0 <- N-I_0
  R_0 <- rep(0, n_age)
  D_0 <- rep(0, n_age)
  
  
  if (estMethod == "Polymod")
  {
    # Trying to estimate the contact matrix using socialmixr: 
    # (https://cran.r-project.org/web/packages/socialmixr/vignettes/introduction.html). 
    # It uses POLYMOD survey data:
    # (https://journals.plos.org/plosmedicine/article/file?id=10.1371/journal.pmed.0050074&type=printable).
    data(polymod)
    
    # Prepare dataframe for 'survey.pop' param
    # as described in the socialmixr docs.
    survey_pop <- data_prov[1:3, c("Value", "Eta")]
    colnames(survey_pop) <- c("population", "lower.age.limit")
    survey_pop$lower.age.limit <- c(0, 25, 75)
    set.seed(1234)
    
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
  }
  else if (estMethod == "GLM")
  {
    # Estimate contact matrix C using our logistic regr.
    
    a11 <- estimate_contact_matrix(province, c("highrisk", "highrisk"))[1]
    a12 <- estimate_contact_matrix(province, c("highrisk", "mediumrisk"))[1]
    a13 <- estimate_contact_matrix(province, c("highrisk", "lowrisk"))[1]
    b11 <- estimate_contact_matrix(province, c("mediumrisk", "highrisk"))[1]
    b12 <- estimate_contact_matrix(province, c("mediumrisk", "mediumrisk"))[1]
    b13 <- estimate_contact_matrix(province, c("mediumrisk", "lowrisk"))[1]
    c11 <- estimate_contact_matrix(province, c("lowrisk", "highrisk"))[1]
    c12 <- estimate_contact_matrix(province, c("lowrisk", "mediumrisk"))[1]
    c13 <- estimate_contact_matrix(province, c("lowrisk", "lowrisk"))[1]
    
    C <- matrix(c(a11,a12,a13,
                  b11,b12,b13,
                  c11,c12,c13),
                nrow = 3, ncol = 3)
    M <- C
    
    for (i in 1:n_age){
      for (j in 1:n_age){
        M[i,j] <- C[i,j]*class_percent[i]/class_percent[j]
      }
    }
  }
  else
  {
    stop("Estimation method not known. Please choose Polymod or GLM.")
  }
  
  # Logistic R0
  calc_R_0 <- function(t) {
    return((R_0_start-R_0_end) / (1 + exp(-k*(-t+x0))) + R_0_end)
  }
  
  # Time dependent beta
  beta_fun <- function(t){
    return(calc_R_0(t)*gamma/max(Re(eig$values)))
  }
  
  # Get beta from R0 and gamma
  eig <- eigen(M)
  
  # Solve the model
  vparameters <- c(gamma = gamma, alpha = alpha, rho = rho, C = C)
  inits = c(S=S_0,I=I_0,R=R_0, D=D_0)
  
  # S,I and R for various t
  vt <- seq(1, days, 1)
  
  # Get the beta values
  beta_vals <- sapply(X=vt,FUN=beta_fun)
  Beta <- approxfun(x=beta_vals, method="linear", rule=2)
  
  results <- as.data.frame(ode(y=inits, times=vt, func=calc_deriv, parms=vparameters))
  
  return(results)
}

# ---------------
sirdModelTDweigthed <- function(province="Torino", D=7, alpha=0.01,
                        rho=1/6, days=127, estMethod="Polymod",
                        R_0_start = 5, k = 0.1, x0 = 20,
                        R_0_end = 0.8, dataProvinces=dataProvinces)
{
  # The SIRD model.
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
           dS <- -as.matrix(S*Beta(t))*(as.matrix(C)%*%as.matrix(I/N))
           dI <- -dS - gamma*as.matrix(I) - alpha*rho*as.matrix(I)
           dR <- +gamma*as.matrix(I)
           dD <- +alpha*rho*as.matrix(I)
           
           out <- list(c(dS,dI,dR,dD))
         })
  }
  
  # Days
  #D=7
  
  # Death rate
  #alpha=0.05
  
  # Days that take an infected individual to die
  #rho=1/6
  
  #R0=3
  #days=127
  #estMethod="Polymod"
  
  #province <- "Torino"
  
  # Recovery period
  gamma <- 1/D
  
  dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
  data_prov <- dataistat[dataistat$Territorio == province,]
  pop <- data_prov$Value[data_prov$Eta == "Total"]
  class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]
  
  # number in each age class
  N <- pop*class_percent
  
  n_age <- length(class_percent)
  
  # start with one infected in each class
  I_0 <- rep(1, n_age)
  S_0 <- N-I_0
  R_0 <- rep(0, n_age)
  D_0 <- rep(0, n_age)
  
  
  if (estMethod == "Polymod")
  {
    # Trying to estimate the contact matrix using socialmixr: 
    # (https://cran.r-project.org/web/packages/socialmixr/vignettes/introduction.html). 
    # It uses POLYMOD survey data:
    # (https://journals.plos.org/plosmedicine/article/file?id=10.1371/journal.pmed.0050074&type=printable).
    data(polymod)
    
    # Prepare dataframe for 'survey.pop' param
    # as described in the socialmixr docs.
    survey_pop <- data_prov[1:3, c("Value", "Eta")]
    colnames(survey_pop) <- c("population", "lower.age.limit")
    survey_pop$lower.age.limit <- c(0, 25, 75)
    set.seed(1234)
    
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
  }
  else if (estMethod == "GLM")
  {
    # Estimate contact matrix C using our logistic regr.
    
    a11 <- estimate_contact_matrix(province, c("highrisk", "highrisk"))[1]
    a12 <- estimate_contact_matrix(province, c("highrisk", "mediumrisk"))[1]
    a13 <- estimate_contact_matrix(province, c("highrisk", "lowrisk"))[1]
    b11 <- estimate_contact_matrix(province, c("mediumrisk", "highrisk"))[1]
    b12 <- estimate_contact_matrix(province, c("mediumrisk", "mediumrisk"))[1]
    b13 <- estimate_contact_matrix(province, c("mediumrisk", "lowrisk"))[1]
    c11 <- estimate_contact_matrix(province, c("lowrisk", "highrisk"))[1]
    c12 <- estimate_contact_matrix(province, c("lowrisk", "mediumrisk"))[1]
    c13 <- estimate_contact_matrix(province, c("lowrisk", "lowrisk"))[1]
    
    C <- matrix(c(a11,a12,a13,
                  b11,b12,b13,
                  c11,c12,c13),
                nrow = 3, ncol = 3)
    M <- C
    
    for (i in 1:n_age){
      for (j in 1:n_age){
        M[i,j] <- C[i,j]*class_percent[i]/class_percent[j]
      }
    }
  }
  else
  {
    stop("Estimation method not known. Please choose Polymod or GLM.")
  }
  
  # Logistic R0
  calc_R_0 <- function(t) {
    return((R_0_start-R_0_end) / (1 + exp(-k*(-t+x0))) + R_0_end)
  }
  
  # Time dependent beta
  beta_fun <- function(t){
    return(calc_R_0(t)*gamma/max(Re(eig$values)))
  }
  
  # Get beta from R0 and gamma
  eig <- eigen(M)
  
  # Solve the model
  vparameters <- c(gamma = gamma, alpha = alpha, rho = rho, C = C)
  inits = c(S=S_0,I=I_0,R=R_0, D=D_0)
  
  # S,I and R for various t
  vt <- seq(1, days, 1)
  
  # Get the beta values
  beta_vals <- sapply(X=vt,FUN=beta_fun)
  Beta <- approxfun(x=beta_vals, method="linear", rule=2)
  
  real_cases <- dataProvinces[dataProvinces$Province == province,]$New_cases
  real_cases <- sapply(real_cases, function(x){ if (x>0) { return(-x) } else { return(x) }})
  
  eventfun <- function(t, x, vparameters){
    n_compartment <- 4
    n_age <- length(x)/n_compartment
    
    S <- as.matrix(x[1:n_age])
    I <- as.matrix(x[(n_age+1):(2*n_age)]) + rep(real_cases[t]/3, 3)
    R <- as.matrix(x[(2*n_age+1):(3*n_age)])
    D <- as.matrix(x[(3*n_age+1):(4*n_age)])
    
    return(c(S,I,R,D))
  }
  
  results <- as.data.frame(ode(y=inits, times=vt, func=calc_deriv, parms=vparameters, events = list(fun = eventfun, times=2:days)))
  
  return(results)
}
