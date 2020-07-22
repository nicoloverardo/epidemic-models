library(deSolve)
library(socialmixr)
source("mltransmission.R")

# ----------------

sirdModelFinal <- function(data_prov, real_cases, D=7, alpha=0.01,
                        rho=1/6, days=127,
                        R_0_start = 4.8, k = 0.1, x0 = 20,
                        R_0_end = 0.8, w = 0.2)
{
  calc_deriv <- function(t, y, parms)
  {
    n_compartment <- 4
    n_age <- length(y)/n_compartment
    
    S <- as.matrix(y[1:n_age])
    I <- as.matrix(y[(n_age+1):(2*n_age)])
    R <- as.matrix(y[(2*n_age+1):(3*n_age)])
    D <- as.matrix(y[(3*n_age+1):(4*n_age)])
    
    I[I<0] <- 0
    with(as.list(parms),
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
  
  # Recovery period
  gamma <- 1/D
  
  # Get population data
  #data_prov <- dataistatorig[dataistatorig$Territorio == province,]
  pop <- data_prov$Value[data_prov$Eta == "Total"]
  class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]
  
  # Number of people in each class
  N <- data_prov$Value[data_prov$Eta != "Total"]
  n_age <- length(class_percent)
  
  # Start with one infected in each class
  I_0 <- rep(1, n_age)
  S_0 <- N-I_0
  R_0 <- rep(0, n_age)
  D_0 <- rep(0, n_age)
  
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
  #class_percent <- mat$participants$proportion
  M <- C
  eig <- eigen(M)
  
  # Logistic R0
  calc_R_0 <- function(t) {
    return((R_0_start-R_0_end) / (1 + exp(-k*(-t+x0))) + R_0_end)
  }
  
  # Time dependent beta
  beta_fun <- function(t){
    return(calc_R_0(t)*gamma/max(Re(eig$values)))
  }
  
  # Initial values
  parms <- c(gamma = gamma, alpha = alpha, rho = rho, C = M)
  y = c(S=S_0,I=I_0,R=R_0, D=D_0)
  vt <- seq(1, days, 1)
  
  # Get the beta values
  beta_vals <- sapply(X=vt, FUN=beta_fun)
  Beta <- approxfun(x=beta_vals, method="linear", rule=2)
  
  # Get real number of infected
  cases <- real_cases
  cases <- sapply(cases, function(x){ 
    if (x>0) return(-x) else return(0)
  })
  
  # Reduce number of predicted infected with real data
  eventfun <- function(t, y, parms){
    with(as.list(y),
         {
           d <- cases[t]*class_percent
           if (y[4] > -d[1]){
             y[4] <- y[4] + (d[1] * w)
           }

           if (y[5] > -d[2]){
             y[5] <- y[5] + (d[2] * w)
           }

           if (y[6] > -d[3]){
             y[6] <- y[6] + (d[3] * w)
           }
           
           return(y)
         })
  }
  
  # Solve the model
  results <- as.data.frame(ode(y=y, times=vt, func=calc_deriv,
                               parms=parms, events = list(func=eventfun, time=vt)))
  
  # Replace int time with date
  results$time <- seq(as.Date("24 Feb 2020","%d %b %Y"), length.out = days, by=1)
  
  return(results)
}

# ----------------

sirdModel <- function(province="Torino", D=7, alpha=0.01, rho=1/6, R0=3, days=127, estMethod="Polymod")
{
  # The SIRD model.
  calc_deriv <- function(t, y, parms)
  {
    n_compartment <- 4
    n_age <- length(y)/n_compartment
    
    S <- as.matrix(y[1:n_age])
    I <- as.matrix(y[(n_age+1):(2*n_age)])
    R <- as.matrix(y[(2*n_age+1):(3*n_age)])
    D <- as.matrix(y[(3*n_age+1):(4*n_age)])
    
    I[I<0] <- 0
    with(as.list(parms),
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
  
  # Recovery period
  gamma <- 1/D
  
  # Get population data
  dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
  data_prov <- dataistat[dataistat$Territorio == province,]
  pop <- data_prov$Value[data_prov$Eta == "Total"]
  class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]
  
  # Number of people in each age class
  N <- pop*class_percent
  n_age <- length(class_percent)
  
  # Start with one infected in each class
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
  
  # Initial values
  parms <- c(gamma = gamma, beta = beta, alpha = alpha, rho = rho, C = C)
  y = c(S=S_0,I=I_0,R=R_0, D=D_0)
  vt <- seq(1, days, 1)
  
  # Solve the model
  results <- as.data.frame(ode(y=y, times=vt, func=calc_deriv, parms=parms))
  
  return(results)
}


# -----------------------------
# TIME DEPENDENT BETA

sirdModelTD <- function(province="Torino", D=7, alpha=0.01,
                        rho=1/6, days=127, estMethod="Polymod",
                        R_0_start = 5, k = 0.1, x0 = 20,
                        R_0_end = 0.8)
{
  calc_deriv <- function(t, y, parms)
  {
    n_compartment <- 4
    n_age <- length(y)/n_compartment
    
    S <- as.matrix(y[1:n_age])
    I <- as.matrix(y[(n_age+1):(2*n_age)])
    R <- as.matrix(y[(2*n_age+1):(3*n_age)])
    D <- as.matrix(y[(3*n_age+1):(4*n_age)])
    
    I[I<0] <- 0
    with(as.list(parms),
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
  
  # Recovery period
  gamma <- 1/D
  
  # Get population data
  dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
  data_prov <- dataistat[dataistat$Territorio == province,]
  pop <- data_prov$Value[data_prov$Eta == "Total"]
  class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]
  
  # Number of people in each class
  N <- pop*class_percent
  n_age <- length(class_percent)
  
  # Start with one infected in each class
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
  
  # Get beta from R0 and gamma
  eig <- eigen(M)
  
  # Time dependent beta
  beta_fun <- function(t){
    return(calc_R_0(t)*gamma/max(Re(eig$values)))
  }
  
  # Initial values
  parms <- c(gamma = gamma, alpha = alpha, rho = rho, C = C)
  y = c(S=S_0,I=I_0,R=R_0, D=D_0)
  vt <- seq(1, days, 1)
  
  # Get the beta values
  beta_vals <- sapply(X=vt, FUN=beta_fun)
  Beta <- approxfun(x=beta_vals, method="linear", rule=2)
  
  # Solve the model
  results <- as.data.frame(ode(y=y, times=vt, func=calc_deriv, parms=parms))
  
  return(results)
}


# ---------------
sirdModelTDweigthed <- function(province="Torino", D=7, alpha=0.01,
                                rho=1/6, days=134, estMethod="Polymod",
                                R_0_start = 5, k = 0.1, x0 = 20,
                                R_0_end = 0.8, dataProvinces=dataProvinces, w = 0.2)
{
  calc_deriv <- function(t, y, parms)
  {
    n_compartment <- 4
    n_age <- length(y)/n_compartment
    
    S <- as.matrix(y[1:n_age])
    I <- as.matrix(y[(n_age+1):(2*n_age)])
    R <- as.matrix(y[(2*n_age+1):(3*n_age)])
    D <- as.matrix(y[(3*n_age+1):(4*n_age)])
    
    I[I<0] <- 0
    with(as.list(parms),
         {
           N <- S+I+R+D
           dS <- -as.matrix(S*Beta(t))*(as.matrix(C)%*%as.matrix(I/N))
           dI <- -dS - gamma*as.matrix(I) - alpha*rho*as.matrix(I)
           dR <- +gamma*as.matrix(I)
           dD <- +alpha*rho*as.matrix(I)
           
           out <- list(c(dS,dI,dR,dD))
         })
  }
  
  gamma <- 1/D
  
  dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
  data_prov <- dataistat[dataistat$Territorio == province,]
  pop <- data_prov$Value[data_prov$Eta == "Total"]
  class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]
  
  N <- pop*class_percent
  n_age <- length(class_percent)
  
  I_0 <- rep(1, n_age)
  S_0 <- N-I_0
  R_0 <- rep(0, n_age)
  D_0 <- rep(0, n_age)
  
  
  if (estMethod == "Polymod")
  {
    data(polymod)
    
    survey_pop <- data_prov[1:3, c("Value", "Eta")]
    colnames(survey_pop) <- c("population", "lower.age.limit")
    survey_pop$lower.age.limit <- c(0, 25, 75)
    set.seed(1234)
    

    mat <- contact_matrix(polymod,
                          countries = "Italy",
                          age.limits = c(0, 25, 75, 100),
                          n = 10,
                          survey.pop = survey_pop,
                          symmetric = TRUE)
    
    C <- Reduce("+", lapply(mat$matrices, function(x) {x$matrix})) / length(mat$matrices)
    class_percent <- mat$participants$proportion
    M <- C
  }
  else if (estMethod == "GLM")
  {
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
  
  calc_R_0 <- function(t) {
    return((R_0_start-R_0_end) / (1 + exp(-k*(-t+x0))) + R_0_end)
  }
  
  beta_fun <- function(t){
    return(calc_R_0(t)*gamma/max(Re(eig$values)))
  }
  
  eig <- eigen(M)

  parms <- c(gamma = gamma, alpha = alpha, rho = rho, C = C)
  y = c(S=S_0,I=I_0,R=R_0, D=D_0)
  vt <- seq(1, days, 1)
  
  beta_vals <- sapply(X=vt,FUN=beta_fun)
  Beta <- approxfun(x=beta_vals, method="linear", rule=2)
  
  real_cases <- dataProvinces$New_cases
  real_cases <- sapply(real_cases, function(x){ if (x>0) { return(-x) } else { return(0) }})
  
  eventfun <- function(t, y, parms){
    with(as.list(y),
         {
           d <- real_cases[t]*class_percent
           if (y[4] > -d[1]){
             y[4] <- y[4] + (d[1] * w)
           }
           
           if (y[5] > -d[2]){
             y[5] <- y[5] + (d[2] * w)
           }
           
           if (y[6] > -d[3]){
             y[6] <- y[6] + (d[3] * w)
           }
           
           return(y)
         })
  }
  
  results <- as.data.frame(ode(y=y, times=vt, func=calc_deriv, parms=parms, events = list(func=eventfun, time=c(1:days))))
  
  return(results)
}

# ---------------
sirdModelTDWL <- function(province="Torino", D=7, alpha=0.01,
                          rho=1/6, days=134, estMethod="Polymod",
                          R_0_start = 5, k = 0.1, x0 = 20,
                          R_0_end = 0.8, dataProvinces=dataProvinces, w = 0.2)
{
  calc_deriv <- function(t, y, parms)
  {
    n_compartment <- 4
    n_age <- length(y)/n_compartment
    
    S <- as.matrix(y[1:n_age])
    I <- as.matrix(y[(n_age+1):(2*n_age)])
    R <- as.matrix(y[(2*n_age+1):(3*n_age)])
    D <- as.matrix(y[(3*n_age+1):(4*n_age)])
    
    I[I<0] <- 0
    with(as.list(parms),
         {
           N <- S+I+R+D
           dS <- -as.matrix(S*Beta(t))*(as.matrix(C)%*%as.matrix(I/N))
           dI <- -dS - gamma*as.matrix(I) - alpha*rho*as.matrix(I)
           dR <- +gamma*as.matrix(I)
           dD <- +alpha*rho*as.matrix(I)
           
           out <- list(c(dS,dI,dR,dD))
         })
  }
  
  
  # # Days
  # D=7
  #
  # # Death rate
  # alpha=0.05
  #
  # # Days that take an infected individual to die
  # rho=1/6
  #
  # #days=127
  # estMethod="Polymod"
  # province <- "Torino"
  # R_0_start = 5
  # k = 0.1
  # x0 = 20
  # R_0_end = 0.8

  
  # Recovery period
  gamma <- 1/D
  
  dataistat <- read.csv("data/istat/pop_prov_age_3_groups.csv")
  data_prov <- dataistat[dataistat$Territorio == province,]
  pop <- data_prov$Value[data_prov$Eta == "Total"]
  class_percent <- data_prov$Percentage[data_prov$Eta != "Total"]
  
  N <- pop*class_percent
  
  n_age <- length(class_percent)
  
  # Setting initial values with real data
  # totRealData <- nrow(dataProvinces)
  totRealData <- 40
  dp <- dataProvinces[1:totRealData, ]
  D_0 <- dp[totRealData, "Tot_deaths"]*class_percent
  I_0 <- dp[totRealData, "Curr_pos_cases"]*class_percent
  R_0 <- +gamma*as.matrix(I_0)
  S_0 <- N-D_0-I_0-R_0
  
  if (estMethod == "Polymod")
  {
    data(polymod)
    
    survey_pop <- data_prov[1:3, c("Value", "Eta")]
    colnames(survey_pop) <- c("population", "lower.age.limit")
    survey_pop$lower.age.limit <- c(0, 25, 75)
    set.seed(1234)
    
    mat <- contact_matrix(polymod,
                          countries = "Italy",
                          age.limits = c(0, 25, 75, 100),
                          n = 10,
                          survey.pop = survey_pop,
                          symmetric = TRUE)
    
    C <- Reduce("+", lapply(mat$matrices, function(x) {x$matrix})) / length(mat$matrices)
    class_percent <- mat$participants$proportion
    M <- C
  }
  else if (estMethod == "GLM")
  {
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
  
  calc_R_0 <- function(t) {
    return((R_0_start-R_0_end) / (1 + exp(-k*(-t+x0))) + R_0_end)
  }
  
  beta_fun <- function(t){
    return(calc_R_0(t)*gamma/max(Re(eig$values)))
  }
  
  # Will need to check this
  R_0_start <- calc_R_0(totRealData)
  
  eig <- eigen(M)
  
  parms <- c(gamma = gamma, alpha = alpha, rho = rho, C = C)
  y = c(S=S_0,I=I_0,R=R_0, D=D_0)
  
  vt <- seq(totRealData, days, 1)
  
  beta_vals <- sapply(X=vt,FUN=beta_fun)
  Beta <- approxfun(x=beta_vals, method="linear", rule=2)
  
  real_cases <- dataProvinces$New_cases
  real_cases <- sapply(real_cases, function(x){ if (x>0) { return(-x) } else { return(0) }})
  
  eventfun <- function(t, y, parms){
    with(as.list(y),
         {
           d <- real_cases[t]*class_percent
           if (y[4] > -d[1]){
             y[4] <- y[4] + (d[1] * w)
           }
           
           if (y[5] > -d[2]){
             y[5] <- y[5] + (d[2] * w)
           }
           
           if (y[6] > -d[3]){
             y[6] <- y[6] + (d[3] * w)
           }
           
           return(y)
         })
  }
  
  results <- as.data.frame(ode(y=y, times=vt, func=calc_deriv, parms=parms, events = list(func=eventfun, time=vt)))
  
  return(results)
}
