#lombardia basic model
library(EpiDynamics)
agegroup <- c('0-20','21-30','31-45','46-60','61-above')
population <- c('75523', '10060574', '108273','166358','125840')
lomabrdpop <- data.frame(agegroup,population)
#https://www.epicentro.iss.it/coronavirus/bollettino/Bolletino-sorveglianza-integrata-COVID-19_14-maggio-2020_appendix.pdf
#bolentino refrence for looking percentage of virus effecting on different age groupd as on 5 th may 

#Risk stratification
# refrence - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5555315/
stratum_agegroup <-  c('High Risk','Medium Risk','Low Risk')
#stratum_suspectible <-  c('125840','166444','831160')
stratum_suspectible <-  c('125840','274631','10136097')
suspectible_sample <- data.frame(stratum_agegroup,stratum_suspectible)

#stochastic model iing refrence - Modeling, Stochastic Control, Optimization, and Applications
#edited by George Yin, Qing Zhang
#providing different transmission rate to different Risk Groups 

#important
#TRANSMISSON RATES ARE CALUCLATEd BY FOLLOWING THIS REPORT https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4002176/
#pandemic modelling package used - https://cran.r-project.org/web/packages/EpiDynamics/EpiDynamics.pdf
#for calculations we used MATLAB


# Parameters and initial conditions.
parameters <- list(beta = matrix(c(2.42, 1.26, 0.80,0.01,
                                   1.26, 2.16, 0.20,0.01,
                                   0.80, 0.20, 0.75,0.01,
                                   0.01,0.01,0.01,0.01),
                                 nrow = 4, ncol = 4),
                   #sigma exposed to infectious , gamma= recovery rate
                   sigma =0.2  , gamma = 0.05 ,
                   mu = c(0.55,0.42,0.18,0.01) / (55 * 365),
                   nu = c(1 / (55 * 365), 0, 0, 0),
                   n = c(125840,274631,10136097,1000)/10537568)


initials <- c(S = c(0.59, 0.40, 0.01,0.1),
              E = c(0.40, 0.40, 0.40,0.01),
              I = c(0.50, 0.30, 0.01,0.01),  
              R = c(0.0298, 0.04313333, 0.12313333,1))
# Solve and plot.
seir4.age.classes <- SEIR4AgeClasses(pars = parameters,
                                     init = initials,
                                     time = 0:200)
PlotMods(seir4.age.classes,
         variables = c('I1', 'I2', 'I3', 'I4'), grid = F)
