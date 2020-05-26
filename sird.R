# installation
source("https://kingaa.github.io/sbied/prep/packages.R")

# test that pomp works correctly
source("https://kingaa.github.io/scripts/pompTest.R")

# if the above fails
source("https://kingaa.github.io/scripts/hello.R",echo=TRUE)

###############
library(plyr)
library(tidyverse)
library(pomp)

dataProvinces <- read.csv("/run/media/nick/Windows/Users/nvera/OneDrive/Documents/School/Unimi/DSE/CovidInternship/dataProvinces.csv")

meas <- read.csv("https://kingaa.github.io/sbied/stochsim/Measles_Consett_1948.csv") %>%
  select(week,reports=cases)

sir_step <- function (S, I, R, H, N, Beta, mu_IR, delta.t, ...) {
  dN_SI <- rbinom(n=1,size=S,prob=1-exp(-Beta*I/N*delta.t))
  dN_IR <- rbinom(n=1,size=I,prob=1-exp(-mu_IR*delta.t))
  S <- S - dN_SI
  I <- I + dN_SI - dN_IR
  R <- R + dN_IR
  H <- H + dN_IR;
  c(S = S, I = I, R = R, H = H)
}

sir_init <- function (N, eta, ...) {
  c(S = round(N*eta), I = 1, R = round(N*(1-eta)), H = 0)
}

dmeas <- function (reports, H, rho, log, ...) {
  dbinom(x=reports, size=H, prob=rho, log=log)
}

rmeas <- function (H, rho, ...) {
  c(reports=rbinom(n=1, size=H, prob=rho))
}

meas %>% 
  pomp(times="week",t0=0,
    rprocess=euler(sir_step,delta.t=1/7),
    rinit=sir_init,accumvars="H"
  ) -> measSIR

measSIR %>% pomp(rmeasure=rmeas,dmeasure=dmeas) -> measSIR


measSIR %>%
  simulate(params=c(Beta=7.5,mu_IR=0.5,rho=0.5,eta=0.03,N=38000),
           nsim=20,format="data.frame",include.data=TRUE) -> sims

sims %>%
  ggplot(aes(x=week,y=reports,group=.id,color=.id=="data"))+
  geom_line()+
  guides(color=FALSE)
