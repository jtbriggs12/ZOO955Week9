#HW Week 9

#Jess Briggs and David Ortiz

library(tidyverse)

#Q1 - generate and plot data

r <- 0.2
k <- 100

time = seq(from=1, to=50)
N=array(dim=c(length(time), 1)); N[1]=5
colnames(N) <- "population"

for (i in time[2:length(time)]){
  N[i] = N[i-1] + (r*N[i-1]) * (1-(N[i-1]/k))
}

N2 <- as.data.frame(N)

N2$time <- as.numeric(time)

ggplot(data = N2, aes(x = time, y = population))+
  geom_point()

#Q2 - create a model for Q1 using optim


calc_nll <- function(y_obs, y_pred, sigma){
  -sum(dnorm(x=y_obs, mean=y_pred, sd=sigma, log=T))
}


k0 <- 4
r0 <- 0.3
N <- x
k <- 100

obj_func <- function(par){
  r0 <- par[1]
  k0 <- par[2]
  sigma <- par[3]
  y_pred <-  k0 + (r0*N) * (1-(N/k))
  nll <- calc_nll(N2$population, y_pred, sigma)
}

# Estimate parameters using optim()
optfit <- optim(par=c(0.3, 4, 10), fn = obj_func)
optcoefs <- optfit$par

optcoefs

time = seq(from=1, to=50)
test =array(dim=c(length(time), 1)); test[1]=optcoefs[2]

for (i in time[2:length(time)]){
  test[i] = test[i-1]+ (optcoefs[1]*test[i-1]) * (1-(test[i-1]/k))
}

plot(test)
