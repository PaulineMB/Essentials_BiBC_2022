#' Basic Euler integration


if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(tidyverse)


#' Integration time: a sequence from zero to ten at timestep steps
timestep = 0.5
time <- seq(0, 10, by = timestep)
#' Initial condition (starting value of the variable at time zero)
x0 <- 0.1
#' The differential equation to be integrated (see tutorial)
f <- function(x){x * (1.-x)}

#' An empty R vector to store the integration results 
#' (x value per timestep)
x <- c()
#' Store the initial condition in the first position of this vector
x[1] <- x0

#' Loop over time: approximate the solution at each time step
for (i in 1:(length(time)-1)){
  x[i+1] = x[i] + timestep * f(x[i])
}

#' Basic R plotting 
#' Plot the obtained x values over time
plot(x~time)
#' Add to the plot the known numerical solution
curve(0.1 * exp(x)/(1+0.1*(exp(x)-1.)), add=T)
#' Add a legend
legend("topleft", c("approximation", "analytical"), pch=c(1,NA), lty=c(NA,1))
