#' Getting started with Ram model and Grind


#' First we need to source the file grind.R 
#' which is a wrapper made around the deSolve, rootSolve etc packages 

#loading required packages and sourcing Grind
if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(tidyverse, rootSolve, deSolve, FME)
source("grind.R")


#' Below we define a function called model in a format appropriate for 
#' deSolve and Grind. This function can be used to calculate the values 
#' of the differential equation dN at different time points through 
#' numerical integration. As arguments it uses t (for a vector of time points), 
#' state (for a vector containing the initial state of the variables in the system), 
#' and parms (for a vector containing the values of the parameters of the system). 
#' State and parameters should be named vectors.

#' The with(as.list(c(state, parms))) does nothing more than make these names 
#' available without having to write parms["v"] every time you want to use 
#' v from the parameters vector, and parms["q0"] every time you want to use q0.

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    a <- q0/(q0+exp(-m*t))
    dN <- r*a*N*(1-(N/K)^v)
    return(list(dN))  
  }) 
}


#' Below we specify the vector p with parameter values
p <- c(K=0.6,r=0.4,m=2,q0=0.005,v=2)

#' Next we specify the vector s with inital state of the variables
s <- c(N = 0.124) #units are OD.

#' Now we are going to numerically integrate the model, 
#' specifying the time interval, timestep of integration, initial state, 
#' parameter values and which model to solve

run(tmax = 100, tstep = 1,
    state = s,
    parms = p,
    odes = model
)
