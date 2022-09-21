#### Essential skills tutorial: Bacteria growing on a resource ####

### 1. ODE solving using different methods ###
library(deSolve)

# Define the model in format acceptable for deSolve (and Grind)
model <- function(t, state, parms) {
  # state <- ifelse(state<0,0,state)
  with(as.list(c(state,parms)), {
    f <- v*R/(k+R)
    dtR <- - f*e*B
    dtB <- f*B
    return(list(c(dtR, dtB)))  
  }) 
} 

# Parameters
p <- c(e=5e-7,v=1.4,k=1)
# Initial conditions
s <- c(R=350,B=1e3)

# Time vector
timestep <- 0.005     
time <- seq(0,12,by=timestep)

# Solve the system using the Euler method
out <- ode(y=s, times=time, func=model, parms=p, method="euler", hini=timestep)
out_euler <- data.frame(out)

# Solve the system using Runge-Kutta 23, with dynamical time step
out <- ode(y=s, times=time, func=model, parms=p, method="ode23", hini=timestep)
out_rk23 <- data.frame(out)

# Plot results
par(mfrow=c(2,1))
plot(time,out_euler$R, type='l', col='red', lwd=3)
lines(time,out_rk23$R, type='l', col='orange', lwd=3)
plot(time,out_euler$B, type='l', col='blue', lwd=3, log="y")  # Note: logarithmic y-axis
lines(time, out_rk23$B, type='l', col='cyan', lwd=3)
