#' Question 2 e + f Answers


#setup, loading deSolve
if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(tidyverse, cowplot, deSolve)


#' Defining the model:
  
#' This format for defining an ODE model is needed for 
#' deSolve (and Grind). We will explain how this model 
#' definition works in more detail in the next practical 
model <- function(t, state, parms) {
  #state <- ifelse(state<0,0,state)
  with(as.list(c(state,parms)), {
    f <- v*R/(k+R)
    dtR <- - f*e*B
    dtB <- f*B
    return(list(c(dtR, dtB)))  
  }) 
} 

#' Parameter values are defined here and are put in a vector p
p <- c(e=5e-7,v=1.4,k=1)
#' Initial condition values are defined here and put in a 
#' vector s (state). Note: This HAS to be in the order 
#' in which you define the equations in the list of 
#' functions returned by model. So same order as dtR and dtB
s <- c(R=350,B=1e3)

#' As before we define a time vector with the sequence of 
#' integration timesteps
timestep <- 0.05     
time <- seq(0,15,by=timestep)

#' Solve the system using the Euler method
#' Note that we added some code at the start and end to 
#' monitor the time it takes the computer to numerically 
#' solve this equation
startTiming = proc.time()
out <- ode(y=s, times=time, func=model, parms=p, method="euler", hini=timestep)
out_euler <- data.frame(out)
timeEuler = proc.time() - startTiming

#' Solve the system using Runge-Kutta 23, with dynamical time step
#' Note that we added some code at the start and end to monitor 
#' the time it takes the computer to numerically solve 
#' this equation
startTiming = proc.time()
out <- ode(y=s, times=time, func=model, parms=p, method="ode23", hini=timestep)
out_rk23 <- data.frame(out)
timeRKMethod = proc.time() - startTiming


#' You can look at what, exactly, ode23 means here: 
#' https://blogs.mathworks.com/cleve/2014/05/26/ordinary-differential-equation-solvers-ode23-and-ode45/ 
  
#' Put simply, for Runge-Kutta methods, there are 
#' different "orders" i.e. using a different number of 
#' the in class described k_i values to estimate the 
#' slope of the function in a different number of points 
#' and with different info. 
#' By combining 2 Runge-Kutta order methods, here RK2 and RK3, 
#' and comparing their results one gets an idea of the error: 
#' RK3 will be more precise than RK2 and this will matter more 
#' if precision is being an issue. The size of this error is 
#' then used to decide on the size of the integration timestep
#' at the next stage.

#' Basic R plotting of results
par(mfrow=c(1,3))
plot(time,out_euler$R, type='l', col='red', lwd=3, main = "Resource concentration (R)", ylab = "R")
lines(time,out_rk23$R, type='l', col='orange', lwd=3)
plot(time,out_euler$B, type='l', col='blue', lwd=3, main = "Number of bacteria (B)", ylab = "B")  
lines(time, out_rk23$B, type='l', col='cyan', lwd=3)
plot(time,out_euler$B, type='l', col='blue', lwd=3, log="y", main = "Number of bacteria (B) on log scale", ylab = "B (log)")  # Note: logarithmic y-axis
lines(time, out_rk23$B, type='l', col='cyan', lwd=3)

#' Fancier ploting of results ggplot --> arranged with 
#' cowplot library (see here: https://wilkelab.org/cowplot/index.html)
dataFramePlotsEulerVsRK = dplyr::bind_rows(out_euler, out_rk23) %>%
  mutate(method = c(rep("Euler", nrow(out_euler)),
                    rep("Runge-Kutta23", nrow(out_rk23)))
  )

Rplot = dataFramePlotsEulerVsRK %>%
  ggplot(aes(x = time, y = R, colour = method)) +
  geom_point() + 
  theme_bw() +
  # we set the left and right margins to 0 to remove 
  # unnecessary spacing in the final plot arrangement.
  theme(plot.margin = margin(6, 0, 6, 0))

Bplot = dataFramePlotsEulerVsRK %>%
  ggplot(aes(x = time, y = B, colour = method)) +
  geom_point() + 
  theme_bw() +
  scale_y_continuous(trans = "log",
                     breaks = c(2, 20, 200, 2000, 20000, 200000, 2000000, 20000000, 200000000, 2000000000)
  ) +
  theme(plot.margin = margin(6, 0, 6, 0))

legend = get_legend(
  # create some space to the left of the legend
  Rplot + theme(legend.box.margin = margin(0, 0, 0, 12))
)

rowPlots = cowplot::plot_grid(Rplot + theme(legend.position = "none"),
                              Bplot + theme(legend.position = "none"),
                              labels = c("A", "B"),
                              align = "vh",
                              hjust = -1,
                              nrow = 2)

plot_grid(rowPlots, legend, rel_widths = c(3, 1))

#' Run the above code for timestep=0.05
#' we learned earlier that for Euler the timestep needed 
#' to be smaller than 0.075 to get ok results, 
#' and indeed here we again observe negative R and B exponentially
#' growing. In contrast the RK23 method performs fine.
#' If you change the timestep to 0.001 and rerun for both methods
#' R goes to zero and B stabilizes yet still differences 
#' between Euler and RK23 solutions exist.

#' what about calculation time?
print(paste0("Calculation Euler for timestep: ", timestep))
print(timeEuler)
print(paste0("Calculation Runge-Kutta23 for timestep: ", timestep))
print(timeRKMethod)

#' Interestingly you see that RK23 takes more compute time 
#' than Euler. This implies that the cost of making more 
#' intermediate computations in the RK23 method is larger 
#' than the savings from sometimes being able to use a larger 
#' timestep due to the dynamic timestepping.
#' This is probably because of the so-called "stiffness" of 
#' the system requiring sufficient precision even once 
#' the equilibrium is reached
#' (recall the earlier oscillations around the equilibrium you 
#' observed for Euler forward with a timestep of 0.075)


#' Answer question f:
#' If you uncomment # state <- ifelse(state<0,0,state), 
#' then if you get negative values because of overshooting 0 
#' in the numerical integration step, you correct that mistake 
#' (by setting it to 0) before doing the next 
#' numerical integration step.

#' You will see now that both numerical integration methods 
#' produce an R going to zero and a B stabilizing, 
#' yet methods still have quantitatively somewhat 
#' different results. 