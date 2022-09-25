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


#' Fancier ggplot plotting
#' Again plotting both obtained x values and numerical solution 
#' over time and add legends
dataFramePlot <- data.frame(x_value = c(x, 0.1 * exp(time)/(1+0.1*(exp(time)-1.))), time = time, solution_method = c(rep("Approximation", length(x)), rep("Analytical", length(x))))

ggplot2::ggplot(data = dataFramePlot, aes (x = time, y = x_value, colour = solution_method)) + geom_point(data = dataFramePlot %>% subset(solution_method == "Approximation"), size = 2) + geom_line(data = dataFramePlot %>% subset(solution_method != "Approximation"), size = 1.2) + theme_bw() + ggtitle(paste0("Approximate versus analytical solution of f(x) = x*(1-x) for time step of ", timestep, "")) + theme(plot.title = element_text(hjust = 0.5))
