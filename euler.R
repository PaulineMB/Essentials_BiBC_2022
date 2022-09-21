# time intervals: a sequence from zero to ten at 0.5 steps
tstep <- 0.005
maxtime <- 12
nrsteps <- (maxtime/tstep)
time <- seq(0, nrsteps, by = tstep)
R0 <- 350
B0 <- 1000
f <- function(R,B){-e*(v*R/(k+R))*B}
g <- function(R,B){(v*R/(k+R)*B)}
v <- 1.4
e <-0.0000007
k <-1
## An empty R vector to store the results
R <-c()
B <-c()
## Store the initial condition in the first position of the vector
R[1] <- R0
B[1] <- B0
# loop over time: approximate the function at each time step
for (i in 1:(length(time)-1)){
  R[i+1]=R[i]+tstep*f(R[i],B[i])
  B[i+1]=B[i]+tstep*g(R[i],B[i])
}
plot(time*tstep, R)
plot(time*tstep, B)