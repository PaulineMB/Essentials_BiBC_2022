#' Basic Parameter fitting Ram et al. Paper

  
#loading required packages and sourcing Grind
if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(tidyverse, rootSolve, deSolve, FME)
source("grind.R")


#' Question 2
#' Before we start to fit, let us have a look at the data

#' The 3 experiments are indexed by their date:
exptA <- "2015-11-18"  #Experiment A
exptB <- "2015-12-14"  #Experiment B
exptC <- "2016-01-06"  #Experiment C

#' Q2A
#' Let us read in data of Figure 3 for experiment A
fig3RA <- read.csv(paste("Fig3/",exptA,"_R.csv",sep="")) # Red strain
fig3GA <- read.csv(paste("Fig3/",exptA,"_G.csv",sep="")) # Green strain
plot(fig3RA$Time, fig3RA$OD, ylim=c(0,0.8),col="red",pch=".",xlab="Time (hr)",
     ylab="OD", main = "Figure 3 A1 (red) and A2 (green)")
points(fig3GA$Time, fig3GA$OD, ylim=c(0,0.8),col="green",pch=".")

fig4 <- read.csv(paste("Fig4/",exptA,"_RG.csv",sep=""))
plot(fig4$Time, fig4$OD, ylim=c(0,0.8), col="blue", pch=".", xlab="Time (hr)", 
     ylab="Total OD", main = "Figure 4 A")

fig5 <- read.csv(paste("Fig5/flow_df_",exptA,".csv",sep=""))
fig5G <- fig5[fig5$Strain=="Green",]
fig5R <- fig5[fig5$Strain=="Red",]
plot(fig5G$time, fig5G$freq_mean, ylim=c(0,1), col="green",xlab="Time (hr)", 
     ylab="Frequency", main =  "Figure 5 A")
points(fig5R$time, fig5R$freq_mean, col="red")


#' Q2B
#' Now to fit we first need to define the model

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    a <- q0/(q0+exp(-m*t))
    dN <- r*a*N*(1-(N/K)^v)
    return(list(dN))  
  }) 
}

s <- c(N=0.124)
p <- c(K=0.6,r=0.4,m=2,q0=0.005,v=2)



#' To fit, we need to do two more things:
#' 1.Tell Grind which parameters we would like to fit to the data. 
#'   All other parameters will be kept constant, at a previously defined value.
#' 2.Tell Grind what the lower and/or upper bounds are for certain parameters. 
#'   E.g. we don't want negative growth speeds, so r would need a lower bound of 0.
#'   Similarly, Ram et al., specify that v should be equal or larger than 1, 
#'   so that would require a lower bound of 1. If nothing is specified the 
#'   parameter fit can take on any value.


#' N + all the parameters are free to be fitted. 
free <- c("N",names(p))
#' For the lower bound, we bound everything at 0, and v at 1.
#' So you first use the length of the vector free to first set 
#' lower bound for all parameters to zero
#' Next for parameter v specifically you set the lower bound to 1
lower <- rep(0, length(free)); lower[match("v",free)] <- 1; lower


#' To fit, Grind wants a data frame with time and the variables it needs to fit. 
#' Note how we rename OD to N to be consistent.
data3RA <- as.data.frame(cbind(fig3RA$Time,fig3RA$OD)); names(data3RA) <- c("time","N")

#' Now we fit the parameters. fun = log tells the fitting that we want to 
#' fit the log of the data, not the actual data. The other parameters are 
#' self-explanatory. Note that we also start from some initial guess for 
#' the parameter values and try to improve the fit from there. 
#' For now, we'll just use the parameters from above.
fit3RAlog  <- fit(data3RA, state = s, odes = model, parms = p,
                  free=free, fun=log, lower=lower, pch=".",
                  legend=FALSE, tstep=0.1, main="Fig 3 a red monoculture (fun=log)",
                  ymin = 0, ymax = 1
)
#' In this fit, you can get at the fitted parameters using $par
print(fit3RAlog$par)


#' Q2C&D
#' Now look at fitting statistics
summary(fit3RAlog)


#' Q2E
#' Here you can write code to do the same as above but now for the red strain in Fig 3B
#' Make sure to look at both parameter values and fit statistics


#' The below loads in all the data from Fig3 of the Ram paper
#' (both their data and their fits). This data is in a list. 
#' You can look at sections of this list using $ or [[]]. 
#' This comes in handy when wanting to fit the model to multiple data sets at once.
#' As for the plots of the standard fits, note that unfortunately Grind's standard 
#' time plot uses a specific colour palette, one for each ODE to plot, 
#' and since we fit green and red separately it is not easily possible 
#' to make the colours green for the green bacteria.

source("HelperFunctionsAnswerFile_2022.R")
dataAndFitsFigureThree = readInDataFigureThreeRamPaper()

#' you can access the data using either $ or [[]]:
head(dataAndFitsFigureThree$DataexptAFig3G)
head(dataAndFitsFigureThree[["DataexptAFig3R"]])


#' Question 3

#' Q3A 


#' Q3B
#' To do this, we need to tell the fit function of
#'  -What parameters to fit for all data together (free parameters)
#'  -What parameters to fit separately per dataset (different parameters).
#'  -Which data sets to use for fitting


#set up initial state, and initial guess for parameter values
s <- c(N=0.124)
p <- c(K=0.6,r=0.4,m=2,q0=0.005,v=2)
#all parameters are fitted
free <- c("N",names(p)) 
#define the parameters for which different values should be obtained from different data sets, 
#for all other parameters a single fit value is obtained from the combined data sets
differ <- c("q0", "m")
#define the list of data sets to be fitted to
l <- list(dataAndFitsFigureThree$DataexptAFig3G, dataAndFitsFigureThree$DataexptAFig3R)

#' As before we need to set the lower bound of v to 1. 
#' But now, we are fitting 2*the parameters that differ per dataset, 
#' + all those that don't for all data together. So we need some code 
#' to make sure that we set the right parameter to have a lower bound of 1. 
#' To do that, we use the following:
  
#the total number of parameters to fit = 2 * those in differ (as these need to be fitted twice, once
#for each data set) + all remaining parameters in free that should be fitted combined for the two datasets
totfree <- c(free[!(free %in% differ)], differ, differ)
npar <- length(totfree)
cat("Number of free parameters:",npar)

#' Now set all instances of v in the total number of parameters to have a lower bound of 1, 
#' and all the other lower bounds to 0
lower <- rep(0,npar); lower[which(totfree == "v")] <- 1; lower  # set lower bounds

#' Now do the combined fitting

fitq0mdiffer <- fit(data=l, free=free, differ=differ, fun=log, odes = model, 
                    state = s, parms = p, lower=lower, pch=".",legend=FALSE, tstep=0.1,  
                    main="Red from A and B, q0 and m differing", add=TRUE, ymin = 0, ymax = 1)
summary(fitq0mdiffer)
fitq0mdiffer$ssr

#' Q3C


#' Question 4

#' Q4A: define the 2D model here


#' Q4B: Define which parameters are free to be fitted to the data of Fig4

#' Now let us read in the parameters as Ram et al. fitted them 
#' to figure 3A for the red and green strain
#' and let us construct a parameter vector for the 2D model from this

#' Take the parameters from the single fits (we now use the fitted values 
#' obtained in the paper). Parameters are renamed, appending indices 1 and 2, 
#' respectively for the red (R) and green (G) strains.
pR        <- dataAndFitsFigureThree$FitexptAFig3R$par[2:length(dataAndFitsFigureThree$FitexptAFig3R$par)]
names(pR) <- paste(names(pR),"1",sep="")
pG        <- dataAndFitsFigureThree$FitexptAFig3G$par[2:length(dataAndFitsFigureThree$FitexptAFig3G$par)]
names(pG) <- paste(names(pG),"2",sep="")
#' Total parameter set to fit figure 4 is then pR+pG plus the competition parameters
p4        <- c(pR,pG,c1=1,c2=1)

#' Next construct initial conditions for N1 and N2 from the optical density data 
#' representing N1+N2


#' Determine the initial conditions from the data: optical density at start
#' Assume initially strains have equal frequency assigning half of OD to each.
fig4Data  <- readr::read_csv("Fig4/2015-11-18_RG.csv")
data4Fit  <- dplyr::bind_cols(fig4Data$Time, fig4Data$OD) %>% dplyr::rename(time = ...1, OD = ...2) %>% as.data.frame()
initialOD <- data4Fit[1,2]
s4        <- c(N1=initialOD/2,N2=initialOD/2);s4


#' Q4C: Write code to do the actual fitting




#' Q4D: redo the fitting for a 2D model with a single c parameter


