####################
#Author: Evan Whitfield
#Date Modified: 10/23/24
#Purpose: Problem 8.42 (Rice)
####################

data <- read_csv("C:/Users/edwhi/Downloads/gamma-ray.csv")

gammacounts  <- data$count
gammaseconds <- data$seconds
lambda_from_data <- sum(gammacounts)/sum(gammaseconds)

#Create Improper Gamma Prior - Not possible because it is improper?
gammaprior <- function(theta){
	if(theta <= 0){
		return(0)
	} else {
		return(1)
	}
}

#Poisson Distribution for likelihood (ignoring the constant wrt to theta)
likelihood <- function(theta, y){
	return(theta^y * exp(-1*theta))
}

#Create a function that takes in an observed ...
thetasampler <- function(y, niter, thetastartval, thetaproposalsd){
	
	#vector to store sampled theta's
	theta <- rep(0, niter)
	#use the starting value as the first theta
	theta[1] <- thetastartval

	#loop through the desired number of iterations
	for(i in 2:niter){
		#temporary value for vurrent theta is previous theta
		currenttheta <- theta[i-1]
		
		#get the next random draw
		newtheta <- currenttheta + rnorm(1, 0, thetaproposalsd)

		#Find r ration:
		r <- prior(theta = newtheta)*likelihood(theta = newtheta, y = y)/
		    (prior(theta = currenttheta)*likelihood(theta = currenttheta, y = y))

		#accept this new valvue with prod min(1, r)
		if(runif(1) < r){
			theta[i] <- newtheta		#accept move with probability min(1, r)
		} else {
			theta[i] <- currenttheta	#otherwise "reject" move, and stay where we are
		} #end if's
	} #end loop

	#return the vector of theta values
	return(theta)

} #end function
		
#running with data
withdata <- thetasampler(y = gammacounts,		
				 niter = 50000,
				 thetastartval = 0.1,
				 thetaproposalsd = 0.1)

#########################################
#Now some R code to compare the sample from the posterior

#sample values from posterior using MCMC
hist(withdata,
     prob = T,
     xlab = "values of theta",
     ylab = "Histogram of sampled values")


#overlays true posterior
curve(dgamma(x, shape = gammacounts, rate = 1 + lambda_from_data), add = T)