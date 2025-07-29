#load packages
library(tidyverse)

#install.packages("invgamma")
library(invgamma)

diabetes <- read_csv("C:/Users/edwhi/Downloads/diabetes-dataset.csv")

post <- function(Y, X, beta, sigmasq){
    beta_hat <- beta[1] + X*beta[2]
    loglike <- sum(log(dnorm(Y, beta_hat, sqrt(sigmasq))))
    logprior <- sum(log(dnorm(beta,0,15)))+log(dinvgamma(sigmasq, 1, 1))
    loglike + logprior
}

bayes_SLR <- function(y,
                      X,
                      init_beta,
                      init_sigmasq,
                      n_samples = 10000,
                      can_SD = 0.1){

	#Initial values:
	beta <- init_beta
	sigmasq <- init_sigmasq

	# Keep track of the samples
	keep_beta_sigmasq <- data.frame(
      	beta0 = vector(mode = "numeric", length = n_samples),
		beta1 = vector(mode = "numeric", length = n_samples),
		sigmasq = vector(mode = "numeric", length = n_samples)
         	)

     	keep_beta_sigmasq[1, ] <- c(beta, sigmasq)
     
     	#prop to posterior at current beta
     	cur_post <- post(y, X, beta, sigmasq)
     	for(i in 2:n_samples){
		#Update beta using MH sampling:
	   	for(j in 1:2){

		# Draw candidate:
	  	can_beta <- beta

		if(j == 1){
			can_beta[j] <- rnorm(1, beta[j], can_SD)
		} else {
			can_beta[j] <- rnorm(1, beta[j], can_SD/50)
		}
		can_post <- post(y, X, can_beta, sigmasq)
		
		# Compute acceptance ratio:
		logR <- can_post - cur_post
		logU <- log(runif(1))
		if(logU < logR){ #accept
			beta <- can_beta
			cur_post <- can_post
		}
	}
	keep_beta_sigmasq[i,1:2] <- beta

	#now update our sigma
	can_sigmasq <- rnorm(1, sigmasq, can_SD)

	#make sure it isn’t negative
	if (can_sigmasq <= 0){
		can_sigmasq <- 0.001
	}
	can_post <- post(y, X, beta, can_sigmasq)
	
	# Compute acceptance ratio:
	logR <- can_post - cur_post
	logU <- log(runif(1))
	if(logU < logR){#accept
		sigmasq <- can_sigmasq
		cur_post <- can_post
	}
	keep_beta_sigmasq[i, 3] <- sigmasq
}

# return posterior values
return(keep_beta_sigmasq)
}


fit <- bayes_SLR(y = diabetes$Outcome,
	X = diabetes$Glucose,
	init_beta=c(0,0),
	init_sigmasq = 1,
	n_samples = 15000,
	can_SD=0.125)

#look at posteriors
burn <-10000
par(mfrow = c(1, 2))
	
#for intercept
hist(fit[-1:-burn, "beta0"],
	xlab = "beta0 estimates",
	main = "Posterior for beta0")
	abline(v = mean(fit[-1:-burn, "beta0"]),
	lwd = 2,
	col = "blue")

#for slope
hist(fit[-1:-burn, "beta1"],
	xlab = "beta1 estimates",
	main = "Posterior for beta1")
	abline(v = mean(fit[-1:-burn, "beta1"]),
	lwd = 2,
	col = "blue")

#look at how they ’mix’
plot(fit[-1:-burn, "beta0"],
	xlab = "run",
	ylab = "beta0")
plot(fit[-1:-burn, "beta1"],
	xlab = "run",
	ylab = "beta1")

#report mean and sd of posteriors

#beta0
c(mean(fit[-1:-burn, "beta0"]), sd(fit[-1:-burn, "beta0"]))

#beta1
c(mean(fit[-1:-burn, "beta1"]), sd(fit[-1:-burn, "beta1"]))


#get CI for beta0
quantile(fit[-1:-burn, "beta0"],
c(0.025, 0.975))

#get CI for beta1
quantile(fit[-1:-burn, "beta1"],
c(0.025, 0.975))

