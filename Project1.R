#############################
# Author: Evan Whitfield
# Purpose: Project 1
# Last Editted on: 9/30/24
#############################


#create vectors for n,p, and name of the methods that we wish to use
sizes <- c(seq(15, 100, 15),100)
props <- seq(0.01, 0.99, length.out = 15)
Methods <- c("Wald Interval", "Adj Wald", "Clopper-Pearson", "Score Interval", "Raw Percentile Bootstrap", "Bootstrap T Interval")

#Define how many random samples from each binomial we want

#Part 1.

#Basic Wald Function
WaldCI <- function(y, n, alpha = 0.05){
	c(y/n - qnorm(1 - alpha/2)*sqrt(((y/n)*(1-y/n))/n),
	  y/n + qnorm(1 - alpha/2)*sqrt(((y/n)*(1-y/n))/n))
}

#Adjusted Wald Function
Adjusted_WaldCI <- function(y, n, alpha = 0.05){
	y_adj <- y + 2
	n_adj <- n + 4
	c((y_adj/n_adj) - qnorm(1 - alpha/2)*sqrt(((y_adj/n_adj)*(1-y_adj/n_adj))/n_adj),
	  y_adj/n_adj + qnorm(1 - alpha/2)*sqrt(((y_adj/n_adj)*(1-y_adj/n_adj))/n_adj))
}

#Parametric Bootstraps
BootCIs <- function(y, n, B = 100, alpha = 0.05){
  p_hat <- y/n
  boot_estimates <- replicate(B, {
    p_hat_boot <- rbinom(1, n, p_hat)/n
    #se_p_hat_boots <- sd(p_hat_boot)
    BootStrap_Interval <- quantile(p_hat_boot, c(alpha/2, 1 - alpha/2))
    B2 <- 100
    secondary_boot <- rbinom(B2, n, p_hat_boot)/n
    theoretical_SE_p_hat_boot <- sqrt((p_hat_boot*(1-p_hat_boot)/n))
    estimated_SE_p_hat_boot <- sd(secondary_boot)

	
    #find SE for t-stat
    p_boot_t <- (p_hat_boot - p_hat)/estimated_SE_p_hat_boot
    p_boot_t_theoretical <- (p_hat_boot - p_hat)/theoretical_SE_p_hat_boot

    return(c("p_hat_boot" = p_hat_boot,
	       "p_boot_t" = p_boot_t,
		 "p_boot_t_theoretical" = p_boot_t_theoretical))
  })

  return(c("Lower Bound % Interval" = quantile(boot_estimates[1, ], alpha/2, name = FALSE),
           "Upper Bound % Interval" = quantile(boot_estimates[1, ], 1 - alpha/2, name = FALSE),
           "Lower Bound T-Interval" = p_hat-quantile(boot_estimates[2, ], 1 - alpha/2, name = FALSE)*sd(boot_estimates[1, ]),
	     "Upper Bound T-Interval" = p_hat-quantile(boot_estimates[2, ], alpha/2, name = FALSE)*sd(boot_estimates[1, ]),
	     "Lower Bound T-Interval Theoretical" = p_hat-quantile(boot_estimates[3, ], 1 - alpha/2, name = FALSE)*sd(boot_estimates[1, ]),
	     "Upper Bound T-Interval Theoretical" = p_hat-quantile(boot_estimates[3, ], alpha/2, name = FALSE)*sd(boot_estimates[1, ])))
}



#Part 2 - The Random Samples

sizes <- c(seq(15, 100, 15),100)
props <- seq(0.01, 0.99, length.out = 15)
N <- 1000

#Set up a 3D array to store all samples. 
#First dimension are individual sample values.
#Second dimension is the different sample sizes
#Third dimension is the different p values.
Sample_array <- array(NA, dim = c(N, length(sizes), length(props)))

#Creating the array
for (n in 1:length(sizes)){
  for (p in 1:length(props)){
    Sample_array[,n,p] <- rbinom(N, sizes[n], props[p])/sizes[n]
  }
}


#NOT FINISHED. This is a bunch of if/elseif statements. I feel like there is a better way. With an array/vector instead of adding counts. This is my python brain thinking.
CI_Success_Check <- function(p, n, N = 1000){
	Wald_Count <- 0
	Adj_Wald_Count <- 0
	Raw_Boot_Count <- 0
	Boot_T_Count <- 0
	Samples <- rbinom(N, n, p)/n
	for (i in Samples){
      	if(i > WaldCI(n*p, n)[1] & i < WaldCI(n*p, n)[2]){
			Wald_Count <- Wald_Count + 1
		}
		if(i > Adjusted_WaldCI(n*p, n)[1] & i < Adjusted_WaldCI(n*p, n)[2]){
			Adj_Wald_Count <- Adj_Wald_Count + 1
		}
      	if(i >= BootCIs(n*p, n)[1] & i <= BootCIs(n*p, n)[2]){
			Raw_Boot_Count <- Raw_Boot_Count + 1
		}
		if(i > BootCIs(n*p, n)[3] & i < BootCIs(n*p, n)[4]){
			Boot_T_Count <- Boot_T_Count + 1
		}
	}
	return(c("Wald Success" = Wald_Count/N,
		   "Adj Wald Success" = Adj_Wald_Count/N,
		   "Raw Boot % Success" = Raw_Boot_Count/N,
		   "Boot T Int Success" = Boot_T_Count/N))
}