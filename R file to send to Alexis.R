#Parametric Bootstraps
BootCIs <- function(y, n, B = 100, alpha = 0.05){
  p_hat <- y/n
  boot_estimates <- replicate(B, {
    p_hat_boot <- rbinom(1, n, p_hat)/n

    #Secondary Bootstrap --> MAYBE NOT NEEDED
    B2 <- 100
    secondary_boot <- rbinom(B2, n, p_hat_boot)/n
    estimated_SE_p_hat_boot <- sd(secondary_boot)

    #theoretical using formula sqrt(p*1-p)/n)
    theoretical_SE_p_hat_boot <- sqrt((p_hat_boot*(1-p_hat_boot)/n))
	
    #find SEs for t-stats
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

#NOT FINISHED. This is a bunch of if/elseif statements. I feel like there is a better way. With an array/vector instead of adding counts. This is my python brain thinking.
#Going to switch to 
CI_Success_Check <- function(p, n, N = 1000){
	Wald_Count <- 0
	Adj_Wald_Count <- 0
	Raw_Boot_Count <- 0
	Boot_T_Count <- 0
	Samples <- rbinom(N, n, p)/n
	for (i in Samples){
      	if(i > waldCI(n*p, n)[1] & i < waldCI(n*p, n)[2]){
			Wald_Count <- Wald_Count + 1
		}
		if(i > AdjustedWaldCI(n*p, n)[1] & i < AdjustedWaldCI(n*p, n)[2]){
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
