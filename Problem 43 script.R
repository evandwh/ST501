#First read in the data, but didn't want to show my filepath
#Called data, data3

fitdistr(data3, "gamma")

n <- length(data3)
B <- 10000

boot_estimates <- replicate(B,{
 sim_data <- rgamma(n, alpha_hat, lambda_hat)
 ybar <- mean(sim_data)
 sbsq <- mean(sim_data^2)-ybar^2
 alpha_hat_boot <- ybar^2/sbsq
 lambda_hat_boot <- ybar/sbsq
 return(c("alpha_hat_boot" = alpha_hat_boot, "lambda_hat_boot" = lambda_hat_boot))
 })


boot_estimates2 <- replicate(B,{
 sim_data <- rgamma(n, 1.0269950924, 0.0128596094)
 ybar <- mean(sim_data)
 sbsq <- mean(sim_data^2)-ybar^2
 alpha_hat_boot <- ybar^2/sbsq
 lambda_hat_boot <- ybar/sbsq
 return(c("alpha_hat_boot" = alpha_hat_boot, "lambda_hat_boot" = lambda_hat_boot))
 })

c(sd(boot_estimates[1, ]), sd(boot_estimates[2, ]))

c(sd(boot_estimates2[1, ]), sd(boot_estimates2[2, ]))