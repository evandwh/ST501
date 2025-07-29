
n <- 10
B <- 10000

boot_estimates <- replicate(B,{
 sim_data <- rdexp(n, scale = 3.3)
 ybar <- mean(sim_data)
 return("ybar" = ybar)
 })

sd(boot_estimates)

hist(boot_estimates)