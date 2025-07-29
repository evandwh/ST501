##################################
# Author: Evan Whitfield
# Collaborators: N/A
# Program Purpose: HW7 5.9 - Normal Approximations
# Date: 7/18/2024
##################################

#Gather random data from Binom(20,0.2)
sample_data_1a <- rbinom(50000,20, 0.2)

#Plot data from Binom(20,0.2)
CDF_1a <- ecdf(sample_data_1a)
plot(CDF_1a, main = "CDF of Binom(n = 20, p = 0.2)")

#Gather random data from Normal approximation of Binom(20,0.2)
sample_data_2a <- rnorm(50000,4,sqrt(20*0.2*0.8))

#Plot data from Normal approximation of Binom(20,0.2)
CDF_2a <- ecdf(sample_data_2a)
plot(CDF_2a, add = TRUE)

#Gather random data from Binom(40,0.5)
sample_data_1b <- rbinom(50000,40, 0.5)

#Plot data from Binom(40,0.5)
CDF_1b <- ecdf(sample_data_1b)
plot(CDF_1b, main = "CDF of Binom(n = 40, p = 0.5)")

#Gather random data from Normal approximation of Binom(40,0.5)
sample_data_2b <- rnorm(50000,20,sqrt(40*0.5*0.5))

#Plot data from Normal approximation of Binom(40,0.5)
CDF_2b <- ecdf(sample_data_2b)
plot(CDF_2b, add = TRUE)