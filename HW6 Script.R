##################################
# Author: Evan Whitfield
# Collaborators: N/A
# Program Purpose: HW6 Other Problem #1 - Weibull Estimation
# Date: 7/3/2024
##################################

#Randomly generate numbers from U(0,1)
N <- 50000
u <- runif(n = N, min = 0, max = 1)

#Set parameters for Weibull
a <- 1
b <- 4

#Inverse of the Weibull CDF
X <- a*(-log(1-u))^(1/b)

#Plot estimated PDF and CDF
hist(X, breaks = seq(from = 0, to = 2.5, by = 0.25), freq=FALSE, main = "Emperical distribution of X")
plot(ecdf(X), ylab = "CDF estimate", xlab = "x", main = "CDF estimate for Weibull Variable")

#Estimate probability and compare to actual probability
(sum(X< 0.8) - sum(X < 0.2))/N
pweibull(0.8, shape = b, scale = a) - pweibull(0.2, shape = b, scale = a)

#Estimate Q1, Med, and Q3 and compare to actual Q1, Med, and Q3
round(quantile(X), 3)
round(c(qweibull(0.25, b, a), qweibull(0.5, b, a), qweibull(0.75, b, a)),3)

