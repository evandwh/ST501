

#read in the data
arrivals <- read.csv("C:\\Users\\edwhi\\Downloads\\gamma-arrivals.csv")
arrivals_corrected <- arrivals[,1]

#a
hist(arrivals_corrected,
xlab = "Arrival times",
main = "Histogram of Arrival Times")

#b fit the parameters
mean_arrival <- mean(arrivals_corrected)
sd_arrival <- mean(arrivals_corrected^2)-(mean_arrival)^2
alpha_hat_MOM<- mean_arrival^2/sd_arrival
lambda_hat_MOM <- mean_arrival/sd_arrival
MOMs <- c(alpha_hat_MOM, lambda_hat_MOM)
MOMs

#mle fits
MLEs <- MASS::fitdistr(arrivals_corrected, densfun = "gamma")
MLEs

#c
hist(arrivals_corrected,
xlab = "Arrival times",
main = "Histogram of Arrival Times",
freq = FALSE)
curve(dgamma(x, shape = MOMs[1], rate = MOMs[2]),
col = "Red", from = 0, to = 500, add = TRUE)
curve(dgamma(x, shape = MLEs$estimate[1], rate = MLEs$estimate[2]),
col = "Blue", from = 0, to = 500, add = TRUE)
legend("topright", legend = c("MOM", "MLE"),
col = c("Red", "Blue"), lwd = 1)

#d
B <- 10000
n <- length(arrivals_corrected)
MOM_boots <- replicate(B, {
MOM_data <- rgamma(n, shape = alpha_hat_MOM, rate = lambda_hat_MOM)
mean_MOM_data <- mean(MOM_data)
sd_MOM_data <- mean(MOM_data^2)-(mean(MOM_data))^2
alpha_hat_MOM_boot <- mean_MOM_data^2/sd_MOM_data
lambda_hat_MOM_boot <- mean_MOM_data/sd_MOM_data
c(alpha_hat_MOM_boot, lambda_hat_MOM_boot)
})

B2 <- 100
MLE_boots <- replicate(B2, {
MLE_data <- rgamma(n, shape = MLEs$estimate[1], rate = MLEs$estimate[2])
MLE_boot <- MASS::fitdistr(MLE_data, "gamma")
c(MLE_boot$estimate[1], MLE_boot$estimate[2])
})

c(sd(MOM_boots[1,]), sd(MOM_boots[2,]))
c(sd(MLE_boots[1,]), sd(MLE_boots[2,]))

#e
n <- 25
sample_data_MOM <- rgamma(n, shape = alpha_hat_MOM, rate = lambda_hat_MOM)
c(mean(sample_data_MOM)-qnorm(0.975)*sd(sample_data_MOM)/sqrt(n),
  mean(sample_data_MOM)+qnorm(0.975)*sd(sample_data_MOM)/sqrt(n))

sample_data_MLE <- rgamma(n, shape = MLEs$estimate[1], rate = MLEs$estimate[2])
c(mean(sample_data_MLE)-qnorm(0.975)*sd(sample_data_MLE)/sqrt(n),
  mean(sample_data_MLE)+qnorm(0.975)*sd(sample_data_MLE)/sqrt(n))