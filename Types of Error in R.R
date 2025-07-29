library(ggplot2)

mu <- 5    # Mean
sigma <- 2 # Standard deviation
alpha <- 0.05
Title <- paste("Right-Sided Z-Test with Alpha =", as.character(alpha))
 

knull_x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 1000)
knull_y <- dnorm(knull_x, mean = mu, sd = sigma)
knull <- data.frame(knull_x, knull_y)

alt_mu <- mu + 1
alt_x <- seq(mu + 1 - 4*sigma, mu +1 + 4*sigma, length.out = 1000)
alt_y <- dnorm(alt_x, mean = alt_mu, sd = sigma)
alternate <- data.frame(alt_x, alt_y)

cutoff <- qnorm(1-alpha, mean = mu, sd = sigma)
cutoff_rounded <- round(cutoff, digits = 3)

ggplot() +
  geom_line(data = knull, aes(x = knull_x, y = knull_y, color = "red")) +
  geom_line(data = alternate, aes(x = alt_x, y = alt_y, color = "green")) + 
  geom_area(data = subset(knull, knull_x > cutoff), aes(x = knull_x, y = knull_y), fill = "blue", alpha = 0.5) +
  geom_area(data = subset(alternate, alt_x < cutoff), aes(x = alt_x, y = alt_y), fill = "red", alpha = 0.5) + 
  geom_vline(xintercept = cutoff, linetype = "dashed", color = "red") +
  labs(title = Title,
	 subtitle = paste("The z_cutoff is", as.character(cutoff_rounded)),
       x = "Z",
       y = "Density") +
  theme_minimal()