##################################
# Author: Evan Whitfield
# Collaborators: N/A
# Program Purpose: HW8
# Date: 7/25/2024
##################################


#Part B: Set up n, N, and epsilon. Prepare for saving minimums.
#I decided to store minimums from the different sample sizes in a matrix. 
#The row number is the sample size. The column numbner is the sample number.
#For example, minimum_matrix[6,248] would be the minimum of the 248th sample taken of a sample size n=6.

max_n = 50
N = 1000
eps = 0.05
minimum_matrix <- matrix( , nrow = max_n, ncol = N)

#Create 1000 samples with n=1, store minimums
minimums <- c()
samples <- replicate(N, rexp(1, 1))
for(j in 1:length(samples)){
	minimums = append(minimums, min(samples[j]))
}
minimum_matrix[1,] <- minimums


#Part D: Going ahead and replicating the process for all sample sizes.
#Create 1000 samples of all the other sample sizes, store minimums
for(i in 2:max_n){
	minimums <- c()
	samples <- replicate(N, rexp(i, 1))
	for(j in 1:N){
		minimums = append(minimums, min(samples[,j]))
	}
	minimum_matrix[i,] <- minimums
}

#Part C: Calculating the probabilies all at once
#Calculates the number of minimums that are less than epsilon, then divides by N to approx probability.
probs <- c()
for(i in 1:max_n){
	probs[i] <- sum(minimum_matrix[i,] < eps)/N
}

#Part E: Plot of Probability vs. Sample size
plot(x = c(1: max_n), y = probs, xlab = "Sample Size", ylab = "Probability", main = "Probability Minimum is Less Than 0.05")

#Part F: The plot from Part E shows that as the sample size increases, there is a larger and larger probability
#	   that the minimum is less than 0.05. As n increases without bound, the probability of the minimum being less
#	   than 0.05 is going towards 1.

#Part G: Needed to create a list of all sample minimums to graph on same plot.
x1 <- rep(c(1:50),each = 1000)
y1 <- c()
for(i in 1:50){
	y1 <- append(y1, minimum_matrix[i,])
}
plot(x1,y1, xlab = "Sample Size", ylab = "Sample Minimum", main = "Minimum vs Sample Size for 50000 Samples")

#Part H: As seen in the graph, as the sample size increases, the sample minimums are centered closer and closer to 0.
