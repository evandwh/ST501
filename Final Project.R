##################################################################
# Author: Evan Whitfield
# Last Editted: 12/9/24
# Purpose: Project 2 for ST502
##################################################################

#--Part 1--#

#Set alpha for RR
alpha <- 0.05

#Calc Test Statistic
test_stat <- (15-40)^2/(15+40)
print(test_stat)

#Calculate P Value
p_value <- pchisq(test_stat, df = 1, lower.tail = FALSE)
print(p_value)

#Determine Rejection Region for given alpha
reject_region <- qchisq(alpha, df = 1, lower.tail = FALSE)
print(reject_region)

#Making a matrix of our sample data values
raw_values <- c(85, 40, 15, 110)
data <- matrix(raw_values, nrow = 2, ncol = 2)

#Running the McNemar Test
mcnemar.test(data, correct = FALSE)

####################################################################
#--Part 2--#
library(MultiRNG)
library(ggplot2)

#Set values for N and alpha, and store values we want to change in vectors
N <- 1000
alpha <- 0.05
sample_sizes <- c(25, 40, 80, 200)
drug_A_success <- c(0.1, 0.4, 0.8)
drug_B_success <- c(0, 0.02, 0.05, 0.1)
corr <- c(0, 0.2, 0.5)

#Function that draws the sample data based on the current values we are interested in
draws <- function(n, drug_success, correlation){
      corr_matrix <- matrix(c(1,correlation,correlation,1), nrow = 2, ncol = 2)
	draw.correlated.binary(no.row = n, d = 2, prop.vec = c(drug_success[1], drug_success[2]), corr.mat = corr_matrix)
}

#returns true if you should reject, false if you should not
trial <- function(data, alpha = 0.05){
  two_way_table = matrix(c(0,0,0,0), nrow = 2, ncol = 2)
  for(i in 1:nrow(data)){
    if(data[i,1] == 1 & data[i,2] == 1){
      two_way_table[1,1] <- two_way_table[1,1] + 1
    }
    else if(data[i,1] == 0 & data[i,2] == 1){
      two_way_table[1,2] <- two_way_table[1,2] + 1
    }
    else if(data[i,1] == 1 & data[i,2] == 0){
      two_way_table[2,1] <- two_way_table[2,1] + 1
    }
    else if(data[i,1] == 0 & data[i,2] == 0){
      two_way_table[2,2] <- two_way_table[2,2] + 1
    }
  }
  McNemar <- mcnemar.test(two_way_table, correct = FALSE)
  if(is.na(McNemar$p.value)){
    p_value <- 0
  }
  else {
    p_value <- McNemar$p.value
  }
  return(p_value < alpha)
}

#Create a Data Frame to store results from each simulation
results_df <- data.frame(Sample_Size = numeric(),
                        Drug_A_Success = numeric(),
                        DrugB_Minus_DrugA = numeric(),
                        Correlation = numeric(),
                        Proportion_Rejected = numeric()
                        )

#Run through all the values we are interested in and store results in a dataframe
for(m in 1:length(corr)){
  for(k in 1:length(drug_B_success)){
    for(j in 1:length(drug_A_success)){
      for(i in 1:length(sample_sizes)){
        results <- replicate(N, trial(draws(n = sample_sizes[i], 
                                      drug_success = c(drug_A_success[j], drug_A_success[j]+drug_B_success[k]),
                                      correlation = corr[m]))
                            )
        new_row <- c(sample_sizes[i],
                     drug_A_success[j],
                     drug_B_success[k],
                     corr[m],
                     sum(results)/N
                     )
        results_df <- rbind(results_df, new_row)
      }
    }
  }
}

#I realized that the names of the columns were lost when adding rows.
#We used colnames in our last project, so I figured it would be okay to use here.
colnames(results_df) <- c("n","p1","p2_minus_p1","Corr","Prop")

#Create the appropriate plots. I had to use the ggplot2 website to understand different aspects of the graph.
#Website is in the bibliography.
#Creates graphs for p1 = 0.8
ggplot(subset(results_df,p1 == 0.8), aes(x = p2_minus_p1, y = Prop, color = as.factor(Corr))) +
  geom_line() +
  facet_wrap(~n, ncol = 4) +
  geom_hline(yintercept = alpha) +
  ggtitle("Power plot for different sample sizes with p1 = 0.8") +
  labs(color = "Correlation", x = "p2-p1",y = "Proportion Rejected") +
  theme(
    axis.text.x = element_text(angle = 45)
  )

#Create a new window for the next graph if running on a windows device.
windows()

#Creates 2nd group of graphs for p1 = 0.1
ggplot(subset(results_df,p1 == 0.1), aes(x = p2_minus_p1, y = Prop, color = as.factor(Corr))) +
  geom_line() +
  facet_wrap(~n, ncol = 4) +
  geom_hline(yintercept = alpha) +
  ggtitle("Power plot for different sample sizes with p1 = 0.1") +
  labs(color = "Correlation", x = "p2-p1",y = "Proportion Rejected") +
  theme(
    axis.text.x = element_text(angle = 45)
  )

#new window on windows device
windows()

#Creates 3rd group of graphs for p1 = 0.4
ggplot(subset(results_df,p1 == 0.4), aes(x = p2_minus_p1, y = Prop, color = as.factor(Corr))) +
  geom_line() +
  facet_wrap(~n, ncol = 4) +
  geom_hline(yintercept = alpha) +
  ggtitle("Power plot for different sample sizes with p1 = 0.4") +
  labs(color = "Correlation", x = "p2-p1",y = "Proportion Rejected") +
  theme(
    axis.text.x = element_text(angle = 45)
  )


