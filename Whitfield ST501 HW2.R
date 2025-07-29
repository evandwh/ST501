#Homework 2

#1.17
curve((1-x/100)^4, from=0, to=100, xlab = "% defective in lot", ylab = "Probability of Lot Accepted")

#Other Problem 2
a <- runif(3, min=0, max = 3)
b <- colSums(a)
sum(b < 2)/10000

#Other Problem 3
Results3 <- replicate(100000, {
rolls <- sample(1:6, size = 5, replace=TRUE)

if(length(unique(rolls)) == 2){ 
  sorted_rolls <- sort(rolls)
  if ((sorted_rolls[1] == sorted_rolls[2]) & (sorted_rolls[4] == sorted_rolls[5])) {
    TRUE
  } else {
    FALSE
  }
} else {
  FALSE
}
})

mean(Results3)
