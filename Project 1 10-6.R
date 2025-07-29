#Part 1 Create various functions
waldCI <- function(y, n, alpha = 0.05){
  p_hat <- y/n
  c(p_hat - qnorm(1-alpha/2)*sqrt(((p_hat)*(1-p_hat))/n),
    p_hat + qnorm(1-alpha/2)*sqrt(((p_hat)*(1-p_hat))/n))
}

AdjustedWaldCI <-function(y, n, alpha = 0.05){
  p_hat <-(y+2)/(n+4)
  c(p_hat - qnorm(1-alpha/2)*sqrt(((p_hat)*(1-p_hat))/(n+4)),
     p_hat + qnorm(1-alpha/2)*sqrt(((p_hat)*(1-p_hat))/(n+4)))
}


ClopperPearson <- function(y,n,alpha=0.05){
  if(y == n){
    return(c(1,1))
  }
  if(y == 0){
    return(c(0,0))
  }
  else{
    a1 <- 2*y
    b1 <- 2*(n-y+1)
    a2 <- 2*(y+1)
    b2 <- 2*(n-y)
    return(c((1+((n-y+1)/(y*qf(alpha/2,a1,b1))))^(-1),
    (1+((n-y)/((y+1)*qf(1-alpha/2,a2,b2))))^(-1)))
  }
}
ClopperPearson(5,15)

Score_Interval <- function(y,n,alpha = 0.05){
  p_hat <- y/n
  c((p_hat+((qnorm(alpha/2)^2)/(2*n))+qnorm(alpha/2)*sqrt((p_hat*(1-p_hat)+(qnorm(alpha/2)^2)/(4*n))/n))/(1+(qnorm(alpha/2)^2)/(2*n)),
    (p_hat+((qnorm(alpha/2)^2)/(2*n))-qnorm(alpha/2)*sqrt((p_hat*(1-p_hat)+(qnorm(alpha/2)^2)/(4*n))/n))/(1+(qnorm(alpha/2)^2)/(2*n))
  )
}

BootCIs <- function(y, n, B = 100, alpha = 0.05){
  p_hat <- y/n
  p_hat_boot_values <- c()
  p_boot_t_values <- c()
  for(i in 1:B) {
    p_hat_boot <- rbinom(1, n, p_hat)/n

    #theoretical using formula sqrt(p*1-p)/n)
    theoretical_SE_p_hat_boot <- sqrt((p_hat_boot*(1-p_hat_boot)/n))
    
    #find SEs for t-stats
    p_boot_t <- (p_hat_boot - p_hat)/theoretical_SE_p_hat_boot
    
    #p_boot_t_fixed <- setdiff(p_boot_t_theoretical, c(Inf, -Inf))
    
    p_hat_boot_values <- append(p_hat_boot_values, p_hat_boot)
    p_boot_t_values <- append(p_boot_t_values, p_boot_t)
  }
  p_boot_t_values <- setdiff(p_boot_t_values, c(Inf, -Inf))
  return(c("Lower Bound % Interval" = quantile(p_hat_boot_values, alpha/2, name= FALSE),
           "Upper Bound % Interval" = quantile(p_hat_boot_values, 1 - alpha/2, name = FALSE),
           "Lower Bound T-Interval" = p_hat-quantile(p_boot_t_values, 1 - alpha/2, name = FALSE)*sd(p_hat_boot_values),
           "Upper Bound T-Interval" = p_hat-quantile(p_boot_t_values, alpha/2, name = FALSE)*sd(p_hat_boot_values)))
}

#Part 2 Generate random samples
#create vectors for n,p, and name of the methods that we wish to use
N<- 1000
sizes <- c(seq(15, 100, 15),100)
props <- seq(0.01, 0.99, length.out = 15)

#Set up a 3D array to store all samples.
#First dimension are individual sample values.
#Second dimension is the different sample sizes
#Third dimension is the different p values.
sample_array <- array(c(0,0,0), dim = c(N, length(sizes), length(props)))
#Creating the array
for (n in 1:length(sizes)){
  for (p in 1:length(props)){
    sample_array[,n,p] <- rbinom(N, sizes[n], props[p])
  }}

#Part 3/Part 4 Creating the CIs for each function and creating calculations 

#Create Matricies to store results for each interval
WaldCIMatrix 		<- matrix(0,nrow=105,ncol=8)
AdjustedWaldMatrix 	<- matrix(0,nrow=105,ncol=8)
ScoreIntervalMatrix 	<- matrix(0,nrow=105,ncol=8)
CPMatrix 			<- matrix(0,nrow=105,ncol=8)
BSRawMatrix 		<- matrix(0,nrow=105,ncol=8)
BSTMatrix 			<- matrix(0,nrow=105,ncol=8)

row_counter <-1
for (k in 1:length(props)){
  for (j in 1:length(sizes)){

      ##Set Up Counting algorithm!!!
	Wald_Count 		<- Wald_Below 	<- Wald_Above 	<- 0
	Adj_Wald_Count 	<- Adj_Wald_Below <- Adj_Wald_Above <- 0
	Score_Count 	<- Score_Below 	<- Score_Above 	<- 0 
	CP_Count 		<- CP_Below 	<- CP_Above 	<- 0
	BSRaw_Count 	<- BSRaw_Below 	<- BSRaw_Above 	<- 0
	BST_Count 		<- BST_Below 	<- BST_Above 	<- 0
 
      #calculate CIs
      WaldCI 		<- waldCI(props[k]*sizes[j],sizes[j],)
      AdjustedWald_CI 	<- AdjustedWaldCI(props[k]*sizes[j],sizes[j],)
	Score_CI 		<- Score_Interval(props[k]*sizes[j],sizes[j],)
	CP_CI 		<- ClopperPearson(props[k]*sizes[j],sizes[j],)
	BootStrapCIs 	<- BootCIs(props[k]*sizes[j],sizes[j],)
	BSRaw_CI 		<- c(BootStrapCIs[1], BootStrapCIs[2])
	BST_CI 		<- c(BootStrapCIs[3], BootStrapCIs[4])

	#Check CI Success Rates
      for(i in sample_array[,j,k]){
        p_check <- i/sizes[j]
        if(p_check >= WaldCI[1] & p_check <= WaldCI[2]){
          Wald_Count <- Wald_Count + 1
        }
        if(p_check < WaldCI[1]){
          Wald_Below <- Wald_Below + 1
        }
        if(p_check > WaldCI[2]){
          Wald_Above <- Wald_Above + 1
        }
        if(p_check >= AdjustedWald_CI[1] & p_check <= AdjustedWald_CI[2]){
          Adj_Wald_Count <- Adj_Wald_Count + 1
        }
        if(p_check < AdjustedWald_CI[1]){
          Adj_Wald_Below <- Adj_Wald_Below + 1
        }
        if(p_check > AdjustedWald_CI[2]){
          Adj_Wald_Above <- Adj_Wald_Above + 1
        }
        if(p_check >= Score_CI[1] & p_check <= Score_CI[2]){
          Score_Count <- Score_Count + 1
        }
        if(p_check < Score_CI[1]){
          Score_Below <- Score_Below + 1
        }
        if(p_check > Score_CI[2]){
          Score_Above <- Score_Above + 1
	  }
	  if(p_check >= CP_CI[1] & p_check <= CP_CI[2]){
          CP_Count <- CP_Count + 1
        }
        if(p_check < CP_CI[1]){
          CP_Below <- CP_Below + 1
        }
        if(p_check > CP_CI[2]){
          CP_Above <- CP_Above + 1
        }
	  if(p_check >= BSRaw_CI[1] & p_check <= BSRaw_CI[2]){
          BSRaw_Count <- BSRaw_Count + 1
        }
        if(p_check < BSRaw_CI[1]){
          BSRaw_Below <- BSRaw_Below + 1
        }
        if(p_check > BSRaw_CI[2]){
          BSRaw_Above <- BSRaw_Above + 1
        }	  
	  if(p_check >= BST_CI[1] & p_check <= BST_CI[2]){
          BST_Count <- BST_Count + 1
        }
        if(p_check < BST_CI[1]){
          BST_Below <- BST_Below + 1
        }
        if(p_check > BST_CI[2]){
          BST_Above <- BST_Above + 1
        }
	
	#Turning Counts into proportions
      Wald_True_Value 	<- Wald_Count/N
      Wald_Below_Value 	<- Wald_Below/N
      Wald_Above_Value 	<- Wald_Above/N
      Adj_Wald_True_Value 	<- Adj_Wald_Count/N
      Adj_Wald_Below_Value 	<- Adj_Wald_Below/N
      Adj_Wald_Above_Value 	<- Adj_Wald_Above/N
	Score_True_Value 	<- Score_Count/N
      Score_Below_Value <- Score_Below/N
      Score_Above_Value <- Score_Above/N
	CP_True_Value 	<- CP_Count/N
      CP_Below_Value 	<- CP_Below/N
      CP_Above_Value 	<- CP_Above/N
	BSRaw_True_Value 	<- BSRaw_Count/N
      BSRaw_Below_Value <- BSRaw_Below/N
      BSRaw_Above_Value <- BSRaw_Above/N
	BST_True_Value 	<- BST_Count/N
      BST_Below_Value 	<- BST_Below/N
      BST_Above_Value 	<- BST_Above/N
      }


	###ADDING TO CORRECT MATRICIES!!!!
      WaldCIMatrix[row_counter,] <- c(props[k],sizes[j],WaldCI,WaldCI[2]-WaldCI[1],Wald_True_Value, Wald_Below_Value, Wald_Above_Value)
      AdjustedWaldMatrix[row_counter,] <- c(props[k],sizes[j],AdjustedWald_CI,AdjustedWald_CI[2]-AdjustedWald_CI[1],Adj_Wald_True_Value, Adj_Wald_Below_Value, Adj_Wald_Above_Value)
      ScoreIntervalMatrix[row_counter,] <- c(props[k],sizes[j],Score_CI,Score_CI[2]-Score_CI[1],Score_True_Value, Score_Below_Value, Score_Above_Value)
      CPMatrix[row_counter,] <- c(props[k],sizes[j],CP_CI,CP_CI[2]-CP_CI[1],CP_True_Value, CP_Below_Value, CP_Above_Value)
      BSRawMatrix[row_counter,] <- c(props[k],sizes[j],BSRaw_CI,BSRaw_CI[2]-BSRaw_CI[1],BSRaw_True_Value, BSRaw_Below_Value, BSRaw_Above_Value)
      BSTMatrix[row_counter,] <- c(props[k],sizes[j],BST_CI,BST_CI[2]-BST_CI[1],BST_True_Value, BST_Below_Value, BST_Above_Value)

	#Move to next row in matricies
      row_counter <-row_counter +1
    }
}

#Change Matricies to DFs for easy subsetting, etc.
Wald_DF <- as.data.frame(WaldCIMatrix)
AdjWald_DF <- as.data.frame(AdjustedWaldMatrix)
Score_DF <- as.data.frame(ScoreIntervalMatrix)
CP_DF <- as.data.frame(CPMatrix)
BSRaw_DF <- as.data.frame(BSRawMatrix)
BST_DF <- as.data.frame(BSTMatrix)


#Change DF Column names to make it understandable
colnames(Wald_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Prop Capt", "Prop Below", "Prop Above")
colnames(AdjWald_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Prop Capt", "Prop Below", "Prop Above")
colnames(Score_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Prop Capt", "Prop Below", "Prop Above")
colnames(CP_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Prop Capt", "Prop Below", "Prop Above")
colnames(BSRaw_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Prop Capt", "Prop Below", "Prop Above")
colnames(BST_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Prop Capt", "Prop Below", "Prop Above")


list_of_dfs <- list(Wald_DF, AdjWald_DF, Score_DF, CP_DF, BSRaw_DF, BST_DF)
names <- c("Wald", "Adjusted Wald", "Score", "Clopper-Pearson(Exact)", "Bootstrap Percentile", "Bootstrap T")


#Plot

for (k in sizes){
  windows()
  par(mfrow = c(2, 3))
  for(i in 1:6){
    df_i <- subset(list_of_dfs[[i]], n == k)
    plot(df_i$p, df_i$"Prop Capt", main = paste(names[i],"n =",k), xlab = paste("p"), ylab = "Proportion Captured")
    abline(h = 0.95)
  }
}

#par(mfrow = c(2, 3))
hist(Wald_DF$Range)
hist(AdjWald_DF$Range)
hist(Score_DF$Range)
hist(CP_DF$Range)
hist(BSRaw_DF$Range)
hist(BST_DF$Range)



plot(Wald_n_100$p, Wald_n_100$"Prop Capt", pch = 11)
points(AdjWald_DF$n, AdjWald_DF$"Prop Capt", col='red', pch=19)
plot(Score_DF$n, Score_DF$"Prop Capt", ylim = c(0,1))
plot(CP_DF$n, CP_DF$"Prop Capt", ylim = c(0,1))
plot(BST_DF$n, BST_DF$"Prop Capt", ylim = c(0,1))
plot(BSRaw_DF$n, BSRaw_DF$"Prop Capt", ylim = c(0,1))

windows()
par(mfrow = c(2, 3))
plot(Wald_DF$p, Wald_DF$"Prop Capt", ylim = c(0,1))
plot(AdjWald_DF$p, AdjWald_DF$"Prop Capt", ylim = c(0,1))
plot(Score_DF$p, Score_DF$"Prop Capt", ylim = c(0,1))
plot(CP_DF$p, CP_DF$"Prop Capt", ylim = c(0,1))
plot(BST_DF$p, BST_DF$"Prop Capt", ylim = c(0,1))
plot(BSRaw_DF$p, BSRaw_DF$"Prop Capt", ylim = c(0,1))
