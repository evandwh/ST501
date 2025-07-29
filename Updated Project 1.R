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
    return(c((alpha/2)^(1/n),1))
  }
  if(y == 0){
    return(c(0,1-(alpha/2)^(1/n)))
  }
  else{
    return(c(qbeta(alpha/2,y,n-y+1),qbeta(1-alpha/2,y+1,n-y)))
  }
}

Score_Interval <- function(y,n,alpha = 0.05){
  p_hat <- y/n
  c((p_hat+((qnorm(alpha/2)^2)/(2*n))+qnorm(alpha/2)*sqrt((p_hat*(1-p_hat)+(qnorm(alpha/2)^2)/(4*n))/n))/(1+(qnorm(alpha/2)^2)/(2*n)),
    (p_hat+((qnorm(alpha/2)^2)/(2*n))-qnorm(alpha/2)*sqrt((p_hat*(1-p_hat)+(qnorm(alpha/2)^2)/(4*n))/n))/(1+(qnorm(alpha/2)^2)/(2*n))
  )
}

BootCIs <- function(y, n, B = 100, alpha = 0.05){
  if(y == 0){
	return(c(0,0,0,0))
  }
  if(y == n){
	return(c(1,1,1,1))
  }
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

N<- 100

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
WaldCIMatrix 		<- matrix(0,nrow=length(sizes)*length(props)*N,ncol=6)
AdjustedWaldMatrix 	<- matrix(0,nrow=length(sizes)*length(props)*N,ncol=6)
ScoreIntervalMatrix 	<- matrix(0,nrow=length(sizes)*length(props)*N,ncol=6)
CPMatrix 			<- matrix(0,nrow=length(sizes)*length(props)*N,ncol=6)
BSRawMatrix 		<- matrix(0,nrow=length(sizes)*length(props)*N,ncol=6)
BSTMatrix 			<- matrix(0,nrow=length(sizes)*length(props)*N,ncol=6)

row_counter <-1
for (k in 1:length(props)){
  for (j in 1:length(sizes)){
      for(i in 1:N){

      #calculate CIs
      WaldCI 		<- waldCI(sample_array[i,j,k],sizes[j],)
      AdjustedWald_CI 	<- AdjustedWaldCI(sample_array[i,j,k],sizes[j],)
	Score_CI 		<- Score_Interval(sample_array[i,j,k],sizes[j],)
	CP_CI 		<- ClopperPearson(sample_array[i,j,k],sizes[j],)
	BootStrapCIs 	<- BootCIs(sample_array[i,j,k],sizes[j],)
	BSRaw_CI 		<- c(BootStrapCIs[1], BootStrapCIs[2])
	BST_CI 		<- c(BootStrapCIs[3], BootStrapCIs[4])

	#Check CI Success Rates
        p_check <- props[k]

        if(p_check >= WaldCI[1] & p_check <= WaldCI[2]){
          Wald_Check <- "Captured"
        }
        if(p_check < WaldCI[1]){
          Wald_Check <- "Above"
        }
        if(p_check > WaldCI[2]){
          Wald_Check <- "Below"
        }
        if(p_check >= AdjustedWald_CI[1] & p_check <= AdjustedWald_CI[2]){
          Adj_Wald_Check <- "Captured"
        }
        if(p_check < AdjustedWald_CI[1]){
          Adj_Wald_Check <- "Above"
        }
        if(p_check > AdjustedWald_CI[2]){
          Adj_Wald_Check <- "Below"
        }
        if(p_check >= Score_CI[1] & p_check <= Score_CI[2]){
          Score_Check <- "Captured"
        }
        if(p_check < Score_CI[1]){
          Score_Check <- "Above"
        }
        if(p_check > Score_CI[2]){
          Score_Check <- "Below"
	  }
	  if(p_check >= CP_CI[1] & p_check <= CP_CI[2]){
          CP_Check <- "Captured"
        }
        if(p_check < CP_CI[1]){
          CP_Check <- "Above"
        }
        if(p_check > CP_CI[2]){
          CP_Check <- "Below"
        }
	  if(p_check >= BSRaw_CI[1] & p_check <= BSRaw_CI[2]){
          BSRaw_Check <- "Captured"
        }
        if(p_check < BSRaw_CI[1]){
          BSRaw_Check <- "Above"
        }
        if(p_check > BSRaw_CI[2]){
          BSRaw_Check <- "Below"
        }	  
	  if(p_check >= BST_CI[1] & p_check <= BST_CI[2]){
          BST_Check <- "Captured"
        }
        if(p_check < BST_CI[1]){
          BST_Check <- "Above"
        }
        if(p_check > BST_CI[2]){
          BST_Check <- "Below"
        }

      ###ADDING TO CORRECT MATRICIES!!!!
      WaldCIMatrix[row_counter,] <- c(props[k],sizes[j],WaldCI,WaldCI[2]-WaldCI[1],Wald_Check)
      AdjustedWaldMatrix[row_counter,] <- c(props[k],sizes[j],AdjustedWald_CI,AdjustedWald_CI[2]-AdjustedWald_CI[1],Adj_Wald_Check)
      ScoreIntervalMatrix[row_counter,] <- c(props[k],sizes[j],Score_CI,Score_CI[2]-Score_CI[1],Score_Check)
      CPMatrix[row_counter,] <- c(props[k],sizes[j],CP_CI,CP_CI[2]-CP_CI[1],CP_Check)
      BSRawMatrix[row_counter,] <- c(props[k],sizes[j],BSRaw_CI,BSRaw_CI[2]-BSRaw_CI[1],BSRaw_Check)
      BSTMatrix[row_counter,] <- c(props[k],sizes[j],BST_CI,BST_CI[2]-BST_CI[1],BST_Check)

	#Move to next row in matricies
      row_counter <-row_counter +1
    }
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
colnames(Wald_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(AdjWald_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(Score_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(CP_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(BSRaw_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(BST_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")

list_of_dfs <- list(Wald_DF, AdjWald_DF, Score_DF, CP_DF, BSRaw_DF, BST_DF)
names <- c("Wald", "Adjusted Wald", "Score", "Clopper-Pearson(Exact)", "Bootstrap Percentile", "Bootstrap T")

#Proportion Captured, Above, and Below
Props_DF <- data.frame()
for(i in 1:length(names)){
  for(k in sizes){
    for(j in props){
	new_row <- c(names[i],j,k,nrow(subset(list_of_dfs[[i]],p == j & n == k & Status == "Captured"))/N,
			  		nrow(subset(list_of_dfs[[i]], p == j & n == k & Status == "Below"))/N,
		          	       nrow(subset(list_of_dfs[[i]], p == j & n == k & Status == "Above"))/N)
	Props_DF <- rbind(Props_DF, new_row)
    }
  }
}

colnames(Props_DF) <- c("Method","Props","Sample_Size","Prop_Captured","Prop_Below","Prop_Above")

#Plot
for (k in sizes){
  windows()
  par(mfrow = c(2, 3))
  for(i in 1:length(names)){
    df_i <- subset(Props_DF, Method == names[i] & Sample_Size == k)
    plot(df_i$Props, df_i$Prop_Captured, main = paste(names[i],"n =",k), xlab = paste("p"), ylab = "Proportion Captured")
    abline(h = 0.95)
  }
}