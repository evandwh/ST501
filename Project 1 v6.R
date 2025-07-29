######################################
#
#Authors: Evan Whitfield, Alexis Kolecki
#Purpose: To Investigate different types of confidence intervals for p.
#Last Edited: 10-07-24
#
######################################

######################################
#Part 1 Create functions that return the various confidence intervals
######################################

#Function to create the Wald Confidence Interval
waldCI <- function(y, n, alpha = 0.05){
  p_hat <- y/n
  c(p_hat - qnorm(1-alpha/2)*sqrt(((p_hat)*(1-p_hat))/n),
    p_hat + qnorm(1-alpha/2)*sqrt(((p_hat)*(1-p_hat))/n))
}

#Function to create the Adjusted Wald Confidence Interval
AdjustedWaldCI <-function(y, n, alpha = 0.05){
  p_hat <-(y+2)/(n+4)
  c(p_hat - qnorm(1-alpha/2)*sqrt(((p_hat)*(1-p_hat))/(n+4)),
     p_hat + qnorm(1-alpha/2)*sqrt(((p_hat)*(1-p_hat))/(n+4)))
}

#Function to create the Clopper-Pearson (exact) Confidence Interval
#Special cases created for y = n and y = 0.
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

#Function to return the Score Confidence Interval
Score_Interval <- function(y,n,alpha = 0.05){
  p_hat <- y/n
  c((p_hat+((qnorm(alpha/2)^2)/(2*n))+qnorm(alpha/2)*sqrt((p_hat*(1-p_hat)+(qnorm(alpha/2)^2)/(4*n))/n))/(1+(qnorm(alpha/2)^2)/(2*n)),
    (p_hat+((qnorm(alpha/2)^2)/(2*n))-qnorm(alpha/2)*sqrt((p_hat*(1-p_hat)+(qnorm(alpha/2)^2)/(4*n))/n))/(1+(qnorm(alpha/2)^2)/(2*n))
  )
}

#Function that creates both the Bootstrap Raw Percentile Interval and the Bootstrap T-Interval
#Special cases created for y = n and y = 0.
BootCIs <- function(y, n, B = 100, alpha = 0.05){
  if(y == 0){
	return(c(0,0,0,0))
  }
  if(y == n){
	return(c(1,1,1,1))
  }
  p_hat <- y/n
  
  #Vectors initiated for the p_hat_boot values and p_boot_t values needed to determine intervals.
  p_hat_boot_values <- c()
  p_boot_t_values <- c()
  for(i in 1:B) {
    p_hat_boot <- rbinom(1, n, p_hat)/n

    #theoretical formula sqrt((p*1-p)/n)
    theoretical_SE_p_hat_boot <- sqrt((p_hat_boot*(1-p_hat_boot)/n))
    p_boot_t <- (p_hat_boot - p_hat)/theoretical_SE_p_hat_boot
        
    p_hat_boot_values <- append(p_hat_boot_values, p_hat_boot)
    p_boot_t_values <- append(p_boot_t_values, p_boot_t)
  }
  p_boot_t_values <- setdiff(p_boot_t_values, c(Inf, -Inf))
  return(c("Lower Bound % Interval" = quantile(p_hat_boot_values, alpha/2, name= FALSE),
           "Upper Bound % Interval" = quantile(p_hat_boot_values, 1 - alpha/2, name = FALSE),
           "Lower Bound T-Interval" = p_hat-quantile(p_boot_t_values, 1 - alpha/2, name = FALSE)*sd(p_hat_boot_values),
           "Upper Bound T-Interval" = p_hat-quantile(p_boot_t_values, alpha/2, name = FALSE)*sd(p_hat_boot_values)))
}

####################################
#Part 2 Generate random samples
####################################

#N is number of samples we want for each value of n and p, which are stored in vectors
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

#####################################
#Part 3 - Creating the CIs for each function and creating calculations
#####################################

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

	#Check if each CI captures or misses the true p.
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

      #Sorting the results and adding to the correct matricies
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

#Change Matricies to dataframes for easy subsetting, etc.
Wald_DF <- as.data.frame(WaldCIMatrix)
AdjWald_DF <- as.data.frame(AdjustedWaldMatrix)
Score_DF <- as.data.frame(ScoreIntervalMatrix)
CP_DF <- as.data.frame(CPMatrix)
BSRaw_DF <- as.data.frame(BSRawMatrix)
BST_DF <- as.data.frame(BSTMatrix)

#Change dataframe column names to make it understandable
colnames(Wald_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(AdjWald_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(Score_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(CP_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(BSRaw_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")
colnames(BST_DF) <- c("p", "n", "L. Bound", "U. Bound", "Range", "Status")

#Creating a list of all the dataframes for easy indexing.
list_of_dfs <- list(Wald_DF, AdjWald_DF, Score_DF, CP_DF, BSRaw_DF, BST_DF)

#List of confidence interval names, so that graphs can be correctly named in the for loop for the graphs.
names <- c("Wald", "Adjusted Wald", "Score", "Clopper-Pearson(Exact)", "Bootstrap Percentile", "Bootstrap T")

##########################################################
#Part 4 - Calculating the success and miss rates of the different conference intervals. 
#Range had already been calculated for each confidence interval above.
##########################################################

#Dataframe for analyzing the Standard Error of the Error of each interval
ranges_DF <- data.frame();
for (i in 1:length(names)) {
  for (k in sizes) {
    for (j in props) {
	current_subset <- subset(list_of_dfs[[i]], p == j & n == k)
      SE_range <- sd(current_subset$Range)
      new_row <- data.frame(        
        Method = names[i],
        Props = j,
        Sample_Size = k,
        SE_of_Range = SE_range
      )
      ranges_DF <- rbind(ranges_DF, new_row)
    }
  }
}


#Proportion Captured, Above, and Below
Props_DF <- data.frame()
for (i in 1:length(names)) {
  for (k in sizes) {
    for (j in props) {
      # Subset the current DataFrame for the specific p and n values
      current_df <- subset(list_of_dfs[[i]], p == j & n == k)
      
      # Calculate proportions
      captured_count <- nrow(current_df[current_df$Status == "Captured", ])
      below_count <- nrow(current_df[current_df$Status == "Below", ])
      above_count <- nrow(current_df[current_df$Status == "Above", ])
      
      # Extract the Range value (assuming there's one unique range for each combination)
      range_value <- ifelse(nrow(current_df) > 0, unique(current_df$`Range`), NA)
      
      new_row <- data.frame(
        Method = names[i],
        Props = j,
        Sample_Size = k,
        Proportion_Captured = captured_count / N,
        Proportion_Below = below_count / N,
        Proportion_Above = above_count / N,
        Range = range_value
      )
      
      Props_DF <- rbind(Props_DF, new_row)
    }
  }
}

#NOTE: YOU CAN PRODUCE THE FOLLOWING GRAPHS FOR EACH SAMPLE SIZE IF YOU
#INSERT THE CORRECT COMMAND FOR YOUR DEVICE IN THE NEXT PLOTS
#WE WERE WORKING ON TWO DIFFERENT TYPES OF DEVICES. I HAVE COMMENTED OUT
#THE WINDOWS() COMMAND BECAUSE WE ARE UNSURE WHAT DEVICE WILL BE CHECKING THE CODE.

#Plots for Proportion of Confidence intervals that captured true value of p.
for (k in sizes){
  #windows()
  par(mfrow = c(2, 3))
  for(i in 1:length(names)){
    df_i <- subset(Props_DF, Method == names[i] & Sample_Size == k)
    plot(df_i$Props, df_i$Proportion_Captured, main = paste(names[i],"n =",k), xlab = paste("p"), ylab = "Proportion Captured")
    abline(h = 0.95)
  }
}

#Plots of the Proportion of intervals that missed above or below the true value of p.
for (k in sizes){
  #windows()
  par(mfrow = c(2, 3))
  for(i in 1:length(names)){
    df_i <- subset(Props_DF, Method == names[i] & Sample_Size == k)
    plot(df_i$Props, df_i$Proportion_Below, main = paste(names[i],"n =",k), xlab = paste("p"), ylab = "Proportion", col = "blue",pch =16)
    points(df_i$Props, df_i$Proportion_Above, main = paste(names[i],"n =",k), col = "red", pch=17)
    legend("topright", inset = c(0.15, 0),legend = c("Proportion Below", "Proportion Above"), 
           col = c("blue", "red"), pch = c(16, 17))
  }
}

