
############################################################
####                                                    ####  
####  NRES 746, Lecture 1                               ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Computational algorithms vs standard statistics   ####
############################################################



#############
# Start with a made-up data frame!
#############

df <- data.frame(
  TreatmentA = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179),
  TreatmentB = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180) 
)

summary(df)    # summarize! 

sample.size <- length(df$TreatmentA)     # determine sample size    

reshape_df <- data.frame(                # "reshape" the data frame so each observation gets its own row (standard format)
  Treatment = rep(c("A","B"),each=sample.size),
  Mass = c(df$TreatmentA,df$TreatmentB)
)
plot(Mass~Treatment, data=reshape_df)    # explore/visualize the data

# boxplot(df$TreatmentA,df$TreatmentB,names=c("TreatmentA","TreatmentB"))  # (alternative method!)

observed_dif <- mean(df$TreatmentA) - mean(df$TreatmentB)     # compute sample statistic
observed_dif


################
# Perform standard t-test
################

t.test(df$TreatmentA,df$TreatmentB, var.equal=TRUE, paired=FALSE)


######################
   # ALTERNATIVE ALGORITHMIC APPROACH!
######################

#############
# Simulate the STATISTICAL POPULATION under the null hypothesis
#############

lots <- 1000000  # large number approximating infinity 

popMean_null <- mean(reshape_df$Mass)        # assume groups A and B come from a population with common mean 
popSD_null <- sd(reshape_df$Mass)                      # and common standard deviation... 
popData_null <- rnorm(n=lots,mean=popMean_null,sd=popSD_null)    # the statistical "population" of interest (under null model w no treatment effect)


#############
# Draw a SAMPLE from that population
#############

sampleA <- sample(popData_null,size=sample.size)    # use R's native "sample()" function
sampleB <- sample(popData_null,size=sample.size)

round(sampleA)
difference <- mean(sampleA)-mean(sampleB)   # sample statistic = difference between sample means
difference


#################
# Repeat this process using a FOR loop
#################

reps <- 1000                 # set the number of replicates
null_difs <- numeric(reps)       # initialize a storage vector

for(i in 1:reps){            # for each replicate... 
  sampleA <- sample(popData_null,size=sample.size)      # draw a sample of body masses assuming no treatment effect       
  sampleB <- sample(popData_null,size=sample.size)      # draw a sample of body masses assuming no treatment effect (again!)
  null_difs[i] <- mean(sampleA)-mean(sampleB)           # compute and store the sampling error produced under the null hypothesis
}

hist(null_difs)       # plot out all the sampling errors under the null hypothesis as a histogram
abline(v=observed_dif,col="green",lwd=3)     # indicate the observed sample statistic. 


############
# Generate a p-value algorithmically!!
############

ordered_difs <- sort(abs(null_difs))       # sort the vector of (absolute) anomalies (sampling errors) 
higher_anomaly <- length(which(ordered_difs>=abs(observed_dif)))       # how many of these sampling errors equal or exceed the "error" represented by the observed statistic?
p_value <- higher_anomaly/reps       # compute a p-value! 
p_value


#############
# Develop a function that wraps up all the above steps into one!
#############

t.test.algorithm <- function(dat = reshape_df, group = "Treatment", value = "Mass" ){
  
  #############
  # Compute the sample statistic
  #############
  
  indexA <- which(dat[,group]=="A")     # rows representing treatment A
  indexB <- which(dat[,group]=="B")     # rows representing treatment B
  observed_dif <- mean(dat[indexA,value]) - mean(dat[indexB,value])
  
  sample.size <- length(indexA)   # compute sample size
  
  #############
  # Simulate the STATISTICAL POPULATION under the null hypothesis
  #############
  
  lots <- 1000000  # large number approximating infinity 
  
  popMean_null <- mean(dat[,value])           # assume groups A and B come from a population with common mean 
  popSD_null <- sd(dat[,value])                      # and common standard deviation... 
  popData_null <- rnorm(n=lots,mean=popMean_null,sd=popSD_null)    # the statistical "population" of interest (under null model w no treatment effect)

  #################
  # Repeat sampling process (sampling from population) using a FOR loop
  #################
  
  reps <- 1000                 # set the number of replicates
  null_difs <- numeric(reps)       # initialize a storage structure to hold one anomaly (sampling error) per replicate
  
  for(i in 1:reps){            # for each replicate... 
    sampleA <- sample(popData_null,size=sample.size)      # draw a sample assuming no treatment effect       
    sampleB <- sample(popData_null,size=sample.size)      # draw a sample assuming no treatment effect (again!)
    null_difs[i] <- mean(sampleA)-mean(sampleB)           # compute and store the sampling error produced under the null hypothesis
  }
  
  ordered_difs <- sort(abs(null_difs))       # sort the vector of sampling errors 
  higher_anomaly <- length(which(ordered_difs>=abs(observed_dif)))       # how many of these sampling errors equal or exceed the sample statistic?
  p_value <- higher_anomaly/reps
  
  to_return <- list()   # initialize object to return
  
  to_return$null_difs <- null_difs
  to_return$p_value <- p_value
  to_return$observed_dif <- observed_dif
  
  return(to_return)

}

ttest <- t.test.algorithm(dat = reshape_df, group = "Treatment", value = "Mass" )   # try to run the new function

ttest$p_value     # get the p_value

hist(ttest$null_difs)       # plot out all the sampling errors under the null hypothesis as a histogram
abline(v=ttest$observed_dif,col="green",lwd=3)     # indicate the observed sample statistic. 


###########
# Test data for unequal variances...

df.vardif <- data.frame(
  TreatmentA = c(135, 128, 139, 122, 126, 121, 128, 135, 134, 129),
  TreatmentB = c(215, 69, 143, 153, 218, 186, 125, 98, 271, 340) 
)

summary(df.vardif)    # summarize! 


###########
# Test data for unequal sample sizes...

# NOTE: we use R's missing data designation "NA" to fill in missing data for treatment B here... 

df.ndif <- data.frame(
  TreatmentA = c(135, 128, 139, 122, 126, 121, 128, 135, 134, 129, 134, 125, 130, 132, 125),
  TreatmentB = c(98, 271, 340, rep(NA,12)) 
)

summary(df.ndif)    # summarize!    


##################
# NON-PARAMETRIC T-TEST -- PERMUTATION TEST
##################

reps <- 5000            # Define the number of permutations to run (number of replicates)
null_difs <- numeric(reps)   # initialize storage variable
for (i in 1:reps){			# For each replicate:		
  newGroup <- reshape_df$Treatment[sample(c(1:nrow(reshape_df)))]			   # randomly shuffle the observed data with respect to treatment group
	dif <- mean(reshape_df$Mass[newGroup=="A"])	- mean(reshape_df$Mass[newGroup=="B"])	   #  compute the difference between the group means after reshuffling the data
	null_difs[i] <- dif	    # store this value in a vector
}
hist(null_difs)    # Plot a histogram of null differences between group A and group B under the null hypothesis (sampling errors)
abline(v=observed_dif,col="green",lwd=3)   # Add a vertical line to the plot to indicate the observed difference


########
# Compute a p-value based on the permutation test, just like we did before!
########

higher_anomaly <- length(which(abs(null_difs)>=abs(observed_dif)))
p_value <- higher_anomaly/reps  
p_value


#############
# Develop a function that performs a permutation-t-test!
#############

t.test.permutation <- function(dat = reshape_df, group = "Treatment", value = "Mass" ){
  
  #############
  # Compute the sample statistic
  #############
  
  indexA <- which(dat[,group]=="A")     # rows representing treatment A
  indexB <- which(dat[,group]=="B")     # rows representing treatment B
  observed_dif <- mean(dat[indexA,value]) - mean(dat[indexB,value])
  
  reps <- 5000            # Define the number of permutations to run (number of replicates)
  null_difs <- numeric(reps)   # initialize storage variable
  for (i in 1:reps){			# For each replicate:		
    newGroup <- reshape_df$Treatment[sample(c(1:nrow(reshape_df)))]			   # randomly shuffle the observed data with respect to treatment group
  	dif <- mean(reshape_df$Mass[newGroup=="A"])	- mean(reshape_df$Mass[newGroup=="B"])	   #  compute the difference between the group means after reshuffling the data
  	null_difs[i] <- dif	    # store this value in a vector
  }
  
  higher_anomaly <- length(which(abs(null_difs)>=abs(observed_dif)))
  p_value <- higher_anomaly/reps  
  
  to_return <- list()   # initialize object to return
  
  to_return$null_difs <- null_difs
  to_return$p_value <- p_value
  to_return$observed_dif <- observed_dif
  
  return(to_return)
  
}

ttest2 <- t.test.permutation()

ttest2$p_value

hist(ttest2$null_difs)    # Plot a histogram of null differences between group A and group B under the null hypothesis (sampling errors)
abline(v=ttest2$observed_dif,col="green",lwd=3)   # Add a vertical line to the plot to indicate the observed difference



##############
# Demonstration: bootstrapping a confidence interval!

## use the "trees" dataset in R:

head(trees)   # use help(trees) for more information


#########
# Basic data exploration

plot(trees$Volume~trees$Height, main = 'Black Cherry Tree Height/Volume Relationship', xlab = 'Height', ylab = 'Volume', pch = 16, col ='blue')
plot(trees$Volume~trees$Girth, main = 'Black Cherry Tree Girth/Volume Relationship', xlab = 'Girth', ylab = 'Volume', pch = 16, col ='red')


#########
# Function for returning a vector of R-squared statistics from models regressing a response variable on multiple possible predictor variables
   # here we assume that all columns in the input data frame that are NOT the response variable are potential predictor variables.

Rsquared <- function(df,responsevar="Volume"){    # univariate models only- interaction and multiple regression not implemented here
  response <- df[,responsevar]       # extract the response variable
  names <- names(df)                  
  rsq <- numeric(length(names))        # named storage vector
  names(rsq) <- names(df)               
  rsq <- rsq[names(rsq)!=responsevar]           # assume that all columns that are not the response variable are possible predictor variables
  for(i in names(rsq)){         # loop through predictors
      predictor <- df[,i]                  # extract this predictor
      model <- lm(response~predictor)       # regress response on predictor
      rsq[i] <- summary(model)$r.square       # extract R-squared statistic
  }
  return(rsq)     
}


#########
# test the function to see if it works!

stat <- Rsquared(trees,"Volume")
stat


############
# new function to generate "bootstrap" samples from a data frame

boot_sample <- function(df,statfunc,n_samples,n_stats,responsevar="Volume"){
  indices <- c(1:nrow(df))
  output <- matrix(NA,nrow=n_samples,ncol=n_stats)        # storage object- to store a single bootstrapped sample from the original data
  
  for(i in 1:n_samples){              # for each bootstrap replicate:
    boot_rows <- sample(indices,size=nrow(df),replace=T)         # randomly sample observations with replacement
    newdf <- df[boot_rows,]                       # dataframe of bootstrapped observations
    output[i,] <- statfunc(newdf,responsevar)                 # generate statistics from the bootstrapped sample  (e.g., compute Rsquared after regressing y on all possible x variables)
  }
  return(output)
}


##########
# Generate a few bootstrapped samples!

boot <- boot_sample(df=trees,statfunc=Rsquared,n_samples=10,n_stats=2)       # generate test stats from lots of bootstrapped samples
colnames(boot) <- names(stat)         # name the columns to recall which predictor variables they represent

boot
stat


#############
# use bootstrapping to generate confidence intervals for R-squared statistic!

boot <- boot_sample(df=trees,statfunc=Rsquared,n_samples=1000,n_stats=2)   # generate test statistics (Rsquared vals) for 1000 bootstrap samples
confint <- apply(boot,2,function(t)  quantile(t,c(0.025,0.5,0.975)))       # summarize the quantiles to generate confidence intervals for each predictor variable
colnames(confint) <- names(stat)
t(confint)


