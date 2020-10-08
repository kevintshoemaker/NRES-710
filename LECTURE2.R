
############################################################
####                                                    ####  
####  NRES 710, Lecture 2                               ####
####                                                    ####
####  Kevin Shoemaker and Ben Sullivan                  #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  T-tests                                           ####
############################################################



### one sample t-test (paired t-test is a type of one sample t-test)

sample.data <- rgamma(10,2,.1)
null.mean <- 10

sample.size <- length(sample.data)
sample.mean <- mean(sample.data)
sample.sd <- sd(sample.data)
std.err <- sample.sd/sqrt(sample.size)
t.stat <- (sample.mean-null.mean)/std.err

t.crit <- abs(qt(0.025,sample.size-1))   # for 2-tailed test

p.val <- (1-pt(abs(t.stat),sample.size-1))*2   # 


### alternatively use the t.test function:

t.test(sample.data,mu=null.mean)   # should get the same answer!



### two sample t-test 

sample.data.1 <- rnorm(15,55,10)
sample.data.2 <- rnorm(10,45,10)

sample.size.1 <- length(sample.data.1)
sample.size.2 <- length(sample.data.2)
sample.size.pooled <- length(sample.data.1) + length(sample.data.2)

sample.mean1 <- mean(sample.data.1)
sample.mean2 <- mean(sample.data.2)

sample.sd1 <- sd(sample.data.1)
sample.sd2 <- sd(sample.data.2)
sample.sd.pooled <- sqrt(((sample.size.1-1)*sample.sd1^2 + (sample.size.2-1)*sample.sd2^2)/(sample.size.pooled-2))

std.err.pooled <- sample.sd.pooled*sqrt(1/sample.size.1+1/sample.size.2)
t.stat <- (sample.mean1-sample.mean2)/std.err.pooled

t.crit <- abs(qt(0.025,sample.size-1))   # for 2-tailed test

p.val <- (1-pt(abs(t.stat),sample.size.pooled-2))*2   # 



### alternatively use the t.test function:

t.test(sample.data.1,sample.data.2,var.equal = T)   # should get the same answer!



######
# T test examples


#### 
# T-tests
####

#Ttest
# Are my data greater than zero? 
Group0 <- c(0.5, -0.03, 4, 2.5, 0.89, 2.2, 1.7, 1.125)
hist(Group0)
t.test(Group0,alternative="greater") # This gets at directionality


#Are my data different than zero? 
Group0 <- c(0.5, -0.03, 4, 2.5, 0.89, 2.2, 1.7, 1.125)
hist(Group0)
t.test(Group0) # Okay, that's to zero. What about if it's different than 1? 

# are my data different than 1? 
t.test(Group0, mu=1)

# Now let's test two groups. 
# are the means equal? 
group1 <- c(7,9,6,6,6,11,6,3,8,7)
group2 <- c(11,13,8,6,14,11,13,13,10,11)
t.test(group1, group2, var.equal=T) # Notice how we set equal variance? Look at the output - "Two Sample t-test."
# is this one-tail or two? 

group1 <- c(7,9,6,6,6,11,6,3,8,7)
group2 <- c(11,13,8,6,14,11,13,13,10,11)
t.test(group1, group2) # T value does not change, but DF And p-value do! "Welch Two Sample t-test"
# WELCH IS THE DEFAULT IN R


