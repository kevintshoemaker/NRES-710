
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

p.val <- (1-pt(abs(t.stat),sample.size.pooled-2))*2   # 2-tailed test



### alternatively use the t.test function:

t.test(sample.data.1,sample.data.2,var.equal = T)   # should get the same answer!



# one vs two tailed demo

#my.data <- rnorm(15, 0.5, 1)   # generate sample data
my.data <- c(0.20119786,1.41700898,-0.72426698,0.44006284,0.01487128,-0.19031680,1.75470699,-0.81992816,2.31978530,  2.71442595,-0.31461411,0.52086138,-0.50580117,1.52260888,0.76454698)
samp.mean <- mean(my.data)
samp.sd <- sd(my.data)
samp.n <- length(my.data)
std.err <- samp.sd/sqrt(samp.n)

null.mean <- 0

t.statistic <- (samp.mean-null.mean)/std.err

### Two-tailed
curve(dt(x,samp.n-1),-3,3, main="Meaning of more extreme (two tailed version)",
      ylab="probability density",xlab="t statistic")    # visualize the sampling distribution of the t-statistic
abline(v=t.statistic,lwd=2,col="blue")

xs <- seq(abs(t.statistic),10,0.05)                
ys <- dt(xs,samp.n-1)
polygon(x=c(xs,rev(xs)),y=c(ys,rep(0,times=length(ys))),col="green",border=NA)
polygon(x=c(-xs,rev(-xs)),y=c(ys,rep(0,times=length(ys))),col="green",border=NA)

p.twosided <- pt(-abs(t.statistic),samp.n-1)*2     # two-tailed p-value

text(-2,0.3,paste("p =",round(p.twosided,4)))

### One-sided (alternative = 'greater')
curve(dt(x,samp.n-1),-3,3, main="Meaning of more extreme (one tailed version: greater than)",
      ylab="probability density",xlab="t statistic")    # visualize the sampling distribution of the t-statistic
abline(v=t.statistic,lwd=2,col="blue")

xs <- seq(t.statistic,10,0.05)                
ys <- dt(xs,samp.n-1)
polygon(x=c(xs,rev(xs)),y=c(ys,rep(0,times=length(ys))),col="green",border=NA)

p.onesided <- pt(-abs(t.statistic),samp.n-1)     # one-tailed p-value

text(-2,0.3,paste("p =",round(p.onesided,4)))



### t-crit in one tailed vs two tailed test

sample.size=7

curve(dt(x,sample.size-1),-8,4, main="2-tailed vs 1-tailed critical value",
      xlab="t-statistic",ylab="probability density")

alpha <- 0.1
t.crit.twosided <- qt(alpha/2,sample.size-1) 

abline(v=c(t.crit.twosided,abs(t.crit.twosided)),col="red",lwd=2)


t.crit.twosided <- qt(alpha/2,sample.size-1) 

abline(v=c(t.crit.twosided,abs(t.crit.twosided)),col="red",lwd=2)

t.crit.onesided <- qt(alpha,sample.size-1) 

abline(v=abs(t.crit.onesided),col="green",lwd=2)
abline(v=t.crit.onesided,col="blue",lwd=2)

legend("topleft",lwd=c(2,2,2),col=c("red","green","blue"),bty="n",legend=c("two-tailed crit value","one-tailed crit value (greater than)","one-tailed crit value (less than)"))


######
# More t-test examples


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
t.test(group1, group2) #  "Welch's Two Sample t-test"
# WELCH'S TEST IS THE DEFAULT IN R



## testing for normality

my.data.nonnorm <- rgamma(20,0.1,0.1)    # simulate some non-normal data
hist(my.data.nonnorm,main="non-normal")     # visualize the data distribution

my.data.norm <- rnorm(20,6,0.9)    # simulate some normal data
hist(my.data.norm,main="normal")     # visualize the data distribution

# visualize q-q plot

qqnorm(my.data.nonnorm,main="non-normal")   # visual test for normality

qqnorm(my.data.norm,main="normal")


# run shapiro-wilk test

shapiro.test(my.data.nonnorm)

shapiro.test(my.data.norm)


# Wilcoxon signed rank test

my.data <- rgamma(20,0.1,0.1)-2

hist(my.data)

wilcox.test(my.data)

t.test(my.data)   # t-test for comparison
  
  

# Wilcoxon signed rank test

my.data1 <- rgamma(20,0.1,0.1)-2
my.data2 <- rgamma(20,0.2,0.1)-2

median(my.data1)
median(my.data2)

wilcox.test(my.data1,my.data2)

t.test(my.data1,my.data2)   # t-test for comparison


#### perform rank test from scratch!

allobs <- c(my.data1,my.data2)
inorder <- order(allobs)

rank1 <- inorder[1:20]
rank2 <- inorder[21:40]

t.test(rank1,rank2,var.equal = T)    # perform t-test on the ranks (usually similar to Mann-Whitney test)
  

###### First we will set the population parameters:

true.mean.A <- 13.5
true.mean.B <- 13.9

true.sd <- 3.4

# Assume a two-tailed test- alternative hypothesis is that the mean of A is different from the mean of B

###### Now let's set the sampling scenario

sampsize.A <- 10
sampsize.B <- 12

###### now we will simulate a single 'experiment'

samp.A <- rnorm(sampsize.A,true.mean.A,true.sd)
samp.B <- rnorm(sampsize.B,true.mean.B,true.sd)

###### and now we can run a test!

this.test <- t.test(samp.A,samp.B,var.equal = T)

###### and determine if we rejected our null hypothesis (which we know is not true!)

this.test$p.value < 0.05




### full power analysis:

###### First we will set the population parameters:

true.mean.A <- 13.5
true.mean.B <- 13.9

true.sd <- 3.4

# Assume a two-tailed test- alternative hypothesis is that the mean of A is different from the mean of B

###### Now let's set the sampling scenario

sampsize.A <- 10
sampsize.B <- 12

###### now we will simulate LOTS of  'experiments'

pvals <- numeric(1000)

for(i in 1:1000){
  samp.A <- rnorm(sampsize.A,true.mean.A,true.sd)
  samp.B <- rnorm(sampsize.B,true.mean.B,true.sd)
  
  ###### and now we can run a test!
  
  this.test <- t.test(samp.A,samp.B,var.equal = T)
  
  ###### and determine if we rejected our null hypothesis (which we know is not true!)
  
  pvals[i] <- this.test$p.value
}

 # hist(pvals)

length(which(pvals<0.05))/1000


### full power analysis WITH SAMPLE SIZE DETERMINATION:

###### First we will set the population parameters:

true.mean.A <- 13.5
true.mean.B <- 13.9

true.sd <- 3.4

# Assume a two-tailed test- alternative hypothesis is that the mean of A is different from the mean of B


###### now we will simulate LOTS of  'experiments' under different sample sizes

sampsize <- seq(5,400,10)

power <- numeric(length(sampsize))

for(j in 1:length(sampsize)){
  
  pvals <- numeric(1000)
  for(i in 1:1000){
    samp.A <- rnorm(sampsize[j],true.mean.A,true.sd)
    samp.B <- rnorm(sampsize[j],true.mean.B,true.sd)
    
    ###### and now we can run a test!
    
    this.test <- t.test(samp.A,samp.B,var.equal = T)
    
    ###### and determine if we rejected our null hypothesis (which we know is not true!)
    
    pvals[i] <- this.test$p.value
  }

 # hist(pvals)

  power[j] <- length(which(pvals<0.05))/1000
}

names(power) <- sampsize

plot(power~sampsize)


