
############################################################
####                                                    ####  
####  NRES 710, Lecture 1                               ####
####                                                    ####
####  Kevin Shoemaker and Ben Sullivan                  #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Basic Concepts of Probability and statistics      ####
############################################################



#### ALL FROGS IN CA

allfrogs.bodysize <- rlnorm(10000,1.5,0.4)        # statistical 'population'
hist(allfrogs.bodysize,main="",xlab="SVL (mm)")   # plot out histogram

truemean_SVL <- mean(allfrogs.bodysize)           # the 'parameter'
truemean_SVL 


mysample <- sample(allfrogs.bodysize,10)    # take sample of size 10 (10 frogs measured)
mean(mysample)   # compute the sample mean



mysample <- sample(allfrogs.bodysize,20)    # take sample of size 20 (20 frogs measured)
mean(mysample)   # compute the sample mean



lotsofsamples <- list()

for(s in 1:5000){
  lotsofsamples[[paste0("sample",s)]] <- sample(allfrogs.bodysize,30)    # take sample of size 30 (20 frogs measured)
}

lotsofsamples$sample1
lotsofsamples$sample99
lotsofsamples$sample732


samplemeans <- sapply(lotsofsamples,mean)

hist(samplemeans,xlab="mean body size (n=30)")


hist(rbinom(10000,1,.5),xlab="N heads out of 1")

par(mfrow=c(4,3))
for(i in 2:12){
   hist(rbinom(10000,i,.5),main=paste0("sample size = ",i),xlab=sprintf("N heads out of %s",i)) 
}


hist(rbinom(10000,1000,.5),xlab="N heads out of 1")


#######
# Sampling distribution: the sample mean

mysample <- c(4.1,3.5,3.7,6.6,8.0,5.4,7.3,4.4)
mysample
n <- length(mysample)    # sample size
sample.mean <- mean(mysample)  # sample mean
sample.stdev <- sd(mysample)   # sample standard deviation (r uses denominator of n-1 by default!)
std.error <- sample.stdev/sqrt(n) 

std.error 


sampdist <- function(x){dt((x-sample.mean)/std.error,n-1)}
curve(sampdist,0,11,ylab="probability density",xlab="value",main="sampling distribution for the sample mean!")
abline(v=sample.mean,col="green",lwd=3)
confint <- c(sample.mean+std.error*qt(0.025,n-1),sample.mean+std.error*qt(0.975,n-1))
abline(v=confint,col="blue",lty=2)


#######
# Sampling distribution: the sample mean #2 (brute force simulation version)

mysample <- c(4.1,1.5,3.7,6.6,8.0,4.5,5.3,4.4)
mysample
n <- length(mysample)    # sample size
sample.mean <- mean(mysample)  # sample mean
sample.stdev <- sd(mysample)   # sample standard deviation (r uses denominator of n-1 by default!)

simulated.samples <- list()
for(s in 1:10000){
  sd1 <- sqrt(sum((sample(mysample,length(mysample)-1,replace = T)-sample.mean)^2)/(length(mysample)-2))  # account for unknown standard deviation
  simulated.samples[[paste0("sample ",s)]] <- rnorm(n,sample.mean,sd1)
}
sampling.distribution <- sapply(simulated.samples,mean)

plot(density(sampling.distribution),xlim=c(0,11),ylab="probability density",xlab="value",main="sampling distribution for the sample mean!",lwd=2)    # plot the brute-force sampling distribution
hist(sampling.distribution,add=T,freq=F)
par(new=T)
curve(sampdist,0,11,xlim=c(0,11),xaxt="n",yaxt="n",xlab="",ylab="",col="red",lwd=2)  # official sampling distribution
abline(v=sample.mean,col="green",lwd=3)




## Paired t-test example:

weightloss.data <- c(-10.4,-11.6,3.9,1.5,-0.3,-3.5 -10.0,-6.7,-6.1,-2.4,-6.0,2.3,0.1,-4.1,-3.2, -11.3,-3.2,-9.3,-7.5,-5.7,-0.1,0.0,-9.8,1.0,-11.9)
hist(weightloss.data,breaks=7)
mean.weightloss <- mean(weightloss.data)
null.weightloss <- 0
stdev.weightloss <- sd(weightloss.data)
sample.size <- length(weightloss.data)
std.error <- stdev.weightloss/sqrt(sample.size)

t.statistic <- (mean.weightloss-null.weightloss)/std.error
t.statistic

curve(dt(x,sample.size-1),-5.5,2)
abline(v=t.statistic,col="green",lwd=3)

p=pt(t.statistic,sample.size-1)
p    # this is the p value


####### Alternative: use R's built in t test

t.test(weightloss.data,alternative = "less")   # should get the same p=value!


## Chi squared example

birthdays.bymonth <- c(40,23,33,39,28,29,45,31,22,34,44,20)
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
names(birthdays.bymonth) <- months

sample.size <- sum(birthdays.bymonth)
k = length(birthdays.bymonth)   # number of categories
exp.birthdays.bymonth <- sample.size*rep(1/k,times=k)

Chisq.stat <- sum((birthdays.bymonth-exp.birthdays.bymonth)^2/exp.birthdays.bymonth)
Chisq.stat


## View the summary statistic along with its sampling distribution under the null hypothesis

curve(dchisq(x,k-1),0,75)
abline(v=Chisq.stat,col="green",lwd=3)

p <- 1-pchisq(Chisq.stat,k-1)
p


### use R's built in chi squared function

chisq.test(birthdays.bymonth)     # should get the same p value!



## Z test  (Ben Sullivan example)

df <- read.csv("GSW_height.csv")
GSWheight <- df$Height
GSWheight
mean.gsw <- mean(GSWheight)
sd.gsw <- sd(GSWheight) 
sd.pop <- 4
n <- length(GSWheight)
s.e. <- sd.pop/sqrt(n)



null.height <- 79

z.statistic <- (mean.gsw-null.height)/s.e.
z.statistic

curve(dnorm(x),-3,3)
abline(v=z.statistic,col="green",lwd=3)

p <- 1-pnorm(z.statistic)    # is the p value enough evidence to tell you that GSW players are taller than the NBA average??
p       

pnorm(z.statistic)


#################
# Probability distributions

mean <- 5
rpois(10,mean)    # the random numbers have no decimal component

             # plot a discrete distribution!
xvals <- seq(0,15,1)
probs <- dpois(xvals,lambda=mean)
names(probs) <- xvals
               
barplot(probs,ylab="Probability",main="Poisson distribution (discrete)")

barplot(cumsum(probs),ylab="Cumulative Probability",main="Poisson distribution (discrete)")   # cumulative distribution

sum(probs)   # just to make sure it sums to 1!  Does it??? 


#########
# continuous distributions

shape1 = 0.5
shape2 = 0.5

rbeta(10,shape1,shape2)

curve(dbeta(x,shape1,shape2))   # probability density

curve(pbeta(x,shape1,shape2))   # cumulative distribution

integrate(f=dbeta,lower=0,upper=1,shape1=shape1,shape2=shape2)    # just to make sure it integrates to 1!!


##########
# Binomial

size <- 10
prob <- 0.3
rbinom(10,size,prob)

xvals <- seq(0,size,1)
probs <- dbinom(xvals,size,prob)
names(probs) <- xvals
               
barplot(probs,ylab="Probability",main="Binomial distribution")

barplot(cumsum(probs),ylab="Cumulative Probability",main="Binomial distribution")   # cumulative distribution

sum(probs)   # just to make sure it sums to 1!  Does it???


#########
# Gaussian

mean = 7.1
stdev = 1.9

rnorm(10,mean,stdev)

curve(dnorm(x,mean,stdev),0,15)   # probability density

curve(pnorm(x,mean,stdev),0,15)   # cumulative distribution

integrate(f=dnorm,lower=-Inf,upper=Inf,mean=mean,sd=stdev)    # just to make sure it integrates to 1!!



#########
# t distribution

df = 6

rt(10,df)     # random numbers from the t distribution (not sure why you would ever want this!)

curve(dt(x,df),-4,4)   # probability density

curve(pt(x,df),-4,4)   # cumulative distribution

integrate(f=dt,lower=-Inf,upper=Inf,df=df)    # just to make sure it integrates to 1!!



#########
# Chi-squared distribution

df = 6

rchisq(10,df)     # random numbers from the t distribution (not sure why you would ever want this!)

curve(dchisq(x,df),0,15)   # probability density

curve(pchisq(x,df),0,15)   # cumulative distribution

integrate(f=dchisq,lower=0,upper=Inf,df=df)    # just to make sure it integrates to 1!!


