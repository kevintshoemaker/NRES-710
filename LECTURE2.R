
##  NRES 710, Lecture 2                               
##    University of Nevada, Reno                       
##    Sampling distributions                                      


# Yellow-legged frog example ---------------------

### ALL FROGS IN CA (the statistical population- all the frogs!)

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
  lotsofsamples[[paste0("sample",s)]] <- sample(allfrogs.bodysize,30)    # take sample of size 30 (30 frogs measured)
}

lotsofsamples$sample1
lotsofsamples$sample99
lotsofsamples$sample732


samplemeans <- sapply(lotsofsamples,mean)

hist(samplemeans,xlab="mean body size (n=30)")    # visualize the sampling distribution!


barplot(table(rbinom(10000,1,.5))/10000,xlab="N heads out of 1",ylab="Probability")

par(mfrow=c(3,2))
for(i in seq(2,12,2)){
   barplot(table(rbinom(10000,i,.5))/10000,xlab=sprintf("N heads out of %s",i),ylab="Probability",main=paste0("sample size = ",i))
   #hist(rbinom(10000,i,.5),main=paste0("sample size = ",i),xlab=sprintf("N heads out of %s",i)) 
}


hist(rbinom(10000,1000,.5),xlab="N heads out of 1000",freq = F, main="")


# Survey of common sampling distributions -----------------

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


# Probability distributions ---------------------

# Discrete probability distributions 

mean <- 5
rpois(10,mean)    # note: the random numbers sampled from this distribution have no decimal component

             # plot discrete probabilities of getting particular outcomes!
xvals <- seq(0,15,1)
probs <- dpois(xvals,lambda=mean)
names(probs) <- xvals
               
barplot(probs,ylab="Probability Mass",main="Poisson distribution (discrete)")

barplot(cumsum(probs),ylab="Cumulative Probability",main="Poisson distribution (discrete)")   # cumulative distribution

sum(probs)   # just to make sure it sums to 1!  Does it???


# continuous distributions

shape1 = 0.5
shape2 = 0.5

rbeta(10,shape1,shape2)   # generate 10 random numbers from a continuous distribution

curve(dbeta(x,shape1,shape2),ylab="probability density",xlab="possibilities")   # probability density function (PDF)

curve(pbeta(x,shape1,shape2),ylab="cumulative probability",xlab="possibilities")   # cumulative distribution

integrate(f=dbeta,lower=0,upper=1,shape1=shape1,shape2=shape2)    # just to make sure it integrates to 1!!


# random number generators

rnorm(10)    # generate 10 random numbers from a standard normal distribution
rnorm(5,25,5)  # generate 5 random numbers from a normal distribution with mean=25 and sd=5
rpois(8,18)  # generate 8 random numbers from a poisson distribution with mean=18


## probability density function example 

curve(dt(x,8),-4,4,xlab="possibilities",ylab='relative probability (prob density)')


## probability mass function example 

x <- barplot(sapply(0:10,function(t) dpois(t,2)),xlab="possibilities",ylab='probability')
axis(1,at=x,labels=0:10)


## cumulative distribution function  

    # for continuous distribution
curve(pt(x,df=8),-4,4,xlab="possibilities",ylab='cumulative probability')

    # for discrete distribution
x <- barplot(sapply(0:10,function(t) ppois(t,2)),xlab="possibilities",ylab='cumulative probability')
axis(1,at=x,labels=0:10)


## quantile function  

    # for continuous distribution
curve(qt(x,df=8),0,1,xlab="cumulative probability",ylab='quantile')

    # for discrete distribution
curve(qpois(x,4),0,1,xlab="cumulative probability",ylab='quantile')


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


# Gaussian (normal)

mean = 7.1
stdev = 1.9

rnorm(10,mean,stdev)

curve(dnorm(x,mean,stdev),0,15)   # probability density

curve(pnorm(x,mean,stdev),0,15)   # cumulative distribution

integrate(f=dnorm,lower=-Inf,upper=Inf,mean=mean,sd=stdev)    # just to make sure it integrates to 1!!



# t distribution

df = 6

rt(10,df)     # random numbers from the t distribution

curve(dt(x,df),-4,4)   # probability density

curve(pt(x,df),-4,4)   # cumulative distribution

integrate(f=dt,lower=-Inf,upper=Inf,df=df)    # just to make sure it integrates to 1!!



# Chi-squared distribution

df = 6

rchisq(10,df)     # random numbers from the chi squared distribution

curve(dchisq(x,df),0,15)   # probability density

curve(pchisq(x,df),0,15)   # cumulative distribution

integrate(f=dchisq,lower=0,upper=Inf,df=df)    # just to make sure it integrates to 1!!


