

curve(dnorm(x,4,10),-10,50)

abline(v=c(20+1,20-1),col="blue")
abline(v=c(20+5*2,20-5*2),col="red")

rnorm(10,20,1)

####
# sampling distribution: distribution of sample means (z dist- assume normality of sample means)

population <- runif(10000,20,50)

rand <- sample(population, 10)
mean(rand)

samples <- lapply(1:100,function(t) sample(population, 10))

sample_means <- sapply(samples,mean)
hist(sample_means)

standard.err <- sd(population)/sqrt(10)

hist(sample_means,freq=F)
curve(dnorm(x,mean(population),standard.err),add=T)



####
# sampling distribution: distribution of sample means (t dist- do not assume normality of sample means)

population <- runif(10000,20,50)    # UNOBSERVED, UNKNOWN 

rand <- sample(population, 10)
mean(rand)

samples <- lapply(1:100,function(t) sample(population, 10))

our.sample <- samples[[91]]

sample.mean <- mean(our.sample)     # DATA (all we observe)  # THIS IS NORMAL!
sample.mean
sample.sd <- sd(our.sample)

standard.err <- sample.sd/sqrt(10)
standard.err

hypoth.mean <- 35

t.stat <- (sample.mean-hypoth.mean)/standard.err

curve(dt(x,300),-4,4)

curve(dnorm(x),-4,4,add=T,col="green")

abline(v=t.stat)





