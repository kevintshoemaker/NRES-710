
############################################################
####                                                    ####  
####  NRES 710, Lecture 3                               ####
####                                                    ####
####  Kevin Shoemaker and Ben Sullivan                  #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Chi-squared tests etc.                            ####
############################################################


## Chi squared goodness-of-fit example

birthdays.bymonth <- c(40,23,33,39,28,29,45,31,22,34,44,20)
months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
names(birthdays.bymonth) <- months

sample.size <- sum(birthdays.bymonth)
k = length(birthdays.bymonth)   # number of categories (months)
exp.birthdays.bymonth <- sample.size*rep(1/k,times=k)   # compute the expected number under the null hypothesis.

Chisq.stat <- sum((birthdays.bymonth-exp.birthdays.bymonth)^2/exp.birthdays.bymonth)
Chisq.stat


## View the summary statistic along with its sampling distribution under the null hypothesis

curve(dchisq(x,k-1),0,75)
abline(v=Chisq.stat,col="green",lwd=3)

p <- 1-pchisq(Chisq.stat,k-1)
p


### use R's built in chi squared function

chisq.test(birthdays.bymonth)     # should get the same p value!



n.bunnies <- 112

    # sample assuming null hypothesis is true
all.bunnies <- data.frame(
  ear_type = sample(c("floppy","pointy","mixed"),n.bunnies,replace = T),
  coat_color = sample(c("white","brown","mixed"),n.bunnies,replace = T)
)

head(all.bunnies)

######
# make contingency table

con_table <- table(all.bunnies$ear_type,all.bunnies$coat_color)

######
# generate expected values

prop.ears <- rowSums(con_table)/n.bunnies
sum.coats <- colSums(con_table)
exp_table <- sapply(1:ncol(con_table),function(t) sum.coats[t]*prop.ears)
colnames(exp_table) <- colnames(con_table)

#####
# compute chi-squared statistic

Chisq.stat <- sum((con_table-exp_table)^2/exp_table)


#####
# Compare chi squared statistic with null sampling distribution

curve(dchisq(x,4),0,10)
abline(v=Chisq.stat,col="blue",lwd=2)

p.value <- 1-pchisq(Chisq.stat,4)
p.value

text(7,0.15,paste0("p = ",round(p.value,3)))


######
# Compare with R's built in chi squared function

chisq.test(con_table)



######
# Chi-squared test for normality

normal.data <- rnorm(250,4,1.5)  # generate normal data
nonnormal.data <-  runif(250,2,6)  # generate non-normal data

hist(normal.data)
hist(nonnormal.data)

#####
# First bin the data (chi squared test must be on categorical data!)

breaks <- c(-Inf,1,2,3,3.5,4,4.5,5,6,7,Inf)

normal.data.binned <- cut(normal.data,breaks,labels=breaks[-1])
nonnormal.data.binned <- cut(nonnormal.data,breaks,labels=breaks[-1])

obs_table_norm <- table(normal.data.binned)
obs_table_nonnorm <- table(nonnormal.data.binned)

#####
# Determine expected values in each cell if the underlying distribution were normal

normal.probs <- sapply(2:length(breaks), function(t) pnorm(breaks[t],mean(normal.data),sd(normal.data))-pnorm(breaks[t-1],mean(normal.data),sd(normal.data))  )
exp_table_norm <- length(normal.data)*normal.probs  # check that expected vals are greater than 5

normal.probs <- sapply(2:length(breaks), function(t) pnorm(breaks[t],mean(nonnormal.data),sd(nonnormal.data))-pnorm(breaks[t-1],mean(nonnormal.data),sd(nonnormal.data))  )
exp_table_nonnorm <- length(normal.data)*normal.probs  # check that expected vals are greater than 5

######
# Chi squared stat for normality test on normal data

chistat_norm <- sum((obs_table_norm-exp_table_norm)^2/exp_table_norm)
pval_norm <- 1-pchisq(chistat_norm,length(exp_table_norm)-1)
pval_norm

chistat_nonnorm <- sum((obs_table_nonnorm-exp_table_nonnorm)^2/exp_table_nonnorm)
pval_nonnorm <- 1-pchisq(chistat_nonnorm,length(exp_table_nonnorm)-1)
pval_nonnorm

####
# Compare with shapiro wilk test (which is a MUCH better test!)

shapiro.test(normal.data)
shapiro.test(nonnormal.data)


n.bunnies <- 112

    # sample assuming null hypothesis is true
all.bunnies <- data.frame(
  ear_type = sample(c("floppy","pointy","mixed"),n.bunnies,replace = T),
  coat_color = sample(c("white","brown","mixed"),n.bunnies,replace = T)
)

head(all.bunnies)

######
# make contingency table

con_table <- table(all.bunnies$ear_type,all.bunnies$coat_color)

######
# generate expected values

prop.ears <- rowSums(con_table)/n.bunnies
sum.coats <- colSums(con_table)
exp_table <- sapply(1:ncol(con_table),function(t) sum.coats[t]*prop.ears)
colnames(exp_table) <- colnames(con_table)

#####
# compute chi-squared and G statistics

Chisq.stat <- sum((con_table-exp_table)^2/exp_table)
G.stat <- 2*sum(con_table*log(con_table/exp_table))   # slightly different!


#####
# Compare chi squared and G statistics with null sampling distribution

curve(dchisq(x,4),0,10)
abline(v=Chisq.stat,col="blue",lwd=1)
abline(v=G.stat,col="red",lwd=1)
legend("topright",lty=c(1,1),lwd=c(1,1),col=c("blue","red"),legend=c("Chi-squared","G stat"),bty="n")

p.value.Chi <- 1-pchisq(Chisq.stat,4)
p.value.G <- 1-pchisq(G.stat,4)

text(7,0.12,paste0("p.Chi = ",round(p.value.Chi,3)))
text(7,0.1,paste0("p.G = ",round(p.value.G,3)))


n.bunnies <- 20

    # sample assuming null hypothesis is true
all.bunnies <- data.frame(
  ear_type = sample(c("floppy","pointy","mixed"),n.bunnies,replace = T),
  coat_color = sample(c("white","brown","mixed"),n.bunnies,replace = T)
)

head(all.bunnies)

######
# make contingency table

con_table <- table(all.bunnies$ear_type,all.bunnies$coat_color)

######
# generate expected values

prop.ears <- rowSums(con_table)/n.bunnies
sum.coats <- colSums(con_table)
exp_table <- sapply(1:ncol(con_table),function(t) sum.coats[t]*prop.ears)
colnames(exp_table) <- colnames(con_table)

#####
# run fisher exact test

#?fisher.test

chisq.test(con_table)
fisher.test(con_table)   # better test
fisher.test(con_table,simulate.p.value = T,B=5000)   # use simulated p-value - usually very similar answer

