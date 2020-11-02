
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


