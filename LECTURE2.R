
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


