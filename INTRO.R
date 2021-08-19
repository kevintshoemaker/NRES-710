
############################################################
####                                                    ####  
####  NRES 710, Introduction                            ####
####                                                    ####
####  Kevin Shoemaker and Ben Sullivan                  #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Getting started with R                            ####
############################################################



2+2               # use R as a calculator
four <- 2+2       # define your first variable!
four
five<-2+2         # you can make mistakes and define misleading labels- R will let you!
five<-2+3
five<-four+1      # you can use variables to define new ones

# data()     # 'uncomment' this command and run it to explore built-in datasets



#iris                   # this is a data frame- the basic data storage type in R
head(iris)             # [add your own comment here!]
tail(iris)

#  ?iris               # uncomment this to learn more about the iris dataset
str(iris)


len<-iris$Petal.Length
hist(len)             # what does this do? How could you learn more about this 'hist' function?

# Q: what kind of data are petal lengths?


#install.packages("titanic")     # uncomment this command to install the package- you only need to install once!
 
library(titanic)              # this 'loads' the package and needs to be done every time you run this script

data("titanic_train")
head(titanic_train)
# ?titanic_train              # uncomment and run to learn more about the data

# Q: What kind of data are those in the "Embarked" column? 
# Q: What kind of data are those in "Pclass?"



# Make our own data
# lets pull 15 numbers from the standard normal distribution

a <- rnorm(15)
a <- rnorm(15,mean=2,sd=0.5)
a

# want to be able to repeat this? set the "seed."

set.seed(1234)
a <- rnorm(15)
a

# let's pull 15 numbers from the binomial distribution
b<- rbinom(15, size=1, prob=0.2) # we could "weight the coin"
b


# we can create categories: 
unit<-rep(c("Control","+N","+P","+NP"),each=20)
unit


# we can even create a whole dataframe
my.data <- data.frame(
  Obs.Id = 1:100,
  Treatment = rep(c("A","B","C","D","E"),each=20),
  Block = rep(1:20,times=5),
  Germination = rpois(100,lambda=rep(c(1,5,4,7,1),each=20)),
  AvgHeight = rnorm(100,mean=rep(c(10,30,31,25,35,7),each=20))
)
head(my.data)



# Then, we can import data. 

# Don't forget to set your working directory (or just make sure you're using an Rstudio Project). I did it here, just for fun... 

# setwd("~/Desktop")       # uncomment and run if you want to set the desktop as your working directory.

#Read in the data. Note that the file needs to be in csv format, the name must be in quotes, and the name must include the csv extension.

# Pleach<-read.csv("PbyTime_Bio.csv", header=T) # obvs, this won't work for you because you don't have this file. 

# Use your own file to try it out. This is an example! 


