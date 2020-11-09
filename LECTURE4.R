
############################################################
####                                                    ####  
####  NRES 710, Lecture 4                               ####
####                                                    ####
####  Kevin Shoemaker and Ben Sullivan                  #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Linear Regression and ANOVA                       ####
############################################################



eggs.per.nest <- 100
n.nests <- 15
light <- rnorm(n.nests,50,10)   # make up some light pollution values (predictor var)

probsucc <- function(light){    # egg success as a function of light pollution
  plogis(1.5-0.01*light)
}

hatchlings.successful <- rbinom(n.nests,eggs.per.nest,probsucc(light))   # determine number of successful eggs (response var)

#curve(probsucc,0,100)

plot(hatchlings.successful~light)  # plot the data



slope <- sum((light-mean(light))*(hatchlings.successful-mean(hatchlings.successful)))/sum((light-mean(light))^2)
intercept <- mean(hatchlings.successful) - slope*mean(light)

exp.successful <- intercept+slope*light # expected number of eggs for each observation
residuals <- hatchlings.successful-exp.successful

stderr <- sqrt(((1/(n.nests-2))*sum(residuals^2))/(sum((light-mean(light))^2)))    # standard error

t.stat <- (slope-0)/stderr    # t statistic

pval <- 2*pt(t.stat,n.nests-2)    # p value


############
# use lm function instead (easy way!)

model <- lm(hatchlings.successful~light)

summary(model)   # get the same t stat and p-value hopefully!


############
# plot regression line!

plot(hatchlings.successful~light)  # plot the data
abline(intercept,slope,col="blue")



mod <- lm(Volume~Girth,data=trees)
summary(mod)


my.intercept <- model$coefficients["(Intercept)"]
my.slope <- model$coefficients["light"]
expected.vals <- my.intercept+my.slope*light 
my.residuals <- hatchlings.successful-expected.vals
my.residuals

### alternative way of getting residuals (best way!)

my.residuals2 <- model$residuals

### alternative way using predict function

my.residuals3 <- hatchlings.successful-predict(model)

### histogram of residuals

hist(my.residuals)

### test for normality

qqnorm(my.residuals)

shapiro.test(my.residuals)



layout(matrix(1:4,nrow=2,byrow = T))
plot(anscombe$y1~anscombe$x1,ylab="response",xlab="predictor")
plot(anscombe$y2~anscombe$x2,ylab="response",xlab="predictor")
plot(anscombe$y3~anscombe$x3,ylab="response",xlab="predictor")
plot(anscombe$y4~anscombe$x4,ylab="response",xlab="predictor")


my.residuals <- model$residuals

plot(my.residuals~light)

plot(my.residuals~predict(model))


layout(matrix(1:4,2,byrow = T))
plot(model)


layout(matrix(1:4,nrow=2,byrow = T))
plot(anscombe$y1~anscombe$x1,ylab="response",xlab="predictor")
plot(anscombe$y2~anscombe$x2,ylab="response",xlab="predictor")
plot(anscombe$y3~anscombe$x3,ylab="response",xlab="predictor")
plot(anscombe$y4~anscombe$x4,ylab="response",xlab="predictor")

