
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



######
# add confidence interval on the regression line

newdata <- data.frame(    # make a data frame containing the light values we want to make predictions for (spanning the range of light values in our data)
  light = seq(20,80,1)
)

my.predict <- predict(model, newdata = newdata, interval = "confidence")  # 95% conf int by default

plot(hatchlings.successful~light)  # plot the data
abline(intercept,slope,col="blue")
lines(newdata$light,my.predict[,"upr"],col="red",lty=2)   # add upper bound
lines(newdata$light,my.predict[,"lwr"],col="red",lty=2)   # add lower bound


#  use the lm function to regress tree volume on tree girth using the 'trees' dataset

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


## load the 'mtcars' data set

data(mtcars)

## define which variables to assess correlation for.

myvars <- c("disp","hp","wt")

## grab only the variables of interest

mtcars.reduced <- mtcars[,myvars]

## compute (pearson) correlations for all pairs of variables (correlation matrix)

cor.mat <- cor(mtcars.reduced)

cor.mat

## visualize correlations with the 'pairs' function

pairs(mtcars.reduced)

## run a correlation test- with 95% confidence interval
    # default is Pearson product-moment correlation.

cor.test(mtcars$disp,mtcars$wt)

## now try a non-parametric version

cor.test(mtcars$disp,mtcars$wt, method = "kendall") # or spearman



########
# load the car package

library(car)

#######
# load the trees dataset

data(trees)

######
# visualize relationships among predictor vars

pairs(trees[,c("Girth","Height")])


######
# check for correlation in predictor variables

cor(trees[,c("Girth","Height")])   # predictor variables not highly correlated

# run a multiple regression model

my.mod <- lm(Volume~Girth+Height,data=trees)


# check variance inflation factors

car::vif(my.mod)


########
## mtcars multicollinearity example

mymod <- lm(mpg~disp+hp+wt,data=mtcars) 

vif(mymod)


cor(mtcars.reduced)

## let's remove the 'disp' variable and keep weight...

mymod <- lm(mpg~hp+wt,data=mtcars) 

vif(mymod)


# ?nls

data(DNase)

plot(DNase$density~DNase$conc)   # looks non-linear!

### run linear regression model

model1 <- lm(density~conc,data=DNase)

### run diagnostic plots

par(mfrow=c(2,2))
plot(model1)     # clear non-linearity (and non-normal residuals, and heteroskedasticity!)

par(mfrow=c(1,1))      # plot data with regression line - obvious issues!
plot(DNase$density~DNase$conc)
abline(model1)

### run non-linear regression model - use saturation curve

model2 <- nls(density ~ (max*conc)/(K+conc),data=DNase,start=list(max=2,K=1))
summary(model2)

### run non-linear regression model - use logistic function 

model3 <- nls(density ~ Asym/(1 + exp((xmid - log(conc))/scal)),
                 data = DNase,
                 start = list(Asym = 3, xmid = 0, scal = 1))

summary(model3)

plot(DNase$density~DNase$conc)
abline(model1)
concs.toplot <- seq(0.01,14,0.1)
densities.toplot <- predict(model2,newdata=data.frame(conc=concs.toplot))
lines(concs.toplot,densities.toplot,col="blue")
densities.toplot <- predict(model3,newdata=data.frame(conc=concs.toplot))
lines(concs.toplot,densities.toplot,col="red")

legend("topleft",lty=c(1,1,1),col=c("black","blue","red"),legend=c("linear",
                  "saturation","logistic"))



## residual plots

resids <- DNase$density-predict(model3)

residuals(model3)   # alternative method! This works for 'lm' objects as well (and many other model objects)

## check for normality

qqnorm(resids)
shapiro.test(resids)


## check for heteroskedasticity
plot(resids~predict(model3))

##########
# heteroskedasticity

##### first, simulate data with heteroskedastic residuals

simulated.x <- runif(100,0.1,5)
simulated.y <- exp(rnorm(100,1.1+0.3*simulated.x,0.7))

plot(simulated.y~simulated.x)

### run linear model

model1 <- lm(simulated.y~simulated.x)
par(mfrow=c(2,2))
plot(model1)  # run diagnostic plots    heteroskedasticity issues


### Take a minute to test for normality of residuals!

### run linear model with log transformation of response variable

model2 <- lm(log(simulated.y)~simulated.x)
par(mfrow=c(2,2))
plot(model2)  # run diagnostic plots - no issues!  


#### ANOVA as regression example

data(iris)    # load the iris dataset

plot(iris$Sepal.Length ~ iris$Species)   # r uses a boxplot by default for categorical predictor

my.mod <- lm(Sepal.Length~Species,data=iris)    # run an ANOVA!
summary(my.mod)    # look at the results

anova(my.mod)     # produce an analysis of variance table

####
# alternative!

my.mod <- aov(Sepal.Length~Species,data=iris)   # same model!!
summary(my.mod)     # but produces an anova table by default



