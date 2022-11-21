
#  NRES 710, Lecture 8 ---------------------------------------                             
#   University of Nevada, Reno                        
#
#    GLM                                      

library(MASS)


# logistic regression ----------------------

## made up data for glm #1 (logistic regression)

predictor <- runif(100,0,50)
response <- rbinom(100,1, plogis(-5 + 0.26*predictor) )

plot(response~predictor,ylim=c(-2,2))
abline(lm(response~predictor),col="red")   # overlay regression line

layout(matrix(1:4,nrow=2,byrow=2))
plot(lm(response~predictor))



probs <- runif(10)
probs


data.frame(
  p = probs,
  logit.p=log(probs/(1-probs))
)


  ## conduct logistic regression:

mydat <- data.frame(response=response,predictor=predictor)
model <- glm(response~predictor,family=binomial(link="logit"),data=mydat)    # logistic regression in R
summary(model)   # summary looks similar to ordinary linear regression!

newdat <- data.frame(        # make predictions for plotting regression line and approx conf bounds
  predictor = seq(0,50,1)
)

mypred <- predict(model,type="response",se.fit=T,newdata = newdat)

plot(response~predictor)
lines(newdat$predictor,mypred$fit,col="blue")
lines(newdat$predictor,mypred$fit+2*mypred$se.fit,col="blue",lty=2)
lines(newdat$predictor,mypred$fit-2*mypred$se.fit,col="blue",lty=2)


par(mfcol=c(1,2))

mypred <- predict(model,type="link",se.fit=T,newdata = newdat)

plot(newdat$predictor,mypred$fit,col="blue",type="l",ylab="mean response (logit scale)",xlab="predictor")
lines(newdat$predictor,mypred$fit+2*mypred$se.fit,col="blue",lty=2)
lines(newdat$predictor,mypred$fit-2*mypred$se.fit,col="blue",lty=2)


mypred <- predict(model,type="response",se.fit=T,newdata = newdat)

plot(newdat$predictor,mypred$fit,col="blue",type="l",ylab="mean response",xlab="predictor")
lines(newdat$predictor,mypred$fit+2*mypred$se.fit,col="blue",lty=2)
lines(newdat$predictor,mypred$fit-2*mypred$se.fit,col="blue",lty=2)



 # quantile residuals (GLM diagnostics)

qr <- statmod::qresiduals(model)
qqnorm(qr)
abline(0,1)

plot(qr~predict(model))


## display formula!

library(equatiomatic)

equatiomatic::extract_eq(model,wrap=T,intercept = "beta",show_distribution = T)


# Count regression example ------------------------------

predictor = runif(30,-2,2)
response = rnbinom(30,mu=exp(3-0.5*predictor),size=2)     # make up data!

plot(response~predictor)
abline(lm(response~predictor))

par(mfrow=c(2,2))
plot(lm(response~predictor))


## try Poisson count regression model!

mydat <- mydat <- data.frame(response=response,predictor=predictor)
model <- glm(response~predictor,family=poisson(link="log"),data=mydat)
summary(model)

plot(response~predictor,data=mydat)

newdat <- data.frame(
  predictor = seq(-3,3,0.1)
)

mypred <- predict(model,type="response",se.fit = T,newdata=newdat)

lines(newdat$predictor,mypred$fit,col="blue")
lines(newdat$predictor,mypred$fit+2*mypred$se.fit,col="blue",lty=2)
lines(newdat$predictor,mypred$fit-2*mypred$se.fit,col="blue",lty=2)


## display formula!

library(equatiomatic)

equatiomatic::extract_eq(model,wrap=T,intercept = "beta")


# Demo: heteroskedasticity in Poisson distrubution

library(tidyverse)
library(ggplot2)

thisdat <- sapply(1:15,function(t) rpois(1000,lambda=t) )
thisdat <- thisdat %>% 
  as_tibble(.name_repair = "unique") %>%
  rename_with( ~str_extract(.x,pat="(\\d)+")) %>% 
  pivot_longer(cols=everything(),names_to = "mean", values_to = "value",names_transform = as.numeric)

ggplot(thisdat,aes(x=mean,y=value)) + 
  geom_boxplot(aes(group=mean))


# test for overdispersion:

overdisp_fun <- function(model) {    # function from Ben Bolker...
    rdf <- df.residual(model)
    rp <- residuals(model,type="pearson")
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq/rdf
    pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
    list(chisq=Pearson.chisq,ratio=prat,rdf=round(rdf),p=pval)
}

options(scipen=15)
overdisp_fun(model)



 # quantile residuals (GLM diagnostics)

qr <- statmod::qresiduals(model)
qqnorm(qr)
abline(0,1)

plot(qr~predict(model))



library(DHARMa)

simresids <- simulateResiduals(model,n=250,plot=T)   # clearly this is a bad fit!

plotResiduals(simresids,predictor)   # look for patterns across a predictor variable

testResiduals(simresids)  # run tests on the residuals!


## try NegBinom count regression model!

library(MASS)

## NOTE: in reality you should use glm.nb because you don't know the additional parameter theta!
model <- glm(response~predictor,family=negative.binomial(link="log",theta = 2))
summary(model)

model <- glm.nb(response~predictor)

plot(response~predictor)

newdat <- data.frame(
  predictor = seq(-3,3,0.1)
)

mypred <- predict(model,type="response",se.fit = T,newdata=newdat)

lines(newdat$predictor,mypred$fit,col="blue")
lines(newdat$predictor,mypred$fit+2*mypred$se.fit,col="blue",lty=2)
lines(newdat$predictor,mypred$fit-2*mypred$se.fit,col="blue",lty=2)


# test goodness of fit using DHARMa!

simresids <- simulateResiduals(model,n=250,plot=T)   # looks a lot better!

testResiduals(simresids)  # run tests on the residuals!


# AIC model selection ----------------------

# Make up data!

predictor1 = runif(30,-2,2)
predictor2 <- runif(30,-100,100)
predictor3 <- rnorm(30)   # useless predictor
response = rnbinom(30,mu=exp(3-0.5*predictor1+0.01*predictor2),size=2)

# fit a bunch of candidate models

model.pois.all <- glm(response~predictor1+predictor2+predictor3,family="poisson")
model.nb.all <- glm.nb(response~predictor1+predictor2+predictor3)
model.nb.1 <- glm.nb(response~predictor1)
model.nb.12 <- glm.nb(response~predictor1+predictor2)
model.nb.2 <- glm.nb(response~predictor2)

cand.set <- list(
  Poisson=model.pois.all,
  NegBin_allvars = model.nb.all,
  NegBin_pred1 = model.nb.2,
  NegBin_preds1and2 = model.nb.12,
  NegBin_pred2 = model.nb.2
)

### Make AIC table

AICtab <- data.frame(
  ModelName = names(cand.set),
  LogLikelihood = sapply(cand.set,logLik),
  AIC = sapply(cand.set,AIC)
)

k <- c(3,3,1,2,1)
AICtab$AICc <- AICtab$AIC + (2*k^2+2*k)/(30-k-1)

AICtab$DeltaAICc <- abs(AICtab$AICc-min(AICtab$AICc))

AICtab[order(AICtab$DeltaAICc,decreasing = F),]

