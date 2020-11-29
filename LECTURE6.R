
############################################################
####                                                    ####  
####  NRES 710, Lecture 6                               ####
####                                                    ####
####  Kevin Shoemaker and Ben Sullivan                  #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Multivariate analysis                             ####
############################################################



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

model <- glm(response~predictor,family=binomial(link="logit"))    # logistic regression in R
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

plot(newdat$predictor,mypred$fit,col="blue",type="l",ylab="mean response(logit scale)",xlab="predictor")
lines(newdat$predictor,mypred$fit+2*mypred$se.fit,col="blue",lty=2)
lines(newdat$predictor,mypred$fit-2*mypred$se.fit,col="blue",lty=2)


mypred <- predict(model,type="response",se.fit=T,newdata = newdat)

plot(newdat$predictor,mypred$fit,col="blue",type="l",ylab="mean response",xlab="predictor")
lines(newdat$predictor,mypred$fit+2*mypred$se.fit,col="blue",lty=2)
lines(newdat$predictor,mypred$fit-2*mypred$se.fit,col="blue",lty=2)



# Count regression example

predictor = runif(30,-2,2)
response = rnbinom(30,mu=exp(3-0.5*predictor),size=2)

plot(response~predictor)
abline(lm(response~predictor))

par(mfrow=c(2,2))
plot(lm(response~predictor))


## try Poisson count regression model!

model <- glm(response~predictor,family=poisson(link="log"))
summary(model)

plot(response~predictor)

newdat <- data.frame(
  predictor = seq(-3,3,0.1)
)

mypred <- predict(model,type="response",se.fit = T,newdata=newdat)

lines(newdat$predictor,mypred$fit,col="blue")
lines(newdat$predictor,mypred$fit+2*mypred$se.fit,col="blue",lty=2)
lines(newdat$predictor,mypred$fit-2*mypred$se.fit,col="blue",lty=2)


residuals(model)  # compute the deviance residuals for the poisson regression model

summary(residuals(model))   # median should be near zero

paste0(c("Null deviance: ", "Residual deviance: "),     # null deviance should be much higher than residual deviance
       round(c(model$null.deviance, deviance(model)), 2))

paste0(c("model df: ", "Residual deviance: "),     # resid deviance should be close to residual df
       round(c(model$df.residual, deviance(model)), 2))


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


simresids <- simulateResiduals(model,n=250,plot=T)   # looks a lot better!

testResiduals(simresids)  # run tests on the residuals!


######
# Make up data!

predictor1 = runif(30,-2,2)
predictor2 <- runif(30,-100,100)
predictor3 <- rnorm(30)   # useless predictor
response = rnbinom(30,mu=exp(3-0.5*predictor1+0.01*predictor2),size=2)


###
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

AICtab$DeltaAIC <- abs(AICtab$AIC-min(AICtab$AIC))

AICtab[order(AICtab$DeltaAIC,decreasing = F),]

