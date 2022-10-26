##### regresssion stuff


data("mtcars")

names(mtcars)

summary(mtcars)

pairs(mtcars)

lapply(names(mtcars),function(t) hist(mtcars[[t]],main=t))


fullmod <- lm(mpg~.,data=mtcars)
summary(fullmod)

car::vif(fullmod)

allvars <- names(mtcars)
response <- "mpg"
predvars <- setdiff(allvars,"mpg")

fullformula <- as.formula(paste(response, "~", paste(predvars,collapse="+") ))

fullmod <- lm(fullformula,data=mtcars)

summary(fullmod)

coplot(mpg~cyl | disp,data=mtcars,columns = 6, panel=panel.smooth)

