
#  NRES 710, Lecture 10 
#  University of Nevada, Reno 

#  Machine learning --------------------------                               


# Titanic disaster example  (load data) ----------------

titanic <- read.csv("titanic.csv",header=T)
head(titanic)

# library(titanic)            # alternative: load titanic data from package
# titanic <- titanic_train


# Load packages -------------------

library(ranger)    # fast implementation of random forest
library(party)     # good package for running decision tree analysis (and random forest- just slower)


# process data ----------------

class(titanic$Survived)

titanic$Survived <- as.factor(titanic$Survived)    # code response variable as a factor variable (categorical)
titanic$Sex <- as.factor(titanic$Sex)

class(titanic$Survived)   # make sure it's a factor


predictorNames <- c(  "Sex",       # nice readable names
                      "Age",
                      "Sibs/spouses",
                      "Parents/children",
                      "Fare"
)

pred.names=c(  "Sex",      # the actual names from the data frame
               "Age",
               "SibSp",
               "Parch",
               "Fare"
)
# cbind(pred.names,predictorNames)

response="Survived"


formula1 <- as.formula(paste(response,"~",paste(pred.names,collapse="+")))    # formula for the RF model


# Fit a single decision tree -------------

TerrMamm.tr <- ctree(formula=formula1, data=titanic, controls = ctree_control(mincriterion = 0.85,maxdepth = 3))

plot(TerrMamm.tr)

# Run a random forest model!  ---------------------

titanic2 <- na.omit(titanic)   # remove missing data (ranger does not handle missing data- 'party' implementation of RF does...)

thismod <- ranger(formula1, data=titanic2, probability=T,importance = 'permutation', max.depth = 2, mtry = 3, sample.fraction=0.4, num.trees = 1500 )


    # get the importance values
varimp <- importance(thismod)


lengthndx <- length(varimp)
par(mai=c(1.2,2.4,0.6,0.9))
col <- rainbow(lengthndx, start = 3/6, end = 4/6)      
barplot(height=varimp[order(varimp,decreasing = FALSE)],
        horiz=T,las=1,main="Order of Importance of Predictor Variables",
        xlab="Index of overall importance",col=col,           
        names.arg=predictorNames[match(names(varimp),pred.names)][order(varimp,decreasing = FALSE)])


library(caret)

# perform feature selection --------------

# first add some random features!

titanic2$random1 <- as.factor(sample(c("A","B","C"),replace=T, size=nrow(titanic2)))

titanic2$random2 <- rnorm(nrow(titanic2))

varstotry <- c(pred.names,"random1","random2")
varstotry2 <- c(predictorNames,"random1","random2")
response <- "Survived"

form = as.formula(paste0(response, "~", paste(varstotry,collapse = "+")))

thismod2 <- ranger(form, data=titanic2, probability=T,importance = 'permutation', max.depth = 2, mtry = 3, sample.fraction=0.4, num.trees = 1500 )

    # get the importance values
varimp2 <- importance(thismod2)


lengthndx <- length(varimp)
par(mai=c(1.2,2.4,0.6,0.9))
col <- rainbow(lengthndx, start = 3/6, end = 4/6)      
barplot(height=varimp2[order(varimp2,decreasing = FALSE)],
        horiz=T,las=1,main="Order of Importance of Predictor Variables",
        xlab="Index of overall importance",col=col,           
        names.arg=varstotry2[match(names(varimp2),varstotry)][order(varimp2,decreasing = FALSE)])


# recursive feature elimination (rfe) -------------------


# read in function for using RFE with ranger

rangerFuncs <-  list(summary = defaultSummary,
                     fit = function(x, y, first, last, ...) {
                       loadNamespace("ranger")
                       dat <- if(is.data.frame(x)) 
                         x else as.data.frame(x)
                       dat$.outcome <- y
                       ranger::ranger(.outcome ~ ., data = dat, 
                                      importance = "permutation", 
                                      probability = is.factor(y),
                                      write.forest = TRUE,
                                      ...)
                     },
                     pred = function(object, x)  {
                       if(!is.data.frame(x)) x <- as.data.frame(x)
                       out <- predict(object, x)$predictions
                       if(object$treetype == "Probability estimation") {
                         out <- cbind(pred = colnames(out)[apply(out, 1, which.max)],
                                      out)
                       } 
                       out
                     },
                     rank = function(object, x, y) {
                       if(length(object$variable.importance) == 0)
                         stop("No importance values available")
                       imps <- ranger:::importance(object)
                       vimp <- data.frame(Overall = as.vector(imps),
                                         var = names(imps))
                       rownames(vimp) <- names(imps)
                       
                       vimp <- vimp[order(vimp$Overall, decreasing = TRUE),, drop = FALSE]
                       vimp
                     },
                     selectSize = pickSizeBest,
                     selectVar = pickVars)


# define the settings for recursive feature elimination 
reps = 10
#folds <- lapply(1:reps,function(t) createFolds(titanic2$Survived,k=10))   # changed to leave out grids

control <- caret::rfeControl(functions=rangerFuncs, method="repeatedcv", 
                             repeats=reps, #index = folds, 
                             rerank=T,allowParallel = TRUE)   # number=5,

# run the RFE algorithm (takes a while even with reduced dataset)

results <- caret::rfe(x=titanic2[,varstotry], y=titanic2[,response], 
                      sizes=c(1:length(varstotry)), 
                      rfeControl=control, max.depth = 2, #mtry = 3, 
                      sample.fraction=0.4, num.trees = 1500 )

# list the chosen features

predictors(results)      

# plot the results

plot(results, type=c("g", "o"))    # visualize number of important features

bestvars <- results$optVariables
bestvars

covars_new2 <- bestvars   #results$optVariables[1:7]

formula <- as.formula(paste0(response,"~",paste(covars_new2,collapse = "+")))   # formula with further reduced covars


# Make univariate 'effects' plots -------------

varstoplot <- names(sort(varimp,decreasing = T))   # plot in order of decreasing importance
par(mai=c(1,1,.8,.1))
p=1
for(p in 1:length(pred.names)){            # loop through all predictor variables
  thisvar <- varstoplot[p]    
  
  if(is.factor(titanic2[[thisvar]])){                    # make 'newdata' that spans the range of the predictor variable
    nd <- data.frame(x=as.factor(levels(titanic2[[thisvar]])))
  }else{
    nd <- data.frame(x=seq(min(titanic2[[thisvar]]),max(titanic2[[thisvar]]),length=50))
  }
  names(nd) <- thisvar
  
  othervars <- setdiff(pred.names,thisvar)    # set other variables at their mean value (or for factors, use first observation- I was lazy here)
  temp <- sapply(othervars,function(t){ if(is.factor(titanic2[[t]])){ nd[[t]] <<- titanic2[[t]][1]}else{ nd[[t]] <<- mean(titanic2[[t]]) }} )
  #nd
  
  pred = predict(thismod,data=nd,type="response")$predictions[,2]    # use 'predict' function to make predictions across the param of interest
  
  plot(pred~nd[,1],type="l",xlab=thisvar,main=thisvar)         # plot the predictions
  if(!is.factor(nd[,1])) rug(jitter(titanic2[[thisvar]]))   # with 'rug' for quantitative vars
  
}

# Identify and characterize interactions ----------------

allcomb <- as.data.frame(t(combn(pred.names,2)))    # all bivariate combinations of the predictor variables 
names(allcomb) <- c("var1","var2")

allcomb$int1 <- NA    # for storing relative interaction intensity
allcomb$int2 <- NA

p=1
for(p in 1:nrow(allcomb)){     # loop through all bivariate combinations
  var1 = allcomb$var1[p]
  var2 = allcomb$var2[p]

  if(!is.factor(titanic2[[var1]])){        # break each variable into bins for making predictions
    all1= seq(min(titanic2[[var1]]),max(titanic2[[var1]]),length=10)
  }else{
    all1=as.factor(levels(titanic2[[var1]]))
  }
  if(!is.factor(titanic2[[var2]])){ 
    all2 = seq(min(titanic2[[var2]]),max(titanic2[[var2]]),length=10)
  }else{
    all2=as.factor(levels(titanic2[[var2]]))
  }

  nd <- expand.grid(all1,all2)     # make 'newdata' data frame for making predictions across the 2-D parameter space
  names(nd) <- c(var1,var2)
  
  othervars <- setdiff(pred.names,c(var1,var2))      # set all other vars at their mean value (or use first obs for factors-I was lazy)
  temp <- sapply(othervars,function(t){ if(is.factor(titanic2[[t]])){ nd[[t]] <<- titanic2[[t]][1]}else{ nd[[t]] <<- mean(titanic2[[t]]) }}   )
  
  pred = predict(thismod,data=nd,type="response")$predictions[,2]   # make predictions using 'predict()'
  
  additive_model <- lm(pred~nd[[var1]]+nd[[var2]])     # fit a fully additive model from RF predictions using 'lm()'
  
  pred_add = predict(additive_model)    # generate predictions using the additive model (for comparison with RF predictions)
  
  allcomb$int1[p] <- sqrt(mean((pred-pred_add)^2))   # metric of interaction strength (dif between RF and additive model)
  
  maximp <- mean(varimp[c(var1,var2)])    # for weighted interaction importance
  
  allcomb$int2[p] <- allcomb$int1[p]/maximp   # weighted measure that includes overall importance and interaction strength
  
}

allcomb <- allcomb[order(allcomb$int1,decreasing = T),]
allcomb



### visualize interactions
ints.torun <- 1:3
int=2
for(int in 1:length(ints.torun)){
  thisint <- ints.torun[int]
  var1 = allcomb$var1[thisint]
  var2 = allcomb$var2[thisint]
 
  if(!is.factor(titanic2[[var1]])){ 
    all1= seq(min(titanic2[[var1]]),max(titanic2[[var1]]),length=10)
  }else{
    all1=as.factor(levels(titanic2[[var1]]))
  }
  if(!is.factor(titanic2[[var2]])){ 
    all2 = seq(min(titanic2[[var2]]),max(titanic2[[var2]]),length=10)
  }else{
    all2=as.factor(levels(titanic2[[var2]]))
  }
  
  nd <- expand.grid(all1,all2)
  names(nd) <- c(var1,var2)
  
  othervars <- setdiff(pred.names,c(var1,var2))
  temp <- sapply(othervars,function(t)if(is.factor(titanic2[[t]])){ nd[[t]] <<- titanic2[[t]][1]}else{ nd[[t]] <<- mean(titanic2[[t]]) }  )
  
  pred = predict(thismod,data=nd,type="response")$predictions[,2]
  
  predmat = matrix(pred,nrow=length(all1),ncol=length(all2))
  
  if(!is.factor(titanic2[[var1]])){
    persp(all1,all2,predmat,theta=25,phi=25,xlab=var1,ylab=var2,zlab="prob surv")
  }else{
    plot(predmat[1,]~all2,xlab=var2,ylab="prob surv",type="l",ylim=c(0,1),col="green",lwd=2)
    lines(all2,predmat[2,],col="blue",lwd=2)
    legend("bottomright",bty="n",lty=c(1,1),col=c("green","blue"),lwd=c(2,2),legend=c("Female","Male"))
  }
  
}



# CROSS VALIDATION -------------------------

n.folds = 10       # set the number of "folds"
foldVector = rep(c(1:n.folds),times=floor(length(titanic2$Survived)/9))[1:length(titanic2$Survived)]

counter = 1
CV_df <- data.frame(
  CVprediction = numeric(nrow(titanic2)), # make a data frame for storage
  realprediction = 0,
  realdata = 0
)
i=1
for(i in 1:n.folds){
  fit_ndx <- which(foldVector!=i)
  validate_ndx <- which(foldVector==i)
  model <- ranger(formula1, data = titanic2[fit_ndx,],probability=T,importance = 'permutation') 
  CV_df$CVprediction[validate_ndx]  <- predict(model,data=titanic2[validate_ndx,],type="response")$predictions[,2] 
  CV_df$realprediction[validate_ndx]  <-  predict(thismod,data=titanic2[validate_ndx,],type="response")$predictions[,2]
  CV_df$realdata[validate_ndx] <- titanic2$Survived[validate_ndx]
}

fact=TRUE
if(fact){
  CV_df$realdata=CV_df$realdata-1
}

CV_RMSE = sqrt(mean((CV_df$realdata - CV_df$CVprediction)^2))       # root mean squared error for holdout samples in 10-fold cross-validation
real_RMSE = sqrt(mean((CV_df$realdata - CV_df$realprediction)^2))  # root mean squared error for residuals from final model

# print RMSE statistics

cat("The RMSE for the model under cross-validation is: ", CV_RMSE, "\n")

cat("The RMSE for the model using all data for training is: ", real_RMSE, "\n")


library(ROCR)
library(rms)

par(mfrow=c(2,1))
pred <- prediction(CV_df$CVprediction,CV_df$realdata)     # for holdout samples in cross-validation
perf <- performance(pred,"tpr","fpr")
auc <- performance(pred,"auc")
plot(perf, main="Cross-validation")
text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))

pred <- prediction(CV_df$realprediction,CV_df$realdata)     # for final model
perf <- performance(pred,"tpr","fpr")
auc <- performance(pred,"auc")
plot(perf, main="All data")
text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))



CV_df$CVprediction[which(CV_df$CVprediction==1)] <- 0.9999       # ensure that all predictions are not exactly 0 or 1
CV_df$CVprediction[which(CV_df$CVprediction==0)] <- 0.0001
CV_df$realprediction[which(CV_df$realprediction==1)] <- 0.9999
CV_df$realprediction[which(CV_df$realprediction==0)] <- 0.0001

fit_deviance_CV <- mean(-2*(dbinom(CV_df$realdata,1,CV_df$CVprediction,log=T)-dbinom(CV_df$realdata,1,CV_df$realdata,log=T)))
fit_deviance_real <- mean(-2*(dbinom(CV_df$realdata,1,CV_df$realprediction,log=T)-dbinom(CV_df$realdata,1,CV_df$realdata,log=T)))
null_deviance <- mean(-2*(dbinom(CV_df$realdata,1,mean(CV_df$realdata),log=T)-dbinom(CV_df$realdata,1,CV_df$realdata,log=T)))
deviance_explained_CV <- (null_deviance-fit_deviance_CV)/null_deviance   # based on holdout samples
deviance_explained_real <- (null_deviance-fit_deviance_real)/null_deviance   # based on full model...

# print RMSE statistics

cat("The McFadden R2 for the model under cross-validation is: ", deviance_explained_CV, "\n")

cat("The McFadden R2 for the model using all data for training is: ", deviance_explained_real, "\n")
 



