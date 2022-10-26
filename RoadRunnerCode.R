#########################################################################
####                                                                 ####
####        R Code for In-class Regression Analysis Exercise Setup   ####
####                                                                 ####
####                          Oct. 11, 18, 25, 2021                  ####
####                                                                 ####
####             Peter Weisberg and Kevin Shoemaker                  ####
#########################################################################

### 1. CREATE THE PREDICTOR VARIABLES THAT REPRESENT ROADRUNNER HABITAT

# Elegant approach to simulating correlated predictor variables
library(faux)

set.seed(-1234)

# Create three correlated variables, Steepness, Elevation and Tree Density
# Steeper slopes at lower elevations (oops, this is backwards!)
# Lower tree densities on steeper slopes
# Weak positive correlation between Elevation and Tree Density
# Moderate negative correlation between Slope Steepness and Tree Density
# The algorithm only generates normally distributed variables.

cmat <- c(1, -0.8, -0.6,
          -0.8, 1, 0.3, 
          -0.6, 0.3, 1 
          )

topo <- rnorm_multi(n=1000, mu = c(20, 6500, 300),
                    sd=c(10, 800, 75),
                    r=cmat, 
                    varnames =c("Steep", "Elev", "TreeDens"))
round(cor(topo),3)

# Now create a uniformly distributed variable for "cosine-transformed" aspect
CosAsp <- runif(1000, -1, 1)                
hist(CosAsp)

# Add CosAsp to the data frame with the other 3 variables
topo$CosAsp <- CosAsp #uncorrelated with the other variables (unless by chance)

# Patch Size will be highly correlated with slope aspect
# Larger patches (more continuous tree cover) on cooler slopes
topo$PatchSize <- rnorm_pre(topo$CosAsp, mu=10, sd=5, r=0.85)

# Soil pH will be highly correlated with tree density
# Pine needle decomposition increases soil acidity
topo$SoilpH <- rnorm_pre(topo$TreeDens, mu=0.6, sd=0.2, r=0.9)

# Soil depth will be highly negatively correlated with slope
topo$SoilD <- rnorm_pre(topo$Steep, mu=0.5, sd=0.2, r=-0.9)

cor(topo)

# Canopy Cover will be highly correlated with soil Depth
topo$CanCov <- rnorm_pre(topo$SoilD, mu=31, sd=4, r=0.8)

# Make a copy of this data frame, renamed as "AllData.df"
AllData.df <- data.frame(topo)

# Generage correlation matrix for the full data frame of predictors
round(cor(AllData.df), 2)

# Notice that certain aspects of the correlation structure are indirect
# e.g. soil depth with elevation


######
### 2. GENERATE THE ROADRUNNER ABUNDANCE RESPONSE VARIABLE AS A LINEAR FUNCTION OF THE PREDICTORS
###
###    THIS CAN BE THOUGHT OF AS REPRESENTING THE "TRUE" ROADRUNNER HABITAT SELECTION FUNCTION

# first I know I want to make roadrunner habitat selection a nonlinear function of elevation
# many organisms are distributed in a unimodal function with elevation
# underlying ecophysiological tolerances with temperature often follow this relationship
# I played around with the numbers graphically until I got a relationship that seemed reasonable
# across a reasonable range of elevation values (for the Great Basin)

curve(0.26*x -0.000021*x^2, 4296, 8711)

# then, generate (somewhat arbitrary) relationships with environmental predictor variables

B0 <- -350  # specified at the end to make the abundance values consistently positive
B1 <- -1.8  # slope steepness - makes sense roadrunners would prefer flatter sites given their method of foraging
B2 <- 0     # soil depth - no relationship
B3 <- 0.26  # elevation first order
B4 <- -0.000021 #elevation second order
B5 <- -0.27 # tree density - negative relationship - "open country" was specified in the handout
B6 <- 0     # soil pH - no relationship
B7 <- 110   # CosAsp -note there is an INTERACTION TERM WITH CANOPY COVER
B8 <- 0.35  # PatchSize - positive relationship with tree patch size - larger tree patches mean larger openings?
B9 <- -3.5  # CanCov main effect - interaction term with slope aspect
B10 <- -3.1  # CanCov and CosAsp interaction term coefficient

# setting random number seed because I will build some random error into the calculated Roadrunner abundances
set.seed(-124) 

# here is the calculation of roadrunner abundance!
AllData.df$RR <- B0 + B1*topo$Steep + B2*topo$SoilD + B3*topo$Elev + B4*I(topo$Elev^2) + B5*topo$TreeDens +
  B6*topo$SoilpH + B7*topo$CosAsp + B8*topo$PatchSize + B9*topo$CanCov + B10*topo$CosAsp*topo$CanCov + 20*rnorm(1000)

RR <- AllData.df$RR  #add the abundance variable to existing data frame
head(AllData.df)
summary(AllData.df)

# explore this interaction effect between slope aspect and canopy cover
# remember that every interaction has "two sides" to it
coplot(RR ~ topo$CosAsp | topo$CanCov, panel=panel.smooth,columns = 6)
coplot(RR ~ topo$CanCov | topo$CosAsp, panel=panel.smooth,columns=6)


# What if I was to fit the full model from the full sample of 1000 data points?
# interaction effect between CosAsp and CanCov included
# polynomial term for Elev included

Full.fullsample.lm <- lm(RR ~ Steep + SoilD + Elev + I(Elev^2) + TreeDens + SoilpH + CosAsp * CanCov + PatchSize, data=AllData.df)
summary(Full.fullsample.lm)

library(car)
car::vif(Full.fullsample.lm)  # high inflation factors!

plot(predict(Full.fullsample.lm) ~ AllData.df$RR)
abline(0,1, col="darkgreen")

# What if I was to fit the "true" model from the full sample of 1000 data points?
# Soil pH and Soil Depth not included

True.fullsample.lm <- lm(RR ~ Steep + Elev + I(Elev^2) + TreeDens + CosAsp * CanCov + PatchSize,data=AllData.df)
summary(True.fullsample.lm)

vif(True.fullsample.lm) # high inflation factor for Steep! What to do!

#Let's further explore the potential effect of Steepness on RR

# Calculate the portion of RR abundance that is NOT explained by elevation
RR.given.Elev <- residuals(lm(RR ~ Elev + I(Elev^2),data=AllData.df))

par(mfrow=c(1,2))

plot(RR ~ Elev, ylab = "Roadrunner abundance", xlab = "Elevation (m)",data=AllData.df)
plot(RR.given.Elev ~ Elev, ylab="RR | Elevation", xlab = "Elevation (m)",data=AllData.df)
abline(lm(RR.given.Elev~Elev,data=AllData.df), col="red")

# Partial residual plot: RR | Elevation ~ Steepness
plot(RR ~ Steep, ylab = "Roadrunner abundance", xlab = "Slope Steepness (%)",data=AllData.df)
plot(RR.given.Elev ~ Steep, ylab = "RR | Elevation (m)", xlab = "Slope Steepness (%)",data=AllData.df)
abline(lm(RR.given.Elev~Steep,data=AllData.df), col="red")

summary(lm(RR ~ Steep,data=AllData.df))
summary(lm(RR.given.Elev ~ Steep,data=AllData.df))

par(mfrow=c(1,1))

######
### 3. SAMPLE FROM THE FULL DATASET TO GENERATE A SAMPLE DATA SET TO PROVIDE TO THE CLASS
###    

# randomly select 100 samples!
samp <- sample(nrow(AllData.df), 100)
RR.df <- AllData.df[samp,]

# Move RR to first column, convert to integer data type
# Rename dataframe PJ.RR
PJ.RR <- RR.df[,c(9,1,2,3,4,5,6,7,8)]
PJ.RR$RR <- as.integer(PJ.RR$RR)
head(PJ.RR)

# Write the data to an Excel file to distribute to the class
write.table(PJ.RR, file="PJRoadRunner.csv", sep=",", row.names=F)




######
### 4. BUILD REGRESSION MODELS FROM THE SAMPLE DATASET
###    

# I am going to read in the data you were given
# So that we are looking at the same data set!

PJ.RR <- read.csv("PJRoadRunner.csv")
head(PJ.RR)

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "white", ...)
}

#Explore the data 

pairs(PJ.RR, panel=panel.smooth, diag.panel=panel.hist)
round(cor(PJ.RR),3)

#Start off fitting a full model (but no polynomials or interactions)

full.lm <- (lm(RR ~ ., data=PJ.RR))
summary(full.lm)
plot(predict(full.lm)~PJ.RR$RR)
abline(0,1, col="blue")

par(mfrow=c(2,2))
plot(full.lm)
par(mfrow=c(1,1))

vif(full.lm)

# The model has some issues.
# Each group then went through a model selection process of some sort or another.

# Just for example:
try2.lm <- (lm(RR ~ . - Steep, data=PJ.RR))
vif(try2.lm)
try3.lm <- update(try2.lm, ~ . - CosAsp, data=PJ.RR)
vif(try3.lm)
try4.lm <- update(try3.lm, ~. - SoilD)
vif(try4.lm)
AIC(full.lm, try2.lm, try3.lm, try4.lm)
anova(full.lm, try2.lm, try3.lm, try4.lm)

# imagine you had stumbled upon the "true" interaction relationship
# between canopy cover and slope aspect!

coplot(PJ.RR$RR ~ PJ.RR$CanCov | PJ.RR$CosAsp, panel=panel.smooth,columns=6)

try5.lm <- update(try4.lm, ~. +CosAsp*CanCov)

# imagine you had realized the polynomial relationship with Elevation!
try6.lm <- update(try5.lm, ~. + I(Elev^2))
summary(try6.lm)

# remove the non-significant SoilpH
try7.lm <- update(try6.lm, ~. - SoilpH)
summary(try7.lm)

# Now by some great insight you think to try adding slope steepness back in - 
# Then you would have the true model!
true.lm <- lm(RR ~ I(Elev) + I(Elev^2) + TreeDens + Steep + PatchSize + CosAsp*CanCov, data=PJ.RR)
summary(true.lm)
vif(true.lm)

# you would have to accept a lot of multicollinearity with slope steepness
# would this be appropriate?

# you would also have noticed that with Slope Steepness in the model
# suddenly PatchSize becomes less important ("non-significant")

# let's say you removed PatchSize (since you don't know the "true model")
try8.lm <- lm(RR ~ I(Elev) + I(Elev^2) + TreeDens + Steep + CosAsp*CanCov, data=PJ.RR)
summary(try8.lm)
vif(try8.lm)

AIC(full.lm, try2.lm, try3.lm, try4.lm, try5.lm, try6.lm, try7.lm, try8.lm, true.lm)

# try8.lm and true.lm have comparable support from the data (delta-AIC within 2)
# you might opt for the more parsimonious (one less variable) try8.lm

par(mfrow=c(2,2))
plot(try8.lm)
par(mfrow=c(1,1))
plot(predict(try8.lm) ~ PJ.RR$RR)
abline(0,1, col="red", lwd=3)

library(effects)  #plot these effects!
# with partial residuals!
plot(allEffects(full.lm, partial.residuals=TRUE))

# after removing steepness and aspect
plot(allEffects(try3.lm, partial.residuals=TRUE))

# after including interaction between aspect and canopy cover
plot(allEffects(try5.lm, partial.residuals=TRUE))

# true(theoretical) model
plot(allEffects(true.lm, partial.residuals=TRUE))

plot(Effect(c("CosAsp", "CanCov"), true.lm))

# the model without patch size
plot(allEffects(try8.lm, partial.residuals=TRUE))

######
### 5. AGAINST MY BETTER JUDGMENT I AM GOING TO SHOW YOU A "KITCHEN SINK" APPROACH
###     THE DREDGE FUNCTION IN THE MuMIN LIBRARY FOR MULTI-MODEL INFERENCE
###    "Don't try this at home" (without further training in multi-model inference!)

if(!require(MuMIn)){install.packages("MuMIn")}
library(MuMIn)

#  prevent fitting sub-models to different datasets
options(na.action = "na.fail")

head(PJ.RR)

# first apply "model dredging" to our full model with just additive, linear effects

full.dr <- dredge(full.lm, extra=c("R^2"))
subset(full.dr, delta<4)

# many models have similar support, but slope steepness is in all of them
# the two soil variables are not in any of them
# Patchsize is not in the top models

# then test for model with interactions!
# dredge won't work with more than 31 predictors, mercifully.
# let's just add a few

#full.int.lm <- lm(RR ~ .^2, data=PJ.RR)
full.int.lm <- update(full.lm, ~ . + CosAsp:CanCov + CosAsp:Elev + TreeDens:CosAsp, data=PJ.RR)
full.int.dr <- dredge(full.int.lm, extra=c("R^2"))
subset(full.int.dr, delta<4)

# take out the interactions that weren't important. Add polynomial terms for Elevation, Slope steepness
full.intpoly.lm <- update(full.int.lm, ~ . + CosAsp:CanCov - CosAsp:Elev - TreeDens:CosAsp 
                          - Elev + poly(Elev,2) - Steep + poly(Steep,2), data=PJ.RR)
full.intpoly.dr <- dredge(full.intpoly.lm)
subset(full.intpoly.dr, delta<4)

# did this approach lead us to a "single best" model?
# is the "best supported" model the true model?

# How well did YOU do?!?

# To the Powerpoint...




