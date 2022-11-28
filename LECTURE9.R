
#  NRES 710, Lecture 9 ---------------------------------------                             
#   University of Nevada, Reno                        
#
#    GLMM                                      



# TUNDRA EXAMPLE ---------------------------

## Read in the data

mc1 <- read.csv("tundra2.csv",sep=",",na.strings=c("-","NA"))

summary(mc1)

table(mc1$Year)  # some years have many observations

table(mc1$Site)  # some sites have many observations

library(ggplot2)

## visualize net ecosystem exchange by year- varying by site

ggplot(mc1,aes(x=Year,y=GS.NEE,colour=Site)) +
    geom_point() +
    geom_smooth(method="lm",alpha=0.3) +
    scale_y_continuous(limits=c(-150,400),oob=scales::squish)



library(lme4)
cmod_lmer <- lmer(GS.NEE ~ cYear + (1+cYear|Site),
                data=mc1, weights=n)


summary(cmod_lmer)


library(glmmTMB)
cmod_glmmTMB <- glmmTMB(GS.NEE ~ cYear + (1+cYear|Site),
                data=mc1,
                weights=n,REML=F)

summary(cmod_glmmTMB)   # note NAs present

# try a different optimizer -still doesn't work...
cmod_glmmTMB2 <- update(cmod_glmmTMB,control=glmmTMBControl(optimizer=optim,
                                       optArgs=list(method="BFGS")))

summary(cmod_glmmTMB2)

# try a different specification- uncorrelated slope and intercept terms:

cmod_glmmTMB3 <- glmmTMB(GS.NEE ~ cYear + (1|Site) + (0+cYear|Site),
                data=mc1,
                weights=n, REML=T)

summary(cmod_glmmTMB3)  # fits- not saying it's a good model!




plot(cmod_lmer,type=c("p","smooth"))
plot(cmod_lmer,sqrt(abs(resid(.)))~fitted(.),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid))))
plot(cmod_lmer,resid(.,type="pearson")~cYear,
                  type=c("p","smooth"))
qqnorm(residuals(cmod_lmer,type="pearson",scaled=T))


library(DHARMa)
resids <- simulateResiduals(cmod_lmer)  # similar results for the two different models
plot(resids)

resids <- simulateResiduals(cmod_glmmTMB3)
plot(resids)
testResiduals(resids)


library(lattice)
dotplot(ranef(cmod_lmer,condVar=TRUE),
          lattice.options=list(layout=c(1,2)))


library(car)
Anova(cmod_lmer)



cmod_lmer2 <- lmer(GS.NEE ~ cYear + (1+cYear|Site),
                data=mc1, weights=n,REML=F)
cmod_lmer3 <- lmer(GS.NEE ~ 1 + (1+cYear|Site),
                data=mc1, weights=n,REML=F)

AIC(cmod_lmer2,cmod_lmer3)


cmod_lmer3 <- lmer(GS.NEE ~ cYear + (1+cYear|Site),
                data=mc1, weights=n,REML=F)
cmod_lmer4 <- lmer(GS.NEE ~ cYear + (1|Site),
                data=mc1, weights=n,REML=F)

anova(cmod_lmer4,cmod_lmer3)
AIC(cmod_lmer3,cmod_lmer4)   # random slope term effect is not an artifact of random chance!



# Compute r-squared for GLMM! -----------------

MuMIn::r.squaredGLMM(cmod_lmer)

MuMIn::r.squaredGLMM(cmod_glmmTMB)


confint(cmod_lmer,parm="beta_",method="Wald")

