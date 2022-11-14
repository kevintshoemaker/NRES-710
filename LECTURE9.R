
#  NRES 710, Lecture 9 ---------------------------------------                             
#   University of Nevada, Reno                        
#
#    GLMM                                      



#########
# TUNDRA EXAMPLE
#########

##### Read in the data

mc1 <- read.csv("tundra2.csv",sep=",",na.strings=c("-","NA"))

summary(mc1)

table(mc1$Year)  # some years have many observations

table(mc1$Site)  # some sites have many observations

library(ggplot2)

## visualize net ecosystem exchange by year- varying by site

ggplot(mc1,aes(x=Year,y=GS.NEE,colour=Site))+geom_point()+
    geom_smooth(method="lm",alpha=0.3)+
    scale_y_continuous(limits=c(-150,400),oob=scales::squish)



library(lme4)
cmod_lmer <- lmer(GS.NEE ~ cYear + (1+cYear|Site),
                data=mc1, weights=n)


summary(cmod_lmer)


library(glmmTMB)
cmod_glmmTMB <- glmmTMB(GS.NEE ~ cYear + (1+cYear|Site),
                data=mc1,
                weights=n)

summary(cmod_glmmTMB)

plot(cmod_lmer,type=c("p","smooth"))
plot(cmod_lmer,sqrt(abs(resid(.)))~fitted(.),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid))))
plot(cmod_lmer,resid(.,type="pearson")~cYear,
                  type=c("p","smooth"))
qqnorm(residuals(cmod_lmer,type="pearson",scaled=T))


library(lattice)
dotplot(ranef(cmod_lmer,condVar=TRUE),
          lattice.options=list(layout=c(1,2)))


library(car)
Anova(cmod_lmer)



confint(cmod_lmer,parm="beta_",method="Wald")

