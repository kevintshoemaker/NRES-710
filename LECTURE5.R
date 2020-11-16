
############################################################
####                                                    ####  
####  NRES 710, Lecture 5                               ####
####                                                    ####
####  Kevin Shoemaker and Ben Sullivan                  #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  ANOVA                                             ####
############################################################



#######
# Simple one-way ANOVA example

F1 <- c(1,2,2,3)     # plant height under fertilizer treatment 1
F2 <- c(5,6,5,4)
F3 <- c(2,1,2,2)

# combine into single dataframe for easier visualization and analysis

df <- data.frame(
  Height = c(F1,F2,F3),
  Treatment = rep(c("F1","F2","F3"),each=length(F1)),
  stringsAsFactors = T
)

plot(Height~Treatment, data=df)



grand.mean <- mean(df$Height)   # grand mean

group.means <- by(df$Height,df$Treatment,mean)    # group means

n.groups <- length(group.means)   # number of groups

group.sample.size <- by(df$Height,df$Treatment,length)

sample.size <- nrow(df)

explained.var <- sum(group.sample.size*(group.means-grand.mean)^2/(n.groups-1))

groups <- lapply(1:n.groups,function(t) df$Height[df$Treatment==levels(df$Treatment)[t]])

residual.var <- sapply(1:n.groups,function(t) (groups[[t]]-group.means[t])^2/(sample.size-n.groups) )

unexplained.var <- sum(residual.var)

#######
# now we can compute the F statistic!

Fstat <- explained.var/unexplained.var
Fstat


#######
# define degrees of freedom

df1 <- n.groups-1
df2 <- sample.size-n.groups

#######
# visualize the sampling distribution under null hypothesis

curve(df(x,df1,df2),0,10)


######
# compute critical value of F statistic

Fcrit <- qf(0.95,df1,df2)
Fcrit

######
# compute p-value

pval <- 1-pf(Fstat,df1,df2)


#####
# use aov function

model1 <- aov(Height~Treatment,data=df)
summary(model1)

#####
# use lm function

model1 <- lm(Height~Treatment,data=df)
summary(model1)

anova(model1)



######
# Tukey's test

# find critical q-value for tukey test
q.value <- qtukey(p=0.95,nmeans=n.groups,df=(sample.size-n.groups))

# find honestly significant difference
tukey.hsd <- q.value * sqrt(unexplained.var/(sample.size/n.groups))

# if differences in group means are greater than this value then we can reject the null!

## let's look at the difference between means

all_means <- tapply(df$Height,df$Treatment,mean)
all_levels <- levels(df$Treatment)
pair_totry <- matrix(c(1,2,1,3,2,3),nrow=3,byrow = T)
pair_totry     # these are the pairwise comparisons to make!

thispair <- pair_totry[1,]    # run first pairwise comparison

dif.between.means <- all_means[thispair[1]]-all_means[thispair[2]]
dif.between.means   # since this is greater than tukey.hsd, we already know we can reject the null

### compute p-value!
sample.size.pergroup <- sample.size/n.groups
std.err <- sqrt(unexplained.var / 2 * (2 / sample.size.pergroup))

 # first compute q statistic
q.stat <- abs(dif.between.means)/std.err
p.val <- 1-ptukey(q.stat,nmeans=n.groups,df=(sample.size-n.groups))
p.val

## run all pairwise comparisons

results <- NULL
i=1
for(i in 1:nrow(pair_totry)){
  thispair <- pair_totry[i,]
  temp <- data.frame(
    group1 = all_levels[thispair[1]],
    group1 = all_levels[thispair[2]]
  )
  
  temp$dif = all_means[thispair[1]]-all_means[thispair[2]]
  temp$qstat = abs(temp$dif)/std.err
  temp$pval = 1-ptukey(temp$qstat,nmeans=n.groups,df=(sample.size-n.groups))
  
  results <- rbind(results,temp)
}

results


########  compare with R's built in tukey test function
model1 <- aov(Height~Treatment,data=df)
TukeyHSD(model1)


#######  and finally, compare with 'emmeans'

library(emmeans)
model1 <- lm(Height~Treatment,data=df)
emm <- emmeans(model1,specs=c("Treatment"))  # compute the treatment means with 'emmeans'
pairs(emm)    # run tukey test!


library(agricolae)
data("PlantGrowth")

plant.lm <- lm(weight ~ group, data = PlantGrowth)   #run the 'regression' model
plant.av <- aov(plant.lm)  # run anova test and print anova table
plant.av

#######
# evaluate goodness of fit (assumption violations etc)
layout(matrix(1:4,nrow=2,byrow = T))
plot(plant.av)

#######
# run pairwise comparisons

tukeytest <- TukeyHSD(plant.av)
tukeytest

layout(matrix(1,nrow=1,byrow = T))
plot(tukeytest)   #default plotting method for tukey test objects!


######
# alternative method


 # run tukey test
emm <- emmeans(plant.lm,specs=c("group"))  # compute the treatment means with 'emmeans'
pairs(emm)    # run tukey test!

toplot <- as.data.frame(summary(emm))[,c("group","emmean","lower.CL","upper.CL")]
xvals <- barplot(toplot$emmean,names.arg = toplot$group,ylim=c(0,7))
arrows(xvals,toplot$lower.CL,xvals,toplot$upper.CL,angle=90,code=3)
text(xvals,c(6.4,6.4,6.4),labels = c("ab","a","b"),cex=1.5)


