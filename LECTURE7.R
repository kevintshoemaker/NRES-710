
#  NRES 710, Lecture 7 ---------------------------------------                             
#   University of Nevada, Reno                        
#
#    ANOVA                              


# Simple one-way ANOVA example---------------

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

###
# now we can compute the F statistic!

Fstat <- explained.var/unexplained.var
Fstat


###
# define degrees of freedom

df1 <- n.groups-1
df2 <- sample.size-n.groups

###
# visualize the sampling distribution under null hypothesis

curve(df(x,df1,df2),0,10)


###
# compute critical value of F statistic

Fcrit <- qf(0.95,df1,df2)
Fcrit

###
# compute p-value

pval <- 1-pf(Fstat,df1,df2)


###
# use aov function

model1 <- aov(Height~Treatment,data=df)
summary(model1)

###
# use lm function

model1 <- lm(Height~Treatment,data=df)
summary(model1)

anova(model1)



# Pairwise comparison! -----------------

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


###  compare with R's built in tukey test function
model1 <- aov(Height~Treatment,data=df)
TukeyHSD(model1)


###  and finally, compare with 'emmeans'

library(emmeans)
model1 <- lm(Height~Treatment,data=df)
emm <- emmeans(model1,specs=c("Treatment"))  # compute the treatment means with 'emmeans'
pairs(emm)    # run tukey test!


library(agricolae)
data("PlantGrowth")

plant.lm <- lm(weight ~ group, data = PlantGrowth)   #run the 'regression' model
plant.av <- aov(plant.lm)  # run anova test and print anova table
plant.av

###
# evaluate goodness of fit (assumption violations etc)
layout(matrix(1:4,nrow=2,byrow = T))
plot(plant.av)

###
# run pairwise comparisons

tukeytest <- TukeyHSD(plant.av)
tukeytest

layout(matrix(1,nrow=1,byrow = T))
plot(tukeytest)   #default plotting method for tukey test objects!


###
# alternative method

 # run tukey test
emm <- emmeans(plant.lm,specs=c("group"))  # compute the treatment means with 'emmeans'
pairs(emm)    # run tukey test!

toplot <- as.data.frame(summary(emm))[,c("group","emmean","lower.CL","upper.CL")]
xvals <- barplot(toplot$emmean,names.arg = toplot$group,ylim=c(0,7))
arrows(xvals,toplot$lower.CL,xvals,toplot$upper.CL,angle=90,code=3)
text(xvals,c(6.4,6.4,6.4),labels = c("ab","a","b"),cex=1.5)

# note: many statisticians recommend against compact letter displays... 



## two way interaction example

data("ToothGrowth")
summary(ToothGrowth)

table(ToothGrowth$supp,ToothGrowth$dose)   # three doses, two types of supplements

ToothGrowth$dose <- ordered(ToothGrowth$dose)  # convert dose variable to factor (make it categorical)

model <- lm(len~supp+dose,data=ToothGrowth)  # two way anova with no interaction

summary(model)
anova(model)

model_with_interaction <- lm(len~supp*dose,data=ToothGrowth)  # now try again with interactions
summary(model_with_interaction)
anova(model_with_interaction)



# visualize the interaction

# ?interaction.plot     # this base R function can be used to visualize interactions

with(ToothGrowth, {   # the "with" function allows you to only specify the name of the data frame once, and then refer to the columns of the data frame as if they were variables in your main environment
  interaction.plot(dose, supp, len, fixed = TRUE, col = c("red","blue"), leg.bty = "o")
})

# alternatively, use the "effects" package:

plot(effects::Effect(c("dose", "supp"),model_with_interaction))

plot(effects::Effect(c("dose", "supp"),model))


TukeyHSD(aov(model), "dose")   # run Tukey test for the 'dose' variable in the ToothGrowth model
TukeyHSD(aov(model_with_interaction), "dose")   # run Tukey test for the 'dose' variable in the ToothGrowth model

library(emmeans)
emm = emmeans(model_with_interaction, ~dose:supp)
emm
plot(emm,by="dose")

pairs(emm,type="response")   # look at pairwise comparisons

pwpm(emm)    # generate an informative summary matrix of pairwise comparisons!

emmip(model_with_interaction, supp ~ dose)   # interaction plot of emms

# NOTE: you could use compact letter displays but many statisticians prefer to avoid this!



### Kruskal-Wallis example


## read in data:

Input =("
Group      Value
Group.1      1
Group.1      2
Group.1      3
Group.1      4
Group.1      5
Group.1      6
Group.1      7
Group.1      8
Group.1      9
Group.1     46
Group.1     47
Group.1     48
Group.1     49
Group.1     50
Group.1     51
Group.1     52
Group.1     53
Group.1    342
Group.2     10
Group.2     11
Group.2     12
Group.2     13
Group.2     14
Group.2     15
Group.2     16
Group.2     17
Group.2     18
Group.2     37
Group.2     58
Group.2     59
Group.2     60
Group.2     61
Group.2     62
Group.2     63
Group.2     64
Group.2    193
Group.3     19
Group.3     20
Group.3     21
Group.3     22
Group.3     23
Group.3     24
Group.3     25
Group.3     26
Group.3     27
Group.3     28
Group.3     65
Group.3     66
Group.3     67
Group.3     68
Group.3     69
Group.3     70
Group.3     71
Group.3     72
")

Data = read.table(textConnection(Input),header=TRUE)

Data$Group = factor(Data$Group,levels=unique(Data$Group))    # transform predictor variable to factor

#summarize values by group

groups <- unique(Data$Group)
ngroups <- length(groups)
sumry <- sapply(1:ngroups,function(i){temp <- subset(Data,Group==groups[i]); summary(temp$Value)}  )
colnames(sumry) <- groups
sumry


# histograms by group

library(ggplot2)

ggplot(Data, aes(x=Value)) +
  geom_histogram(bins=10,aes(color=Group,fill=Group)) +
  facet_grid(~Group)

# ## first install required packages if needed
# 
# if(!require(dplyr)){install.packages("dplyr")}
# if(!require(FSA)){install.packages("FSA")}
# if(!require(DescTools)){install.packages("DescTools")}
# if(!require(multcompView)){install.packages("multcompView")}


model <- lm(Value~Group,data=Data)

summary(model)   # no treatment effects
anova(model)   # looks a little weird

layout(matrix(1:4,nrow=2,byrow=T))
plot(model)   # notice the outliers! And violation of normality

shapiro.test(residuals(model))



# K-W test

kruskal.test(Value ~ Group, data = Data)   # now there is a significant group effect!


library(FSA)  # make sure you have this package installed!

dt = dunnTest(Value ~ Group,data=Data,method="bh")
dt

