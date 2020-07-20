
############################################################
####                                                    ####  
####  NRES 746, Lecture 2                               ####
####                                                    ####
####  Kevin Shoemaker                                   #### 
####  University of Nevada, Reno                        ####
####                                                    #### 
############################################################


############################################################
####  Working with Probabilities                        ####
############################################################



#########
# Classic Urn Example

n_red <- 104
n_blue <- 55
n_green <- 30

allSpheres <- c(n_red,n_blue,n_green)           # create vector of urn contents
names(allSpheres) <- c("red","blue","green")    # make it a named vector, for convenience!


P_blue <- allSpheres["blue"]/sum(allSpheres)     # probability of drawing a blue sphere
P_blue


Prob <- allSpheres/sum(allSpheres)    # probability of drawing each type of sphere
Prob


as.numeric( Prob["blue"] + Prob["red"] )     # probability of drawing a blue or red sphere


as.numeric( Prob["blue"] + Prob["red"] + Prob["green"] )      # P(blue OR green)


#### Question: What is the probability of drawing a blue **AND THEN** a red sphere? 

#[your command here]    # P(blue AND THEN red)


as.numeric( (Prob["blue"] * Prob["red"]) + (Prob["red"] * Prob["blue"]) )    # P(blue then red OR red then blue)


##########
# Urn example #2

n_red_sphere <- 39       # contents of new urn
n_blue_sphere <- 76
n_red_cube <- 101
n_blue_cube <- 25

allSpheres <- c(n_red_sphere,n_blue_sphere)         # build up matrix from vectors
allCubes <- c(n_red_cube,n_blue_cube)
allTypes <- c(allSpheres,allCubes)     
allTypes <- matrix(allTypes,nrow=2,ncol=2,byrow=T)     # matrix of urn contents
rownames(allTypes) <- c("sphere","cube")               # name rows and columns
colnames(allTypes) <- c("red","blue")
allTypes

Prob_Shape <- apply(allTypes,1,sum)/sum(allTypes)  # marginal probabilities of shape
Prob_Shape

Prob_Color <- apply(allTypes,2,sum)/sum(allTypes)    # marginal probabilities of color
Prob_Color

allprobs <- (allTypes/sum(allTypes))
allprobs


Prob_Color["red"]      # marginal probability of drawing a red object


as.numeric( Prob_Color["blue"] * Prob_Shape["cube"])      # joint probability of drawing a blue object that is a cube

## NOTE: if the above answer is not correct, please correct it!  And would I be asking this if it were correct? Hmmm...


as.numeric( Prob_Color["blue"] + Prob_Shape["cube"])        # probability of drawing something blue or something cube-shaped...

## NOTE: if the above answer is not correct, please correct it!  



allprobs["cube","blue"] / Prob_Shape["cube"]   # probability of drawing a blue object, given it is a cube


as.numeric( (allprobs["cube","blue"] / Prob_Color["blue"]) * Prob_Color["blue"])   # probability of drawing a blue cube... using conditional probabilities

allprobs["cube","blue"]   # check answer to make sure it's right


# unconditional probability of drawing a blue item..  Seems too complicated, but this method of computing unconditional probabilities will prove useful when getting into Bayesian statistics!
uncond_prob_blue <- (allprobs["cube","blue"] /  Prob_Shape["cube"]) * Prob_Shape["cube"] + 
              (allprobs["sphere","blue"] / Prob_Shape["sphere"]) * Prob_Shape["sphere"]       

as.numeric(uncond_prob_blue)

Prob_Color["blue"]   # check to make sure we get the right answer!


##########
# Bolker medical example

Prob_Disease <- c(1,999999)     # disease prevalence 
Prob_Disease <- Prob_Disease/sum(Prob_Disease)      # probability of disease
names(Prob_Disease) <- c("yes","no")                # make it a named vector!
Prob_Disease


### compute the unconditional probability of testing positive

as.numeric( 1*Prob_Disease["yes"] + 0.01*Prob_Disease["no"] )    # Prob(+test|Disease)*Prob(Disease) + Prob(+test|no Disease)*Prob(no Disease)


##### Monty Hall simulation code (code by Corey Chivers 2012)
#####################################################
# Simulation of the Monty Hall Problem
# Demonstrates that switching is always better
# than staying with your initial guess
#
# Corey Chivers, 2012
#####################################################
 
monty<-function(strat='stay',N=1000,print_games=TRUE){
  doors<-1:3 #initialize the doors behind one of which is a good prize
  win<-0 #to keep track of number of wins
  
  for(i in 1:N){
    prize<-floor(runif(1,1,4)) #randomize which door has the good prize
    guess<-floor(runif(1,1,4)) #guess a door at random
    
    ## Reveal one of the doors you didn't pick which has a bum prize
    if(prize!=guess)
      reveal<-doors[-c(prize,guess)]
    else
      reveal<-sample(doors[-c(prize,guess)],1)
    
    ## Stay with your initial guess or switch
    if(strat=='switch')
      select<-doors[-c(reveal,guess)]
    if(strat=='stay')
      select<-guess
    if(strat=='random')
      select<-sample(doors[-reveal],1)
    
    ## Count up your wins
    if(select==prize){
      win<-win+1
      outcome<-'Winner!'
    }else
      outcome<-'Loser!'
    
    if(print_games)
      cat(paste('Guess: ',guess,
          '\nRevealed: ',reveal,
          '\nSelection: ',select,
          '\nPrize door: ',prize,
          '\n',outcome,'\n\n',sep=''))
  }
  cat(paste('Using the ',strat,' strategy, your win percentage was ',win/N*100,'%\n',sep='')) #Print the win percentage of your strategy
}


###########
# run the monty hall code!

monty(strat="stay",print_games=FALSE)


###########
# run the monty hall code!

monty(strat="switch",print_games=FALSE)


#################
# Probability distributions

mean <- 5
rpois(10,mean)    # the random numbers have no decimal component

             # plot a discrete distribution!
xvals <- seq(0,15,1)
probs <- dpois(xvals,lambda=mean)
names(probs) <- xvals
               
barplot(probs,ylab="Probability",main="Poisson distribution (discrete)")

barplot(cumsum(probs),ylab="Cumulative Probability",main="Poisson distribution (discrete)")   # cumulative distribution

sum(probs)   # just to make sure it sums to 1!  Does it??? 


#########
# continuous distributions

shape1 = 0.5
shape2 = 0.5

rbeta(10,shape1,shape2)

curve(dbeta(x,shape1,shape2))   # probability density

curve(pbeta(x,shape1,shape2))   # cumulative distribution

integrate(f=dbeta,lower=0,upper=1,shape1=shape1,shape2=shape2)    # just to make sure it integrates to 1!!


##########
# Binomial

size <- 10
prob <- 0.3
rbinom(10,size,prob)

xvals <- seq(0,size,1)
probs <- dbinom(xvals,size,prob)
names(probs) <- xvals
               
barplot(probs,ylab="Probability",main="Binomial distribution")

barplot(cumsum(probs),ylab="Cumulative Probability",main="Binomial distribution")   # cumulative distribution

sum(probs)   # just to make sure it sums to 1!  Does it???


#########
# Gaussian

mean = 7.1
stdev = 1.9

rnorm(10,mean,stdev)

curve(dnorm(x,mean,stdev),0,15)   # probability density

curve(pnorm(x,mean,stdev),0,15)   # cumulative distribution

integrate(f=dnorm,lower=-Inf,upper=Inf,mean=mean,sd=stdev)    # just to make sure it integrates to 1!!

