################
# Test hot hand claim:
#      if you take lots and lots of fair coin flips and select all sequences of four trials that 
#       begin with three "successes", then the fourth trial of those selected sequences are more
#       likely to be a failure ("miss") than to be a success



## first simulate a very large number of fair coin flips (trials)
##    in the ESPN paper the example was 100 throws with a chance of 50% each throw
n_trials <- 100
lots_of_trials <- rbinom(n_trials,1,0.5)

## enumerate all groups of four within our large sample
n_groups <- n_trials - 3    # the total number of groups of four
groups_of_four <- lapply(1:n_groups, function(t) lots_of_trials[t:(t+3)]  )

## select only those groups of four that begin with three 'successes'
selected_groups <- groups_of_four[sapply(groups_of_four,function(t) all(t[1:3]==1) )]

## pick out the fourth trial for each of the selected sequences
fourth_trial <- sapply(selected_groups,function(t) t[4])

## determine the total number of hits and misses
results_summary <- table(fourth_trial)
results_summary

## now repeat this experiment multiple times
n_experiments <- 100000
conduct_experiment <- function(){
  lots_of_trials <- rbinom(n_trials,1,0.5)
  groups_of_four <- lapply(1:n_groups, function(t) lots_of_trials[t:(t+3)]  )
  selected_groups <- groups_of_four[sapply(groups_of_four,function(t) all(t[1:3]==1) )]
  if(length(selected_groups)>0){
    fourth_trial <- sapply(selected_groups,function(t) t[4])
    n_heads <- sum(fourth_trial)  # number of heads
    n_selected <- length(fourth_trial)  # number of trials
    return(n_heads/n_selected)  # compute and return the percent of 'successes' among the fourth shots in the sequence
  }else{
    return(NA)  # if no data, return NA
  }
}
#conduct_experiment()

sampling_distribution <- replicate(n_experiments,conduct_experiment())

sampling_distribution <- na.omit(sampling_distribution)

hist(sampling_distribution)

mean(sampling_distribution)  # answer is 46%, just like the ESPN article says


### Are you convinced yet???



