#################### Code for Statistical Programming Practical 2 ####################




#################### Names ####################

#
# Runchen Geng (s2332611)
#
# Sulaiman Zaman (s2405379)
#
# Jake Lawler (s2451377)


#################### Github Repository ####################

# https://github.com/RunchenGeng/STAT_PROG_PRACTICAL2


#################### Contributions ####################

#
# Runchen Geng - Questions 5 & 6, comments & copy-editing
#
# Sulaiman Zaman - Strategies 2 & 3 for Questions 1 & 2, Question 3 comments & copy-editing
#
# Jake Lawler - Strategy 1 for Questions 1 & 2, Question 4, comments & copy-editing
#
# Proportion of work performed by each team member roughly equal.




#################### Task Description ####################

# In this task we investigate the probabilities associated with three different strategies 
# towards a probability puzzle called the prisoner problem.
#
# In the prisoner problem, we have 2n prisoners numbered 1 to 2n. We also have a room that contains 2n boxes, again each uniquely numbered 1 to 2n.
# Finally, we have 2n cards each with a unique number 1 to 2n, and these are randomly hidden inside the boxes, one to each box.
#
# The task is for the prisoners to open at most n boxes in an attempt to find the card with their number on it.
# If every prisoner finds their card, then they all go free.
# The prisoners can confer on a strategy before the first person enters the room, but cannot communicate afterwards.
# The room is returned to exactly its original state between attempts.
#
# In the code below, we investigate by simulation the probabilities associated with three strategies
#
# Strategy 1
# Each prisoner first opens the box with their number on it. If that box contains their number, they are successful and the round ends.
# If that box contains a number j that is not their number, they next open box j and look for their card.
# They continue in this process, opening boxes according to the number of the card in the previous box,
# until they either find the card with their number, or they fail: having opened 50 boxes without finding their card
#
# Strategy 2
# This strategy is equivalent to strategy 1, except that each prisoner starts by opening a random box, 
# instead of the box with their number on it.
#
# Strategy 3
# Each prisoner opens at most n boxes completely at random, looking for their card in each one. 
# They stop when they find their card or have opened 50 boxes without success. 




####################  Function to Indicate Success in a Single Round ####################


##### Aim of Section #####

# In this section, we write a function that indicates whether a specific prisoner will succeed, given:
# a particular arrangement of the cards among the boxes,
# their prisoner number, 
# the maximum number of box opening attempts they are permitted, and 
# the strategy they employ.  


##### Function Description #####

# The function takes as input a vector of length 2n describing where the cards are places in the boxes.
# If number 77 is in the first position of the vector, then box one contains card 77, and so on.
# It also takes integer n for the number of box opening attempts allowed per prisoner, 
# an integer k for their prisoner number, and and indicator variable named strategy (1, 2 or 3) 
# for the strategy they employ (see Task Description above).
#
# The function returns a 0 if the prisoner succeeds, and a 1 if they fail.


##### Function Definition #####

indiv_round_failure <- function(cards,n,k,strategy){
  
  # An if statement to determine the selection of the initial box depending on the strategy
  if(strategy == 1){
    # The number of the first box for strategy 1 corresponds to the prisoners number
    box_choice <- k 
  } else if(strategy == 2) {
    box_choice <- sample(1:(2*n), 1)
  } else{
    # This vector determines which boxes a prisoner following strategy 3 will open
    # It has length n
    box_sample <- sample(1:(2*n), n, replace=FALSE)
    # We set the first box chosen - the last in the vector because of how the later loop works
    box_choice <- box_sample[n]
  }
  
  # Count variable for the number of boxes opened
  counter <- 1 
  # Round failure indicator initialised at 0 (success). This will be the variable the function returns. 
  round_failure <- 0 
  
  while(cards[box_choice]!= k){
    # For strategies 1 and 2, the boxes following the initial box is chosen depending on the card number.   
    if(strategy != 3){
      box_choice <- cards[box_choice] 
      # For strategy 3, the boxes are chosen at random. After a box is chosen, it's respective number is removed 
      # from the possible choices for the next box choice.
    } else{
      box_choice <- box_sample[counter]
    }
    # Counter is updated by 1 after each box opening
    counter <- counter + 1
    
    # If the counter exceeds the value of n, the round has failed and the prisoner has not found their number
    if(counter > n){
      # round failure set to 1 when counter exceeds the value of n. The while loop then breaks at this point. 
      round_failure <- 1
      break
    }
  }
  round_failure
}




#################### Function for Probability of Success in a Single Round ####################


##### Aim of Section #####

# Now that we have a function that indicates whether a prisoner will succeed or fail in any particular round,
# we can create a function that estimates the probability of success in a single round by running the
# indiv_round_failure() function a large number of times for different random distributions of cards.


##### Function Description #####

# The function takes as input:
# the maximum number of boxes the prisoner is permitted to open n
# the prisoner's number k
# the strategy (1,2, or 3 - see Task Description for details)
# the number of trials nreps that will be used in the simulation to estimate probability of success
#
# It returns a probability, the probability that a single prisoner will succeed in their round.


##### Function Definition #####

Pone <- function(n,k,strategy, nreps=10000){
  
  # We create a vector of the box numbers 
  boxes <- 1:(2*n)
  # And initialise a count of the number of failures out of nreps attempts.
  failures <- 0
  
  # We repeat the simulation nreps times.
  for(i in 1:nreps){
    
    # We randomly distribute the cards among the boxes
    cards <- sample(boxes,size = 2*n)
    
    # Here we call the indiv_round_failure() function defined above
    # If the prisoner fails, our count of failures ticks up one.
    if (indiv_round_failure(cards,n,k,strategy)==1){
      failures <- failures + 1
    }
    
  }
  
  # The function return the probability of success: 1 - the probability of failure.
  1-failures/nreps
  
}




#################### Function for Probability of Success for All Prisoners ####################


##### Aim of Section #####

# With some minor adjustments, we can also use the same indiv_round_failure() function
# to estimate the probability that all 2n prisoners succeed for a given n and strategy.
# If any individual prisoner fails to find their card, all 2n prisoners fail and none will be set free.


##### Function Description #####

# The function takes as input:
# the maximum number of boxes the prisoner is permitted to open n
# the strategy (1,2, or 3 - see Task Description for details)
# the number of trials nreps that will be used in the simulation to estimate probability of success
#
# It returns a probability, the probability that all 2n prisoner will succeed in finding their card.


##### Function Definition #####

Pall <- function(n, strategy, nreps=10000){
  
  # We create a vector of the box numbers 
  boxes <- 1:(2*n)
  # And initialise a count of the number of failures out of nreps attempts.
  failures <- 0
  
  # We repeat the simulation nreps times.
  for(i in 1:nreps){
    
    # We randomly distribute the cards among the boxes
    cards <- sample(boxes,size = 2*n)
    
    # We loop through each of the 2n prisoners, checking if any fail
    for(k in 1:(2*n)){
      
      # Here we call the indiv_round_failure() function defined above
      if(indiv_round_failure(cards,n,k,strategy) == 1){
        # If any individual prisoner in the 2n fails, all 2n prisoners fail and we add to the count
        failures = failures + 1
        # And break the loop for this simulation
        break
      }
    }
  }
  
  # The function return the probability of success: 1 - the probability of failure.
  1-failures/nreps
  
}




#################### Using the Functions Above to Return Probabilities ####################


##### Aim of Section #####

# We will use the functions defined above to estimate 12 probabilities:
# Single and joint probabilities of success for 
# n = 5 and n = 50 and
# strategies 1, 2 and 3.

# 2 x 2 x 3 = 12 different conbinations of inputs.

##### Creating Data Frame of Inputs #####

# We create a data frame with the inputs as required
df_prob <- data.frame(
  strategy = rep(c(1,2,3), each = 4),
  # If single = 1, we estimate the probability of success in a single round
  # If single = 0, we estimate the probability of success for all 2n prisoners
  single = rep(c(1,0), times = 6),
  n = rep(c(5,50), each = 2, times = 3),
  # The results are a blank vector for now.
  prob_success = rep(0,12)
)

# Every combination described in the aims of this sectionis represented in the data frame:
df_prob


##### Producing Results #####

# We will use set.seed() to make it easier to discuss the results in the next section.
set.seed(7)

# For each of the 12 rows we check whether we want single or joint probabilities
for( i in 1:12){
  if(df_prob$single[i] == 1){
    # Then we feed the appropriate inputs to the single or joint function as required.
    df_prob$prob_success[i] <- Pone(n = df_prob$n[i], k = 1, strategy = df_prob$strategy[i]) 
  }
  else{
    df_prob$prob_success[i] <- Pall(n = df_prob$n[i], strategy = df_prob$strategy[i])
  }
}

# Here are the results
df_prob




#################### Comments on Results Produced in Previous Section ####################


##### Aim of Section #####

# In this section we discuss the results produced in the previous section


##### Discussion #####


## Strategy 3

# Starting with strategy 3, we compare the results for a single round to the results for all prisoners when n = 5 

(strat3_n5 <- df_prob[df_prob$n == 5 & df_prob$strategy == 3,])

# Note that the probability of all 10 prisoners succeeding:

strat3_n5[strat3_n5$single==0, "prob_success"]

# is very close to the probability of a single prisoner succeeding, taken to the power of 10:

strat3_n5[strat3_n5$single==1, "prob_success"]^10

# This is to be expected since the probability of success for each prisoner is independent 
# of the probability of success for the others, under strategy 3.


## Strategy 2

# The same is true of strategy 2:
(strat2_n5 <- df_prob[df_prob$n == 5 & df_prob$strategy == 2,])

# The probability of all 10 prisoners succeeding:

strat2_n5[strat2_n5$single==0, "prob_success"]

# The probability of a single prisoner succeeding, taken to the power of 10:

strat2_n5[strat2_n5$single==1, "prob_success"]^10

# This suggests the same independence between the prisoners that we say under strategy 3.


## When n = 50 for strategy 2 and 3

# This explains why the probability for success when n = 50 for all prisoners under strategies 2 and 3 is
# so small that R renders it as 0:

df_prob[df_prob$n == 50 & df_prob$strategy != 1,]

# Even with a reasonable chance for each prisoner, taking this figure to the power of 100 results in a
# vanishingly small chance of all 100 prisoners succeeding.


## Strategy 1

# Do we see the same pattern for strategy 1? Here are the results when n = 5.
(strat1_n5 <- df_prob[df_prob$n == 5 & df_prob$strategy == 1,])

# Under strategy 1 each prisoner has about a 50% chance of succeeding, but all 10 prisoners have 
# about a 35% chance of succeeding. Much higher than would be expected if the prisoners were independent in 
# terms of their chance of success.

# The pattern is even more striking when n = 50
(strat1_n50 <- df_prob[df_prob$n == 50 & df_prob$strategy == 1,])

# If the prisoners' chances were independent we would expect the probability for all to succeed to be functionally 0:
strat1_n50[strat1_n50$single==1, "prob_success"]^100

# Instead they are about 31.5%:
strat1_n50[strat1_n50$single==0, "prob_success"]

# We conclude that for strategy 1 alone, the probability that an individual prisoner succeeds is not independent
# of the probability that the others succeeds. It is initially surprising that strategy 1 performs so well even compared
# to strategy 2, which is identical apart from the initial choice of the box.

# What explains the superior performance of strategy 1? We explore that in the next section.




####################  Function to Indicate Success in a Single Round ####################


##### Aim of Section #####

# In order to explain the success of strategy 1, we need to introduce the concepts of loops. Imagine a prisoner following
# strategy 1 - opening first the box with their number on it, then the box with the number of the card inside the first box,
# and so on. 
# If they come across their own number after opening m boxes, we say we have found a loop of length m.
# It is a loop because if they did not stop after finding their card, the next box they would open would be the first one they opened.
# If they continued in this fashion, they would go on in a circle, or a loop.

# The success of strategy 1 depends on the fact that any distribution of integers 1:2n can be decomposed in to some
# number of loops, such that every number is contained in one loop, and no number is contained in more than one loop.

# All 2n prisoners will succeed if and only if the longest loop in the distribution of cards in question is at most length 50.

# In order to explore this further, we write a function to estimate by simulation
# the probability that each loop length (1 to 2n) appears at least once for a random distribution of the cards among the boxes.


##### Function Description #####

#The function dloop() takes input as an integer n, used to denote number of boxes. and another 
#integer nreps used for number of replicate simulations.
#
# It returns a vector of length 2n, where the jth element of this vector estimates the probability of at least one loop
# of length j in a distribution of 2n cards.
#
#The function first create a vector for store the data for every simulation
#
#Next start simulation, at every start of simulations, create a vector to store data for
#present simulation.Then generate random cards and boxes with input integers.
#
#Then use while-loop at each elements to check whether there exist loop, in this procedure
#use a counter to take notes on the length of this loop. after the loop for present element
#is end. Set element on result store vector to 1.
#
#After all elements in one simulation is finished. Store result data to the vector we create
#at start of the function.
#
#Repeat as nreps times.


##### Function Definition #####

dloop <- function(n, nreps) {
  #create vector to store data of every simulation
  final_result <- rep(0, 2 * n)
  #using for-loop to simulate replicate nrep times
  for (i in 1:nreps) {
    #create vector to store data of present simulation
    result <- rep(0, 2 * n)
    #generate 2n random boxes
    u <- sample(1:(2 * n), 2 * n)
    #use another for-loop for finding loops for present element
    for (k in u) {
      #set default loop length one, which is the probable smallest loop length
      loop_len <- 1
      #record the start of present loop
      initial <- k
      #use while-loop to find the loop length
      while (u[k] != initial & u[k] != 0) {
        #if u[k] is not equal to k, then we seek for next one and plus 1 on loop length
        loop_len <- loop_len + 1
        k <- u[k]
      }
      #if the while loop was break, which means we already found a loop with length
      #of loop_len, record it on vector which store present simulation, and continue 
      #on next element to find another loop in this simulation.
      # We assign 1 to the correct element of the loop length vector because
      # we are interested in the probability of *at least one* loop of that length
      # The probability should not increase if there is more than one loop identified.
      result[loop_len] <- 1
    }
    #after one simulation is finished, record result vector to final_result vector
    final_result <- final_result + result
  }
  #return the probability vector
  final_result/nreps
}




#################### Estimate Probability that 100 Prisoners All Succeed ####################


##### Aim of Section #####

# In this section we use the dloop() function to estimate the probability that all 100 prisoners succeed, when 
# n = 50 and they follow strategy 1.

# Above, we mentioned that all 100 prisoners will succeed if and only if there is no loop longer than length 50 
# in the distribution of cards

##### Probability of All 100 Prisoners Succeeding #####


#Here we use the previous function dloop() to create a vector of probabilities for each loop length
fin <- dloop(50, 10000)


#create a variable to store the probability of occurrence of loop with length larger than 50
sum <- sum(fin[51:100])


#use 1 minus sum is the probability we seek for
(prob_success <- 1-sum)  


##### Visualising the Loop Length Probabilities #####

# We'll plot a bar chart of 100 bars, one bar for the probability that a loop of that length exists

# We specify the colour of the bars - 50 blue 50 red
cols <-  rep(c("light blue","red"),each = 50)

# We plot 
plot(fin,
     main = 'Distribution of Loop Lengths for n = 50, nrep = 10000',
     col = cols,
     type="h",
     xlab = 'Length of loop',
     ylab = 'Probability of existence')
legend("topright", 
       legend = c("loop length less than or equal to 50", "loop length more than 50"), 
       fill = c("light blue", "red"))





