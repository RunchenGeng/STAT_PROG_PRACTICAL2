indiv_round_failure <- function(cards,n,k,strategy){
  # An if statement to determine the selection of the initial box depending on the strategy
  if(strategy == 1){
  # The number of the first box for strategy 1 corresponds to the prisoners number
    box_choice <- k 
  } else if(strategy == 2) {
  # The number of the first box for strategy 2 and 3 is random. Box_choice is a number 
  # sampled from values 1 to 2n
    box_choice <- sample(1:(2*n), 1, replace=FALSE)
  } else if(strategy == 3) {
    box_choice <- sample(1:(2*n), 1, replace=FALSE)
  }  
  # Count number of boxes opened
  counter <- 1 
  # Round failure count set to 0 
  round_failure <- 0 
  # A variable assigned to a sample of size 2n ranging from values 1 to 2n. This represents the number of boxes.
  box_sample <- sample(1:(2*n), 2*n, replace=FALSE) 
  
  while(cards[box_choice]!= k){
    # For strategies 1 and 2, the boxes following the initial box is chosen depending on the card number.   
    if(strategy == 1 || 2){
      box_choice <- cards[box_choice] 
    
    # For strategy 3, the boxes are chosen at random. After a box is chosen, it's respective number is removed 
    # from the possible choices for the next box choice.
    } else if(strategy == 3){
      box_choice <- sample(box_sample, 1, replace=FALSE) 
      box_sample <- box_sample[-box_choice]
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


######## Pone ######## 

# Pone is a function to estimate the probability of a single prisoner finding their number. 

Pone <- function(n,k,strategy, nreps=10000){
  
  boxes <- 1:(2*n)
  failures <- 0
  
  for(i in 1:nreps){
    
    cards <- sample(boxes,size = 2*n)
    
    if (indiv_round_failure(cards,n,k,strategy)==1){
      failures <- failures + 1
    }
    
  }
  
  1-failures/nreps
  
}


######## Pall ########

# Pall is a function that estimates the probability of all prisoners finding their number and thus
# all prisoners going free. 

Pall <- function(n, strategy, nreps=10000){
  
  boxes <- 1:(2*n)
  failures <- 0
  
  for(i in 1:nreps){
    
    cards <- sample(boxes,size = 2*n)
    
    for(k in 1:(2*n)){
      
      if(indiv_round_failure(cards,n,k,strategy) == 1){
        failures = failures + 1
        break # break for loop if any individual fails
      }
    }
  }
  
  1-failures/nreps
  
}

# The series of functions below produce outputs showing the probability of a single prisoner
# succeeding in finding their number for each strategy (1, 2 or 3) and for different values of n (5, 50). 
Pone(50,1, 1)
Pone(50,1, 2)
Pone(50,1, 3)
Pone(5,1, 1) 
Pone(5,1, 2)
Pone(5,1, 3)

# The series of functions below produce outputs showing the probability of all prisoners 
# succeeding in finding their number for each strategy (1, 2 or 3) and for different values of n (5, 10)
Pall(50, 1)
Pall(50, 2)
Pall(50, 3)
Pall(5, 1)
Pall(5, 2)
Pall(5, 3)


 
