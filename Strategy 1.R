

#####################################

# This function is used in both Pone and Pall

# It checks whether an individual prisoner fails to find their number
# It returns 0 if they succeed, 1 if they fail

# Before looking at it in detail, more useful to first see how it is used in Pone and Pall.

# It currently only works for strategy 3
# Modifications required for the other two strategies are indicated with comments

# Modification can be done by adding another input called strategy (1,2, or 3) and
# using if else statements on the appropriate lines

indiv_round_failure <- function(cards,n,k){
  
  box_choice <- k # This needs to be modified if strategy is 2 or 3
  counter <- 1 # count number of boxes opened
  round_failure <- 0
  
  while(cards[box_choice]!= k){
    
    box_choice <- cards[box_choice] # This needs to be modified if strategy is 3
    
    counter <- counter + 1
    
    # Check if counter has gone over n
    if(counter > n){
      #print("Failed")
      round_failure <- 1
      break
    }
  }
  round_failure
}



######## Pone ######## 

Pone <- function(n,k,nreps){
  
  boxes <- 1:(2*n)
  failures <- 0
  
  for(i in 1:nreps){
    
    cards <- sample(boxes,size = 2*n)
    
    if (indiv_round_failure(cards,n,k)==1){
      failures <- failures + 1
    }
    
  }
  
  1-failures/nreps
  
}


######## Pall ########


Pall <- function(n,nreps){
  
  boxes <- 1:(2*n)
  failures <- 0
  
  for(i in 1:nreps){
    
    cards <- sample(boxes,size = 2*n)
    
    for(k in 1:(2*n)){
      
      if(indiv_round_failure(cards,n,k) == 1){
        failures = failures + 1
        break # break for loop if any individual fails
      }
    }
  }
  
  1-failures/nreps
  
}


Pone(50,1,10000)

Pall(50,10000)
