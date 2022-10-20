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


########################

indiv_round_failure_2 <- function(cards,n,k){
  
  
  box_choice <- sample(1:(2*n), 1, replace=FALSE) # This needs to be modified if strategy is 2 or 3
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

###################

indiv_round_failure_3 <- function(cards,n,k){
  
  
  box_choice <- sample(1:(2*n), 1, replace=FALSE) # This needs to be modified if strategy is 2 or 3
  counter <- 1 # count number of boxes opened
  round_failure <- 0
  box_sample <- sample(1:(2*n), 2*n, replace=FALSE)
  
  while(cards[box_choice]!= k){
    box_choice <- sample(box_sample, 1, replace=FALSE) 
    box_sample <- box_sample[-box_choice]
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

Pone <- function(n,k,nreps,strategy){
  
  boxes <- 1:(2*n)
  failures <- 0
  
  for(i in 1:nreps){
    
    cards <- sample(boxes,size = 2*n)
  
  if(strategy==1){
    if (indiv_round_failure(cards,n,k)==1){
      failures <- failures + 1
    }
  } else if(strategy ==2){
      if (indiv_round_failure_2(cards,n,k)==1){
        failures <- failures + 1
    }
  } else if(strategy == 3){
    if (indiv_round_failure_3(cards,n,k)==1){
       failures <- failures + 1
    }
  } 
  }
  
  1-failures/nreps
  
}


######## Pall ########


Pall <- function(n,nreps,strategy){
  
  boxes <- 1:(2*n)
  failures <- 0
  
  for(i in 1:nreps){
    
    cards <- sample(boxes,size = 2*n)
    
    for(k in 1:(2*n)){
      
      if(strategy == 1){
        if (indiv_round_failure(cards,n,k)==1){
          failures <- failures + 1
        }
      } else if(strategy ==2){
        if (indiv_round_failure_2(cards,n,k)==1){
          failures <- failures + 1
        }
      } else if(strategy == 3){
        if (indiv_round_failure_3(cards,n,k)==1){
          failures <- failures + 1
        }
      } 
        break # break for loop if any individual fails
      }
    }
    
  
  1-failures/nreps
  
}


Pone(50,1,10000, 3)

Pall(50,10000,3)
