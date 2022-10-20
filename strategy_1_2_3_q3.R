indiv_round_failure <- function(cards,n,k,strategy){
  
  if(strategy == 1){
    box_choice <- k 
  } else if(strategy == 2) {
    box_choice <- sample(1:(2*n), 1, replace=FALSE)
  } else if(strategy == 3) {
    box_choice <- sample(1:(2*n), 1, replace=FALSE)
  }  
  
  counter <- 1 # count number of boxes opened
  round_failure <- 0
  box_sample <- sample(1:(2*n), 2*n, replace=FALSE)
  
  while(cards[box_choice]!= k){
    
    if(strategy == 1 || 2){
      box_choice <- cards[box_choice] 
    } else if(strategy == 3){
      box_choice <- sample(box_sample, 1, replace=FALSE) 
      box_sample <- box_sample[-box_choice]
    }
    
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


Pone(50,1, 1)
Pone(50,1, 2)
Pone(50,1, 3)
Pone(5,1, 1) 
Pone(5,1, 2)
Pone(5,1, 3)

Pall(50, 1)
Pall(50, 2)
Pall(50, 3)
Pall(5, 1)
Pall(5, 2)
Pall(5, 3)


 
