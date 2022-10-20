#################### Question 5 - Coding the dloop() Function ####################
##### Aim of Section #####
#
#In this part, we tried to construct a function can simulate the probability of each
#loop length from 1 to 2n appeared at least once in a random shuffling cards of boxes.
#
##### Function Description #####
#The dloop() takes input as an integer n, used for denote number of boxes. and another 
#integer nrep used for number of replicate simulations
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
#Repeat as nrep times.
#
##### Function Definition #####
dloop <- function(n, nrep) {
  #create vector to store data of every simulation
  final_result <- rep(0, 2 * n)
  #using for-loop to simulate replicate nrep times
  for (i in 1:nrep) {
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
      result[loop_len] <- 1
    }
    #after one simulation is finished, record result vector to final_result vector
    final_result <- final_result + result
  }
  #return the probablity vector
  final_result/nrep
}

#################### Question 6 - Estimate For n = 50, nrep = 10000 ####################
##### Aim of Section #####
#Here we use the previous function dloop()
fin <- dloop(50, 10000)
#create a variable to store the probability of occurrance of loop with length larger than50
sum <- 0
#use for-loop to compute the probability
for (i in 51:100) {
  sum <- sum + fin[i]
}
#use 1 minus sum is the probability we seek for
prb <- 1-sum  
prb
#visualizing the probabilities
cols <-  rep(c("light blue","red"),each = 50)
plot(fin,main = 'Distribution of Loop Length for n = 50, nrep = 10000',col = cols,type="h",xlab = 'length of loops',ylab = 'probability of existence')
legend("topright", legend = c("loop length less than 50", "loop length more than 50"), fill = c("light blue", "red"))