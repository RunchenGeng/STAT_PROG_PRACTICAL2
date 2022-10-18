u <- c(1,3,2,6,4,5,8,9,10,7,12,11)
#this is for test, which has 1 one-loop, 2 two-loop, 1 three-loop and 1 four-loop
result <- rep(0,7)
#used to store result
for (k in u) {
  
  loop_len <- 1
  initial <- k
  #we generate or say re-generate of start value of loop at start of each for-loop
  while (k != 0) {
  #for check, not sure if we need to add this line 
    while(u[k] != initial){
    
    loop_len <- loop_len + 1
    
    k <- u[k]
    }    
  result[loop_len] <- 1
  #our aim is to count whether the loop with index length occurred so i set it as one robustly
  break
}
}
result