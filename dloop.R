dloop <- function(n, nrep) {
  final_result <- rep(0, 2 * n)
  #used to store result
  for (i in 1:nrep) {
    result <- rep(0, 2 * n)
    u <- sample(1:(2 * n), 2 * n)
    check <- u
    #this is for test, which has 1 one-loop, 2 two-loop, 1 three-loop and 1 four-loop
    
    for (k in u) {
      loop_len <- 1
      initial <- k
      #path <- c(k)
      #we generate or say re-generate of start value of loop at start of each for-loop
      #while (k != 0) {
      #for check, not sure if we need to add this line
      while (u[k] != initial & u[k] != 0) {
        loop_len <- loop_len + 1
        k <- u[k]
        #path <- c(path,which(u == u[k]))
        
      }
      #break
      #}
      #u[path] <- 0
      result[loop_len] <- 1
      #our aim is to count whether the loop with index length occurred so i set it as one robustly
    }
    #print(i)
    final_result <- final_result + result
  }
}
dloop(50, 10000)





