dloop <- function(n, nrep) {
  final_result <- rep(0, 2 * n)
  for (i in 1:nrep) {
    result <- rep(0, 2 * n)
    u <- sample(1:(2 * n), 2 * n)
    check <- u
    for (k in u) {
      loop_len <- 1
      initial <- k
      while (u[k] != initial & u[k] != 0) {
        loop_len <- loop_len + 1
        k <- u[k]
      }
      result[loop_len] <- 1
    }
    final_result <- final_result + result
  }
  final_result/nrep
}


fin <- dloop(50, 10000)

sum <- 0
for (i in 51:100) {
  
  sum <- sum + fin[i]
}
prb <- 1-sum


check <- 1/(1:100)
sumcheck <- 0
for (i in 51:100) {
  
  sumcheck <- sumcheck + check[i]
}
prbcheck <- 1-sumcheck

