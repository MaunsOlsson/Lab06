

knapsack_greedy <- function(x, W){
  object <- data.frame(v = x$v, w = x$w, div = x$v/x$w)
  object <- object[order(object$div, decreasing = TRUE), ]
  i <- 1
  vec <- c()
  repeat{
    cumsum <- sum(object$v[1:i])
    vec <- c(vec, as.numeric(rownames(object)[i]))
    if(sum(object$w[1:(i+1)])> W){break}
    i <- i + 1
  }
  output <- list(value = cumsum, elements = vec)
  output
}

knapsack_greedy(x = knapsack_objects[1:800, ], W = 3500)
