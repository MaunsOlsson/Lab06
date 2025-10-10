#' @title knapsack_greedy
#'
#' @description
#' An approximation to the knapsack problem that runs in O(n log(n)) time.
#'
#'
#' @param x x is a dataframe containing 2 columns, positive weights and values of the objects.
#' @param W The maximum weight of the knapsack.
#'
#' @returns The value of the best knapsack and the elements included in it.
#' @export
#'
#' @examples
#'
#' knapsack_objects <- data("knapsack_objects")
#' knapsack_greedy(x = knapsack_objects[1:1200,], W = 2000)
#'
#'
#' @source https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm
#'

knapsack_greedy <- function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(all(c("w", "v") %in% colnames(x)))
  stopifnot(is.numeric(x$w) & is.numeric(x$v))
  stopifnot(all(x$w > 0) & all(x$v > 0))
  stopifnot(is.numeric(W))
  stopifnot(W > 0)

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

  if (object$v[i+1] > cumsum) {
    return(list(value = object$v[i+1], elements = as.numeric(rownames(object)[i + 1])))
  }

  output <- list(value = cumsum, elements = vec)
  output
}

