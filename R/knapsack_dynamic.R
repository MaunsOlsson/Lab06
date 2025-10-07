#' Title
#'
#' @param x
#' @param W
#'
#' @returns
#' @export
#'
#' @examples
#'
#' @source https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm
#'
knapsack_dynamic <- function(x, W) {
  stopifnot(is.data.frame(x))
  stopifnot(all(c("w", "v") %in% colnames(x)))
  stopifnot(is.numeric(x$w) & is.numeric(x$v))
  stopifnot(all(x$w > 0) & all(x$v > 0))
  stopifnot(is.numeric(W))
  stopifnot(W > 0)

  n <- nrow(x)

  m <- matrix(0, (n+1), (W+1))

  for (i in 2:(n+1)) {
    for (j in 2:(W+1)) {
      if (x$w[i-1] > j) {
        m[i, j] <- m[i-1, j]
      }
      else{
        m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i-1]] + x$v[i-1])
      }
    }

  }





  elements <- function(m, a, b) {
      if (a == 1) {
        browser()
      }
      if(m[a, b] ==  m[a-1, b]){
        elements(m[1:(a-1), ], a-1, b)
      } else {
        # Item in the best combination and its weight
        return(c(a-1, x$w[a-1]))
      }

  }

  el <- elements(m, n+1, W+1)
  reached_zero <- FALSE
  items <- c()
  A <- n+1
  B <- W+1

  while (!reached_zero) {
    el <- elements(m, A, B)
    A <- el[1] #+ 1 - 1
    B <- B - el[2]
    items <- c(items, el[1])

    if(A == 1 || B == 1){
      reached_zero == TRUE
    }

  }


  return(m[n+1, W+1])

}
