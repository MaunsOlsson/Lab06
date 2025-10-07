knapsack_dynamic <- function(x, W) {
  stopifnot(is.data.frame(x))
  stopifnot(all(c("w", "v") %in% colnames(x)))
  stopifnot(is.numeric(x$w) & is.numeric(x$v))
  stopifnot(all(x$w > 0) & all(x$v > 0))
  stopifnot(is.numeric(W))
  stopifnot(W > 0)




}
