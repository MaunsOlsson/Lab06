RNGversion(min(as.character(getRversion()),"3.5.3"))


set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                               v = runif(n = n, 0, 10000))




#' brute_force_knapsack
#'
#' @param x x is a dataframe containing 2 columns, positive weights and values of the objects.
#' @param W The maximum weight of the knapsack.
#'
#' @returns
#' @export
#'
#' @examples
brute_force_knapsack <- function(x, W) {
  stopifnot(is.data.frame(x))
  stopifnot(all(c("w", "v") %in% colnames(x)))
  stopifnot(is.numeric(x$w) & is.numeric(x$v))
  stopifnot(all(x$w > 0) & all(x$v > 0))
  stopifnot(is.numeric(W))
  stopifnot(W > 0)
  stopifnot(W <= 2^31)

  n <- nrow(x)

  index <- combn(rep(c(TRUE, FALSE), n), n)

  test <- lapply(1:ncol(index), function(i) {
                 list( value = sum(x[index[, i], ]$v),
                       weight = sum(x[index[, i], ]$w),
                       elements = which(index[, i]))}

                 )

  filtered_values <- sapply(test, function(i){
    if(i$weight < W){

      return(i$value)
    }

    return(0)})

  return(list(value = test[[which.max(filtered_values)]]$value,
              elements = test[[which.max(filtered_values)]]$elements))

}
