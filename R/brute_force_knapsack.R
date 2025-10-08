RNGversion(min(as.character(getRversion()),"3.5.3"))


set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                               v = runif(n = n, 0, 10000))




#' knapsack_brute_force
#'
#' @description
#' Solution to the knapsack problem that runs in O(2^n) time.
#'
#' @param x x is a dataframe containing 2 columns, positive weights and values of the objects.
#' @param W The maximum weight of the knapsack.
#' @param parallel TRUE or FALSE, decides if parallel computation should be used. Might not be faster on all computers.
#'
#' @returns The value of the best knapsack and the elements included in it.
#' @importFrom parallel parSapply parLapply
#' @export
#'
#'
#' @examples
#'
#' suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE), v = runif(n = n, 0, 10000))
#' knapsack_brute_force(x = knapsack_objects[1:8,], W = 2000)
#'
#'
knapsack_brute_force <- function(x, W, parallel = FALSE) {
  stopifnot(is.data.frame(x))
  stopifnot(all(c("w", "v") %in% colnames(x)))
  stopifnot(is.numeric(x$w) & is.numeric(x$v))
  stopifnot(all(x$w > 0) & all(x$v > 0))
  stopifnot(is.numeric(W))
  stopifnot(W > 0)


  n <- nrow(x)
  max_alt <- 2^n - 1

  if (parallel) {

    cores <- parallel::detectCores()
    cl <- makeCluster(cores, type = "PSOCK")

    availComb <- parLapply(cl = cl, 1:max_alt, function(i){
      bits <- as.integer(intToBits(i)[1:n])
      list(value = sum(x$v * bits),
           weight = sum(x$w * bits),
           elements = as.integer(rownames(x))[as.logical(bits)]
      )
    })



    filtered_values <- parSapply(cl = cl, availComb, function(i){
      if(i$weight < W){
        return(i$value)
      }
      return(0)})

    stopCluster(cl)

  } else {

    availComb <- lapply(1:max_alt, function(i){
      bits <- as.integer(intToBits(i)[1:n])
      list(value = sum(x$v * bits),
           weight = sum(x$w * bits),
           elements = as.integer(rownames(x))[as.logical(bits)]
      )
    })
    filtered_values <- sapply(availComb, function(i){
      if(i$weight < W){
        return(i$value)
      }
      return(0)})

  }






  return(list(value = availComb[[which.max(filtered_values)]]$value,
              elements = availComb[[which.max(filtered_values)]]$elements))
}

start.time <- Sys.time()
knapsack_brute_force(x = knapsack_objects[1:20,], W = 2000, parallel = TRUE)
end.time <- Sys.time()
end.time - start.time

start.time <- Sys.time()
knapsack_brute_force(x = knapsack_objects[1:20,], W = 2000, parallel = FALSE)
end.time <- Sys.time()
end.time - start.time
