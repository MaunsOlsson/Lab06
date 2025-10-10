#' @title knapsack_objects
#' @usage data(knapsack_objects)
#' @description This contains an example dataset that is used to create and test the lab.
#'
#' @format A data frame with columns v and w.
#' \describe{
#' Each row describes an object with a value (v) and weight (w).
#' }
#' @source This data comes out of this code:
#' \code{suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
#' set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
#' n <- 2000
#' knapsack_objects <- data.frame(
#' w=sample(1:4000, size = n, replace = TRUE),
#' v=runif(n = n, 0, 10000)
#' )}
#'

"knapsack_objects"
