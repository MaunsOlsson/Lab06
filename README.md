
# Lab06

<!-- badges: start -->
[![R-CMD-check](https://github.com/MaunsOlsson/Lab06/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MaunsOlsson/Lab06/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This repository contains all the code that are needed to solve the exercises in lab06.
This contains some knapsack algorithms.

## Installation

You can install the development version of Lab06 this way:

``` r
# install.packages("devtools")
# devtools::install_github("MaunsOlsson/Lab06", build_vignettes = TRUE)
library(Lab06)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
brute_force_knapsack(x=knapsack_objects[1:8,],W=3500)
dynamic_knapsack(x=knapsack_objects[1:8,],W=3500)
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
```

