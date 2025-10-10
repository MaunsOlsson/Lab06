context("knapsack_dynamic")

data("knapsack_objects")

test_that("Correct object is returned", {
  expect_silent(gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 16770)
  expect_true(all(round(gk$elements) %in% c(5, 8)))

  gk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 16770)
  expect_true(all(round(gk$elements) %in% c(5, 8)))

  gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  gk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))

  gk <- knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)
  expect_gte(round(gk$value), 192647)

  gk <- knapsack_dynamic(x = knapsack_objects[1:1200,], W = 3500)
  expect_gte(round(gk$value), 270290)
})
