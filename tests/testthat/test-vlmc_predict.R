test_that("vlmc predict returns the same value for zero depth model", {
  withr::local_seed(0)
  data_set <- sample(1:5, 50, replace = TRUE)
  d_vlmc <- vlmc(data_set)
  expect_equal(predict(d_vlmc, 1:5), rep(2, 6))
})

test_that("vlmc predict returns the same size matrix for different type", {
  withr::local_seed(0)
  data_set <- sample(1:5, 50, replace = TRUE)
  d_vlmc <- vlmc(data_set)
  expect_length(predict(d_vlmc, 1:5), 6L)
  expect_equal(dim(predict(d_vlmc, 1:5, type = "probs")), c(6, 5))
})

test_that("vlmc predict returns good values and number of predictions for non-zero depth model", {
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  d_vlmc <- vlmc(data_set)
  expect_equal(predict(d_vlmc, c("A", "B", "B", "A", "A")), c("A", "A", "B", "B", "A", "A"))
})

test_that("vlmc predict returns the same size matrix for different type for non-zero depth model", {
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  d_vlmc <- vlmc(data_set)
  expect_length(predict(d_vlmc, c("A", "B", "B", "A")), 5L)
  expect_equal(dim(predict(d_vlmc, c("A", "B", "B", "A"), type = "probs")), c(5, 3))
})

test_that("vlmc predict detects unadapted values in input", {
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  d_vlmc <- vlmc(data_set)
  expect_error(predict(d_vlmc, c("A", "B", "B", "D")))
  expect_error(predict(d_vlmc, c("A", "B", "B", "2")))
})
