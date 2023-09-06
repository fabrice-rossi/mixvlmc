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

test_that("the semantics of final_pred is respected", {
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  d_vlmc <- vlmc(data_set, alpha = 0.1)
  for (k in 1:10) {
    new_dts <- sample(c("A", "B", "C"),
      100 + sample(50:100, 1),
      replace = TRUE
    )
    pred_w_final <- predict(d_vlmc, new_dts, final_pred = TRUE)
    pred_wo_final <- predict(d_vlmc, new_dts, final_pred = FALSE)
    expect_length(
      pred_w_final,
      length(new_dts) + 1
    )
    expect_length(
      pred_wo_final,
      length(new_dts)
    )
    expect_identical(
      pred_wo_final,
      pred_w_final[-length(pred_w_final)]
    )
    probs_pred_w_final <- predict(d_vlmc, new_dts, type = "probs", final_pred = TRUE)
    probs_pred_wo_final <- predict(d_vlmc, new_dts, type = "probs", final_pred = FALSE)
    expect_equal(
      nrow(probs_pred_w_final),
      length(new_dts) + 1
    )
    expect_equal(
      nrow(probs_pred_wo_final),
      length(new_dts)
    )
    expect_identical(
      probs_pred_wo_final,
      probs_pred_w_final[-length(pred_w_final), , drop = FALSE]
    )
  }
})
