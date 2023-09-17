test_that("vlmc predict returns the same value for zero depth model (C++)", {
  withr::local_seed(0)
  for (k in 1:5) {
    data_set <- sample(1:(k + 1), 50, replace = TRUE)
    d_vlmc <- vlmc(data_set, alpha = 1e-5, backend = "C++")
    ## make sure we are in the constant model case
    expect_equal(context_number(d_vlmc), 1L)
    expect_equal(
      predict(d_vlmc, sample(1:(k + 1), 50, replace = TRUE)),
      rep(as.numeric(which.max(table(data_set))), 51)
    )
  }
})

test_that("vlmc predict returns deterministic results (C++)", {
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  d_vlmc <- vlmc(data_set, backend = "C++")
  new_data <- sample(c("A", "B", "C"), 500, replace = TRUE)
  first_try <- predict(d_vlmc, new_data)
  second_try <- predict(d_vlmc, new_data)
  expect_identical(first_try, second_try)
})

test_that("vlmc predict handles correctly edge cases (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, min_size = 5, backend = "C++")
  for (fp in c(TRUE, FALSE)) {
    ec_predict <- predict(model, dts[0], final_pred = fp)
    expect_length(ec_predict, as.integer(fp))
    expect_type(ec_predict, typeof(dts))
    expect_s3_class(ec_predict, class(dts))
    expect_identical(levels(ec_predict), levels(dts))
    prob_ec_predict <- predict(model, dts[0], final_pred = fp, type = "probs")
    expect_equal(nrow(prob_ec_predict), as.integer(fp))
    expect_equal(ncol(prob_ec_predict), length(levels(dts)))
    expect_type(prob_ec_predict, "double")
    expect_identical(class(prob_ec_predict), c("matrix", "array"))
    expect_equal(colnames(prob_ec_predict), as.character(levels(dts)))
  }
})

test_that("vlmc predict returns probabilities (C++)", {
  for (k in 2:5) {
    data_set <- build_markov_chain(1000, k, seed = k)
    x_vlmc <- vlmc(data_set$x, backend = "C++")
    preds <- predict(x_vlmc, data_set$x[1:500], type = "probs")
    expect_equal(rowSums(preds), rep(1, nrow(preds)))
  }
})

test_that("vlmc predict returns the same size matrix for different type for non-zero depth model (C++)", {
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  d_vlmc <- vlmc(data_set, backend = "C++")
  expect_length(predict(d_vlmc, c("A", "B", "B", "A")), 5L)
  expect_equal(dim(predict(d_vlmc, c("A", "B", "B", "A"), type = "probs")), c(5, 3))
})

test_that("vlmc predict detects unadapted values in input (C++)", {
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  d_vlmc <- vlmc(data_set, backend = "C++")
  expect_error(predict(d_vlmc, c("A", "B", "B", "D")))
  expect_error(predict(d_vlmc, c("A", "B", "B", "2")))
  expect_error(predict(d_vlmc, newdata = NULL))
})

test_that("the semantics of final_pred is respected (C++)", {
  withr::local_seed(0)
  data_set <- sample(c("A", "B", "C"), 500, replace = TRUE)
  d_vlmc <- vlmc(data_set, alpha = 0.1, backend = "C++")
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

test_that("predictions do not depend off the back end", {
  for (k in 2:5) {
    data_set <- build_markov_chain(1000, k, seed = k)
    cpp_vlmc <- vlmc(data_set$x, backend = "C++")
    r_vlmc <- vlmc(data_set$x, backend = "R")
    expect_identical(predict(cpp_vlmc, data_set$x), predict(r_vlmc, data_set$x))
    expect_identical(
      predict(cpp_vlmc, data_set$x, type = "probs"),
      predict(r_vlmc, data_set$x, type = "probs")
    )
  }
})
