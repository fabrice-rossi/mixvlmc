test_that("generate_fake_data works as expected", {
  withr::local_seed(0)
  vals <- c(0L, 1L)
  counts <- matrix(2L + sample(1:5, 20, replace = TRUE), ncol = 2)
  freq <- rowSums(counts)
  counts2 <- counts + matrix(sample(0:2, 20, replace = TRUE), ncol = 2)
  probs <- sweep(counts2, 1, rowSums(counts2), "/")
  fake_data <- generate_fake_data(freq, counts, probs, vals)
  expect_length(fake_data$response, sum(freq))
  expect_length(fake_data$predictor, sum(freq))
  global_freqs <- table(fake_data$response)
  expect_equal(as.numeric(global_freqs), colSums(counts))
})

test_that("metrics.vlmc obey its contract", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  m_metrics <- metrics(model)
  ## class
  expect_s3_class(m_metrics, "metrics.vlmc")
  ## names
  expect_true(all(c("accuracy", "conf_mat", "auc") %in% names(m_metrics)))
  ## confusion matrix
  expect_true(inherits(m_metrics$conf_mat, "table"))
  expect_equal(dim(m_metrics$conf_mat), c(length(states(model)), length(states(model))))
  expect_equal(colnames(m_metrics$conf_mat), as.character(states(model)))
  expect_equal(rownames(m_metrics$conf_mat), as.character(states(model)))
  expect_lte(sum(m_metrics$conf_mat), length(dts))
  expect_true(all(colSums(m_metrics$conf_mat) <= table(dts)))
})

test_that("metrics.vlmc objects print as expected", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  m_metrics <- metrics(model)
  expect_snapshot(print(m_metrics))
})

test_that("metrics.vlmc and predict.vlmc return consistent results", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  m_metrics <- metrics(model)
  m_predict <- predict(model, dts, final_pred = FALSE)
  predict_table <- table(m_predict, dts)
  names(dimnames(predict_table)) <- c("predicted value", "true value")
  expect_identical(m_metrics$conf_mat, predict_table)
  for (k in 2:4) {
    data_set <- build_markov_chain(500, k, seed = 3 * k)
    model <- vlmc(data_set$x, alpha = 0.5)
    m_metrics <- metrics(model)
    m_predict <- predict(model, data_set$x, final_pred = FALSE)
    predict_table <- table(m_predict, data_set$x)
    names(dimnames(predict_table)) <- c("predicted value", "true value")
    expect_identical(m_metrics$conf_mat, predict_table)
  }
})

test_that("metrics.vlmc works on real world data", {
  sun_activity <- as.factor(ifelse(sunspot.year >= median(sunspot.year), "high", "low"))
  sun_model_tune <- tune_vlmc(sun_activity)
  metrics(as_vlmc(sun_model_tune))
})
