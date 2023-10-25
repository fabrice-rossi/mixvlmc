test_that("cutoff.ctx_node works in degenerate cases", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, backend = "C++")
  ctx <- find_sequence(model, model$vals[1])
  expect_equal(cutoff(parent(ctx)), 1)
})

test_that("ctx_node based cut off reporting is consistent with direct reporting", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, backend = "C++")
  model_ctxs <- contexts(model)
  model_ctx_df <- contexts(model, type = "data.frame", cutoff = "native")
  model_ctx_df_q <- contexts(model, type = "data.frame", cutoff = "quantile")
  for (k in seq_along(model_ctxs)) {
    expect_equal(
      cutoff(model_ctxs[[k]], scale = "native", raw = TRUE),
      model_ctx_df$cutoff[k]
    )
    expect_equal(
      cutoff(model_ctxs[[k]], scale = "quantile", raw = TRUE),
      model_ctx_df_q$cutoff[k]
    )
  }
})

test_that("ctx_node based metrics reporting is consistent with direct reporting", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, backend = "C++")
  model_ctxs <- contexts(model)
  model_ctx_df <- contexts(model, type = "data.frame", metrics = TRUE)
  for (k in seq_along(model_ctxs)) {
    ctx_metrics <- metrics(model_ctxs[[k]])
    expect_equal(ctx_metrics$accuracy, model_ctx_df$accuracy[k])
    expect_equal(ctx_metrics$auc, model_ctx_df$auc[k])
  }
})
