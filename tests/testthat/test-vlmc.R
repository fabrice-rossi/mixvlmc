test_that("vlmc estimation works on super simple case", {
  x <- rep(c(0, 1), 1000)
  x_vlmc <- vlmc(x)
  expect_identical(depth(x_vlmc), 1)
  expect_identical(context_number(x_vlmc), 2)
  x_vlmc_ctx <- contexts(x_vlmc)
  expect_identical(length(x_vlmc_ctx), 2L)
  expect_identical(x_vlmc_ctx[[1]], 0)
  expect_identical(x_vlmc_ctx[[2]], 1)
})

test_that("post pruning is equivalent to direct pruning (alpha)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  cut_off <- cutoff(model)
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, alpha = cut_off[k])
    direct_model <- vlmc(dts, alpha = cut_off[k])
    expect_equal(pruned_model, direct_model)
  }
})

test_that("post pruning is equivalent to direct pruning (cutoff)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  cut_off <- cutoff(model, mode = "native")
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, cutoff = cut_off[k])
    direct_model <- vlmc(dts, cutoff = cut_off[k])
    expect_equal(pruned_model, direct_model)
  }
})

test_that("alpha pruning is equivalent to cutoff pruning", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1)
  c_cut_off <- cutoff(model, mode = "native")
  a_cut_off <- cutoff(model, mode = "quantile")
  expect_length(a_cut_off, length(c_cut_off))
  for (k in seq_along(a_cut_off)) {
    a_pruned_model <- prune(model, alpha = a_cut_off[k])
    c_pruned_model <- prune(model, cutoff = c_cut_off[k])
    expect_equal(a_pruned_model, c_pruned_model)
  }
})
