test_that("the C++ context algorithm selects the same PST as the R implementation in a specific case", {
  withr::local_seed(0)
  x <- sample(c("A", "B", "C"), 10000, replace = TRUE)
  model <- vlmc(x, alpha = 0.05, backend = "C++")
  r_model <- vlmc(x, alpha = 0.05)
  ctx_cpp <- contexts(model)
  ctx_r <- contexts(r_model)
  expect_equal(length(ctx_cpp), length(ctx_r))
  if (length(ctx_cpp) != length(ctx_r)) {
    ## no need to match in this case
    return()
  }
  all_valid <- TRUE
  for (l in seq_along(ctx_cpp)) {
    all_valid <- Position(\(x) identical(x, ctx_cpp[[l]]), ctx_r, nomatch = 0) > 0
    if (!all_valid) {
      break
    }
  }
  expect_true(all_valid)
})

test_that("post pruning is equivalent to direct pruning (alpha, C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1, backend = "C++")
  r_model <- vlmc(dts, alpha = 0.1)
  cut_off <- cutoff(r_model)
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, alpha = cut_off[k])
    direct_model <- vlmc(dts, alpha = cut_off[k], backend = "C++")
    expect_true(compare_vlmc(pruned_model, direct_model))
  }
})

test_that("post pruning is equivalent to direct pruning (cutoff, C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1, backend = "C++")
  r_model <- vlmc(dts, alpha = 0.1)
  cut_off <- cutoff(r_model, mode = "native")
  for (k in seq_along(cut_off)) {
    pruned_model <- prune(model, cutoff = cut_off[k])
    direct_model <- vlmc(dts, cutoff = cut_off[k], backend = "C++")
    expect_true(compare_vlmc(pruned_model, direct_model))
  }
})

test_that("alpha pruning is equivalent to cutoff pruning (C++)", {
  pc <- powerconsumption[powerconsumption$week == 5, ]
  dts <- cut(pc$active_power, breaks = c(0, quantile(pc$active_power, probs = c(0.25, 0.5, 0.75, 1))))
  model <- vlmc(dts, alpha = 0.1, backend = "C++")
  r_model <- vlmc(dts, alpha = 0.1)
  c_cut_off <- cutoff(r_model, mode = "native")
  a_cut_off <- cutoff(r_model, mode = "quantile")
  expect_length(a_cut_off, length(c_cut_off))
  for (k in seq_along(a_cut_off)) {
    a_pruned_model <- prune(model, alpha = a_cut_off[k])
    c_pruned_model <- prune(model, cutoff = c_cut_off[k])
    expect_true(compare_vlmc(a_pruned_model, c_pruned_model))
  }
})
