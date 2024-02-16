test_that("multi_vlmc results are identical to vlmc ones for a single dts", {
  withr::local_seed(0)
  for (k in 1:9) {
    x <- sample(0:k, 1000 + 100 * k, replace = TRUE)
    svlmc <- vlmc(x, alpha = 0.1, max_depth = 15)
    mvlmc <- multi_vlmc(list(x), alpha = 0.1, max_depth = 15)
    expect_true(compare_ctx(
      contexts(mvlmc),
      contexts(svlmc)
    ))
  }
})
