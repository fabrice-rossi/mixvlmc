test_that("loglikelihood computes the expected values", {
  withr::local_seed(12)
  for (rep in 1:10) {
    nb_dts <- 15L
    dts_bsize <- 30L
    mdts <- vector(mode = "list", length = nb_dts)
    test_mdts <- vector(mode = "list", length = nb_dts)
    for (k in seq_along(mdts)) {
      mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
      test_mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
    }
    mctx <- multi_vlmc(mdts, min_size = 2, max_depth = 6, alpha = 0.1)
    for (ll in c("truncated", "specific", "extended")) {
      expect_equal(
        loglikelihood(mctx, mdts, initial = ll),
        loglikelihood_multi(mctx, mdts, initial = ll)
      )
      expect_equal(
        loglikelihood(mctx, test_mdts, initial = ll),
        loglikelihood_multi(mctx, test_mdts, initial = ll)
      )
    }
  }
})

test_that("loglikelihood is consistent", {
  withr::local_seed(12)
  for (rep in 1:10) {
    nb_dts <- 15L
    dts_bsize <- 30L
    mdts <- vector(mode = "list", length = nb_dts)
    for (k in seq_along(mdts)) {
      mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
    }
    mctx <- multi_vlmc(mdts, min_size = 2, max_depth = 6, alpha = 0.1)
    for (ll in c("truncated", "specific", "extended")) {
      expect_equal(
        loglikelihood(mctx, mdts, initial = ll),
        loglikelihood(mctx, initial = ll)
      )
    }
  }
})
