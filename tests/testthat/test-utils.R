test_that("conversion to discrete time series", {
  expect_error(to_dts(list(r = 1:4)))
  expect_error(to_dts(NULL))
  expect_error(to_dts(1:4, vals = 1:3))
})

test_that("signif_null handles NULL and NA properly", {
  expect_equal(signif_null(NULL, 5), "NA")
  expect_equal(signif_null(NA, 3), "NA")
})

test_that("signif_null mimics signif", {
  for (k in 1:10) {
    x <- rnorm(1)
    expect_equal(signif_null(x, k), signif(x, k))
  }
})

test_that("pp_mat obeys its contract", {
  withr::local_seed(0)
  mm <- matrix(rnorm(15), ncol = 5)
  mm_vec <- as.vector(mm)
  for (k in 2:4) {
    mm_pp <- pp_mat(mm, k)
    expect_equal(length(mm_pp), nrow(mm))
    mwidth <- 0
    for (i in seq_along(mm_vec)) {
      mwidth <- max(mwidth, stringr::str_length(signif(mm_vec[i], k)))
    }
    expect_true(all(stringr::str_length(mm_pp) <= ncol(mm) * (1 + mwidth) - 1))
  }
})

test_that("KL based criterion is computed identical in R and C++", {
  withr::local_seed(0)
  for (k in 1:10) {
    all_equal <- TRUE
    for (rep in 1:10) {
      a_counts <- sample(0:1000, k + 2, replace = TRUE)
      b_counts <- sample(0:1000, k + 2, replace = TRUE)
      p_counts <- pmax(a_counts, b_counts)
      c_counts <- pmin(a_counts, b_counts)
      if (!all.equal(
        kl_div(c_counts / sum(c_counts), p_counts / sum(p_counts)),
        kl_crit(c_counts, p_counts) / sum(c_counts)
      )) {
        all_equal <- FALSE
        break
      }
      expect_true(all_equal)
    }
  }
})
