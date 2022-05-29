test_that("forward match respects the expected output format", {
  nb_vals <- 5
  x <- sample(0:(nb_vals - 1), 100, replace = TRUE)
  mx <- forward_match_all_ctx_counts(x, nb_vals)
  expect_equal(names(mx), c("positions", "counts"))
  expect_equal(length(mx$positions), nb_vals)
  expect_equal(dim(mx$counts), c(nb_vals, nb_vals))
})

test_that("forward match matches as documented for NULL nv_from", {
  nb_rep <- 10
  x <- rep(c(0, 1), nb_rep)
  mx <- forward_match_all_ctx_counts(x, 2)
  expect_equal(mx$positions[[1]], seq(from = 0, by = 2, length.out = nb_rep))
  expect_equal(mx$positions[[2]], seq(from = 1, by = 2, length.out = nb_rep))
  expect_equal(diag(mx$counts), c(0, 0))
  expect_equal(mx$counts[1, 2], nb_rep)
  expect_equal(mx$counts[2, 1], nb_rep - 1)
})

test_that("forward match outputs are consistent for NULL nv_from", {
  nb_vals <- 5
  x <- sample(0:(nb_vals - 1), 500, replace = TRUE)
  mx <- forward_match_all_ctx_counts(x, nb_vals)
  for (k in 1:nb_vals) {
    ## check match positions
    pos <- which(x == k - 1)
    expect_equal(mx$positions[[k]], pos - 1)
    ## check counts
    pos_next <- x[pos + 1]
    pos_next <- pos_next[!is.na(pos_next)]
    expect_equal(mx$counts[k, ], tabulate(pos_next + 1, nbins = nb_vals))
  }
})

test_that("forward match outputs are consistent for non NULL nv_from", {
  nb_vals <- 5
  x <- sample(0:(nb_vals - 1), 500, replace = TRUE)
  from <- sort(sample(2:500, 100, replace = FALSE))
  the_depth <- 1
  mx <- forward_match_all_ctx_counts(x, nb_vals, the_depth, from - 1)
  for (k in 1:nb_vals) {
    ## check match positions
    pos <- (from - 1)[which(x[from - 1] == k - 1)]
    expect_equal(mx$positions[[k]], pos - 1)
    ## check counts
    pos_next <- x[pos + 1 + the_depth]
    pos_next <- pos_next[!is.na(pos_next)]
    expect_equal(mx$counts[k, ], tabulate(pos_next + 1, nbins = nb_vals))
  }
})
