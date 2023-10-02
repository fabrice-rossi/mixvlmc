test_that("find_sequence finds actual sequences", {
  dts <- c(rep(0:2, 3), rep(2:0, 3))
  dts_ctree <- ctx_tree(dts, min_size = 2)
  expect_null(find_sequence(dts_ctree, c(1L, 1L)))
  for (i in 0:2) {
    expect_false(is.null(find_sequence(dts_ctree, 0:i)))
  }
  ## empty context
  expect_false(is.null(find_sequence(dts_ctree, list())))
})

test_that("find_sequence finds all contexts", {
  dts <- c(rep(0:2, 3), rep(2:0, 3))
  dts_ctree <- ctx_tree(dts, min_size = 2)
  ## for a data.frame output to avoid the use of the context class!
  ctx <- contexts(dts_ctree, type="data.frame", frequency = "detailed")
  for (k in 1:nrow(ctx)) {
    the_ctx <- find_sequence(dts_ctree, ctx$context[[k]])
    expect_false(is.null(the_ctx))
    expect_identical(as_sequence(the_ctx), ctx$context[[k]])
  }
})

test_that("find_sequence finds all subsequences", {
  dts <- sample(letters[1:4], 100, replace = TRUE)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 5)
  all_ok <- TRUE
  for (k in 1:(length(dts) - 2)) {
    for (l in 0:(min(4, length(dts) - 1 - k))) {
      the_ctx <- dts[k:(k + l)]
      the_match <- find_sequence(dts_ctree, the_ctx, reverse = FALSE)
      if (is.null(the_match)) {
        all_ok <- FALSE
        break
      }
    }
    if (!all_ok) {
      break
    }
  }
  expect_true(all_ok)
})

test_that("context objects are printed as expected", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
  expect_snapshot(print(find_sequence(dts_ctree, c(0, 0))))
  expect_snapshot(print(find_sequence(dts_ctree, c(0, 1))))
})

test_that("counts are correctly reported", {
  withr::local_seed(0)
  dts <- sample(1:5, 100, replace = TRUE)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 4)
  all_ok <- TRUE
  for (k in 1:(length(dts) - 2)) {
    for (l in 0:(min(3, length(dts) - 1 - k))) {
      the_ctx <- dts[k:(k + l)]
      the_match <- find_sequence(dts_ctree, the_ctx, reverse = FALSE)
      if (is.null(the_match)) {
        all_ok <- FALSE
        break
      }
      the_counts <- counts(the_match)
      my_total <- sum(the_counts[, -1])
      if (the_counts$total != my_total || counts(the_match, "total") != my_total) {
        all_ok <- FALSE
        break
      }
      slow_counts <- count_f_by(dts, the_ctx, 1:5)
      if (!all.equal(as.integer(the_counts[, -1]), slow_counts)) {
        all_ok <- FALSE
        break
      }
    }
    if (!all_ok) {
      break
    }
  }
  expect_true(all_ok)
})

test_that("positions are correctly reported", {
  withr::local_seed(0)
  dts <- sample(1:5, 100, replace = TRUE)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 4)
  all_ok <- TRUE
  for (k in 1:(length(dts) - 2)) {
    for (l in 0:(min(3, length(dts) - 1 - k))) {
      the_ctx <- dts[k:(k + l)]
      the_match <- find_sequence(dts_ctree, the_ctx, reverse = FALSE)
      the_positions <- positions(the_match)
      slow_positions <- find_occurrences(dts, the_ctx)
      if (!identical(slow_positions + l, the_positions)) {
        all_ok <- FALSE
        break
      }
    }
    if (!all_ok) {
      break
    }
  }
  expect_true(all_ok)
})

test_that("node nature is correctly reported", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 3)
  ## 0, 0 is a context but 0, 1 is not
  expect_true(is_context(find_sequence(dts_ctree, c(0, 0), reverse = TRUE)))
  expect_false(is_context(find_sequence(dts_ctree, c(0, 1), reverse = TRUE)))
  ## 1, 0, 0 is a context, but 1 is not
  expect_true(is_context(find_sequence(dts_ctree, c(1, 0, 1), reverse = TRUE)))
  expect_false(is_context(find_sequence(dts_ctree, 1, reverse = TRUE)))
})
