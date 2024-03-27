test_that("find_sequence finds actual sequences", {
  rdts <- c(rep(0:2, 3), rep(2:0, 3))
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    rdts_ctree <- ctx_tree(rdts, min_size = 2)
    expect_null(find_sequence(rdts_ctree, c(1L, 1L)))
    for (i in 0:2) {
      expect_false(is.null(find_sequence(rdts_ctree, i:0)))
    }
    ## empty context
    expect_false(is.null(find_sequence(rdts_ctree, list())))
  }
})

test_that("find_sequence finds all contexts", {
  rdts <- c(rep(0:2, 3), rep(2:0, 3))
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    rdts_ctree <- ctx_tree(rdts, min_size = 2)
    ## for a data.frame output to avoid the use of the context class!
    ctx <- contexts(rdts_ctree, frequency = "detailed")
    for (k in 1:nrow(ctx)) {
      the_ctx <- find_sequence(rdts_ctree, ctx$context[[k]])
      expect_false(is.null(the_ctx))
      expect_identical(as_sequence(the_ctx), ctx$context[[k]])
    }
  }
})

test_that("reversing works", {
  rdts <- c(rep(0:2, 3), rep(2:0, 3))
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)

    rdts_ctree <- ctx_tree(rdts, min_size = 2)
    a_node <- find_sequence(rdts_ctree, 0:2)
    expect_false(is_reversed(a_node))
    rev_node <- rev(a_node)
    expect_true(is_reversed(rev_node))
    drev_node <- find_sequence(rdts_ctree, rev(0:2), reverse = TRUE)
    expect_identical(rev_node, drev_node)
  }
})

test_that("find_sequence finds all subsequences", {
  withr::local_seed(2)
  rdts <- sample(letters[1:4], 100, replace = TRUE)
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 5)
    all_ok <- TRUE
    for (k in 1:(length(rdts) - 2)) {
      for (l in 0:(min(4, length(rdts) - 1 - k))) {
        the_ctx <- rdts[k:(k + l)]
        the_match <- find_sequence(rdts_ctree, the_ctx)
        if (is.null(the_match)) {
          all_ok <- FALSE
          break
        }
        rev_the_match <- find_sequence(rdts_ctree, rev(the_ctx), reverse = TRUE)
        if (!identical(as_sequence(rev_the_match), as_sequence(rev(the_match)))) {
          all_ok <- FALSE
          break
        }
      }
      if (!all_ok) {
        break
      }
    }
    expect_true(all_ok)
  }
})

test_that("context objects are printed as expected", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
  expect_snapshot(print(find_sequence(rdts_ctree, c(0, 0))))
  expect_snapshot(print(find_sequence(rdts_ctree, c(1, 0))))
  expect_snapshot(print(rev(find_sequence(rdts_ctree, c(0, 0)))))
  expect_snapshot(print(rev(find_sequence(rdts_ctree, c(1, 0)))))
})

test_that("counts are correctly reported", {
  withr::local_seed(0)
  rdts <- sample(1:5, 100, replace = TRUE)
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 4)
    all_ok <- TRUE
    for (k in 1:(length(rdts) - 2)) {
      for (l in 0:(min(3, length(rdts) - 1 - k))) {
        the_ctx <- rdts[k:(k + l)]
        the_match <- find_sequence(rdts_ctree, the_ctx)
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
        slow_counts <- count_f_by(rdts, the_ctx, 1:5)
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
  }
})

test_that("positions are correctly reported", {
  withr::local_seed(0)
  rdts <- sample(1:5, 100, replace = TRUE)
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 4)
    all_ok <- TRUE
    for (k in 1:(length(rdts) - 2)) {
      for (l in 0:(min(3, length(rdts) - 1 - k))) {
        the_ctx <- rdts[k:(k + l)]
        the_match <- find_sequence(rdts_ctree, the_ctx)
        the_positions <- positions(the_match)
        slow_positions <- find_occurrences(rdts, the_ctx)
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
  }
})

test_that("node nature is correctly reported", {
  rdts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  for (backend in c("R", "C++")) {
    withr::local_options("mixvlmc.backend" = backend)
    rdts_ctree <- ctx_tree(rdts, min_size = 1, max_depth = 3)
    ## 0, 0 is a context but 0, 1 is not
    expect_true(is_context(find_sequence(rdts_ctree, c(0, 0), reverse = TRUE)))
    expect_false(is_context(find_sequence(rdts_ctree, c(0, 1), reverse = TRUE)))
    ## 1, 0, 0 is a context, but 1 is not
    expect_true(is_context(find_sequence(rdts_ctree, c(1, 0, 1), reverse = TRUE)))
    expect_false(is_context(find_sequence(rdts_ctree, 1, reverse = TRUE)))
  }
})
