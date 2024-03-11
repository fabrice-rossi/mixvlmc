test_that("multi_ctx_tree results are identical to ctx_tree ones for a single dts", {
  withr::local_seed(28)
  for (k in 1:10) {
    dts <- sample(sample(letters, k + 1), 300, replace = TRUE)
    ctx <- ctx_tree(dts, min_size = 2, max_depth = 10)
    mctx <- multi_ctx_tree(list(dts), min_size = 2, max_depth = 10)
    expect_true(compare_ctx(contexts(ctx), contexts(mctx)))
  }
})

test_that("multi_ctx_tree implements the ctx_tree interface", {
  withr::local_seed(21)
  nb_dts <- 20L
  dts_bsize <- 15L
  mdts <- vector(mode = "list", length = nb_dts)
  for (k in seq_along(mdts)) {
    mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
  }
  mctx <- multi_ctx_tree(mdts, min_size = 2, max_depth = 20)
  expect_snapshot(print(mctx))
  expect_snapshot(draw(mctx, frequency = "detailed"))
  expect_no_error(summary(mctx))
  expect_no_error(context_number(mctx))
  expect_no_error(depth(mctx))
  expect_no_error(states(mctx))
})

test_that("multi_ctx_tree finds correct contexts in basic cases", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  ## use twice the same dts, so that contexts are identical
  mdts <- list(dts, dts)
  ctx <- ctx_tree(dts, min_size = 1, max_depth = 4)
  mctx <- multi_ctx_tree(mdts, min_size = 1, max_depth = 4)
  expect_true(compare_ctx(contexts(ctx), contexts(mctx)))
  ctx <- ctx_tree(dts, min_size = 2, max_depth = 4)
  mctx <- multi_ctx_tree(mdts, min_size = 4, max_depth = 4)
  expect_true(compare_ctx(contexts(ctx), contexts(mctx)))
})

test_that("multi_ctx_tree obeys its basic contract", {
  withr::local_seed(5)
  nb_dts <- 10L
  dts_bsize <- 20L
  mdts <- vector(mode = "list", length = nb_dts)
  for (k in seq_along(mdts)) {
    mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
  }
  for (d in 2:6) {
    mctx <- multi_ctx_tree(mdts, min_size = 2, max_depth = d)
    expect_equal(depth(mctx), d)
    expect_equal(states(mctx), c(1L, 2L))
  }
})

test_that("multi_ctx_tree finds correct contexts in more complex cases", {
  withr::local_seed(0)
  nb_dts <- 10L
  dts_bsize <- 20L
  mdts <- vector(mode = "list", length = nb_dts)
  for (k in seq_along(mdts)) {
    mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
  }
  mctx <- multi_ctx_tree(mdts, min_size = 2, max_depth = 6)
  ## check that each context is indeed present with the correct f_by
  mctx_ctx <- contexts(mctx, frequency = "detailed")
  for (k in seq_along(mctx_ctx$context)) {
    expect_equal(
      as.integer(mctx_ctx[k, 3:4]),
      multi_count_f_by(mdts, mctx_ctx$context[[k]], states(mctx))
    )
  }
})

test_that("multi_ctx_tree finds all contexts", {
  withr::local_seed(42)
  nb_dts <- 10L
  dts_bsize <- 10L
  mdts <- vector(mode = "list", length = nb_dts)
  for (k in seq_along(mdts)) {
    mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
  }
  mctx <- multi_ctx_tree(mdts, min_size = 2, max_depth = 12)
  m_ctxs <- contexts(mctx, sequence = TRUE)$context
  ## any context found in individual sequences must appear at least as the
  ## suffix of a context in the tree
  for (k in seq_along(mdts)) {
    base_ctx_tree <- ctx_tree(mdts[[k]], min_size = 1, max_depth = 12)
    base_ctxs <- contexts(base_ctx_tree, sequence = TRUE)$context
    all_true <- TRUE
    for (l in seq_along(base_ctxs)) {
      the_f_by <- multi_count_f_by(mdts, base_ctxs[[l]], states(mctx))
      if (sum(the_f_by) >= 2) {
        pos_in <- Position(\(x) ends_with(x, base_ctxs[[l]]), m_ctxs, nomatch = 0)
        if (pos_in == 0) {
          all_true <- FALSE
          break
        }
      }
    }
    expect_true(all_true)
  }
})

test_that("multi_ctx_tree finds correct weighted contexts in basic cases", {
  withr::local_seed(123)
  dts <- sample(c(1L, 2L), 20, replace = TRUE)
  ## use twice the same dts, so that contexts are identical
  mdts <- list(dts, dts)
  ctx <- ctx_tree(dts, min_size = 1, max_depth = 4)
  the_weights <- c(0.75, 0.45)
  mctx <- multi_ctx_tree(mdts, min_size = 1, max_depth = 4, weights = the_weights)
  expect_true(compare_ctx(contexts(ctx), contexts(mctx)))
  all_mctx <- contexts(mctx, sequence = TRUE, frequency = "detailed")
  for (k in 1:nrow(all_mctx)) {
    the_f_by <- count_f_by(dts, all_mctx$context[[k]], 1:2)
    expect_equal(the_f_by * sum(the_weights), as.numeric(all_mctx[k, 3:4]))
  }
})


test_that("multi_ctx_tree finds correct positions", {
  withr::local_seed(42)
  nb_dts <- 15L
  dts_bsize <- 20L
  mdts <- vector(mode = "list", length = nb_dts)
  for (k in seq_along(mdts)) {
    mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
  }
  mctx <- multi_ctx_tree(mdts, min_size = 2, max_depth = 25, keep_position = TRUE)
  all_mctx <- contexts(mctx, positions = TRUE)
  for (k in 1:nrow(all_mctx)) {
    the_ctx <- all_mctx$context[[k]]
    all_ok <- TRUE
    for (j in seq_along(mdts)) {
      the_pos <- find_occurrences(mdts[[j]], the_ctx)
      all_ok <- all.equal(
        the_pos + length(the_ctx) - 1L,
        all_mctx$positions[[k]][[j]]
      )
    }
    expect_true(all_ok)
  }
})
