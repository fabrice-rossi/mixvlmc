test_that("multi_ctx_free finds correct contexts in basic cases", {
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

test_that("multi_ctx_free obeys its basic contract", {
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
  }
})

test_that("multi_ctx_free finds correct contexts in more complex cases", {
  withr::local_seed(0)
  nb_dts <- 10L
  dts_bsize <- 20L
  mdts <- vector(mode = "list", length = nb_dts)
  for (k in seq_along(mdts)) {
    mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
  }
  mctx <- multi_ctx_tree(mdts, min_size = 2, max_depth = 4)
  ## check that each context is indeed present with the correct f_by
  mctx_ctx <- contexts(mctx, frequency = "detailed")
  for (k in seq_along(mctx_ctx$context)) {
    expect_equal(
      as.integer(mctx_ctx[k, 3:4]),
      multi_count_f_by(mdts, mctx_ctx$context[[k]], states(mctx))
    )
  }
})

test_that("multi_ctx_free finds all contexts", {
  withr::local_seed(42)
  nb_dts <- 10L
  dts_bsize <- 10L
  mdts <- vector(mode = "list", length = nb_dts)
  for (k in seq_along(mdts)) {
    mdts[[k]] <- sample(c(1L, 2L), dts_bsize + sample(1:5, 1), replace = TRUE)
  }
  mctx <- multi_ctx_tree(mdts, min_size = 2, max_depth = 10)
  m_ctxs <- contexts(mctx, sequence = TRUE)$context
  ## any context found in individual sequences must appear at least as the
  ## suffix of a context in the tree
  for (k in seq_along(mdts)) {
    base_ctx_tree <- ctx_tree(mdts[[k]], min_size = 1, max_depth = 10)
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
