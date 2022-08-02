test_that("contexts keep the original type of the dts", {
  dts <- sample(c(0L, 1L), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  dts_contexts <- contexts(dts_tree)
  expect_type(dts_contexts[[1]], "integer")
  dts <- sample(c(0, 1), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  dts_contexts <- contexts(dts_tree)
  expect_type(dts_contexts[[1]], "double")
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  dts_contexts <- contexts(dts_tree)
  expect_type(dts_contexts[[1]], "character")
  dts <- sample(as.factor(c("A", "B", "C")), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  dts_contexts <- contexts(dts_tree)
  expect_s3_class(dts_contexts[[1]], "factor", exact = TRUE)
  expect_equal(levels(dts_contexts[[1]]), levels(dts))
})

test_that("the context tree construction finds the specified contexts", {
  dts <- c(0, 1, 1, 1, 0, 0, 1, 0, 1, 0)
  dts_ctree <- ctx_tree(dts, min_size = 1, max_depth = 2)
  contexts <- list(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
  expect_identical(contexts(dts_ctree, reverse = TRUE), contexts)
})

test_that("contexts return the appropriate data format", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  expect_type(contexts(dts_tree), "list")
  expect_s3_class(contexts(dts_tree, type = "data.frame"), "data.frame")
})

test_that("contexts reversing reverses", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  def_ctx <- contexts(dts_tree)
  rev_ctx <- contexts(dts_tree, reverse = FALSE)
  expect_length(rev_ctx, length(def_ctx))
  for (k in seq_along(def_ctx)) {
    expect_identical(rev_ctx[[k]], rev(def_ctx[[k]]))
  }
})

test_that("context format is consistent", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  raw_ctx <- contexts(dts_tree, type = "data.frame")
  expect_named(raw_ctx, c("context"))
  freq_ctx <- contexts(dts_tree, type = "data.frame", frequency = "total")
  expect_named(freq_ctx, c("context", "freq"))
  full_ctx <- contexts(dts_tree, type = "data.frame", frequency = "detailed")
  expect_named(full_ctx, c("context", "freq", "A", "B", "C"))
})

test_that("basic count consistency", {
  dts <- sample(c("A", "B", "C"), 100, replace = TRUE)
  dts_tree <- ctx_tree(dts, max_depth = 4)
  full_ctx <- contexts(dts_tree, type = "data.frame", frequency = "detailed")
  expect_equal(nrow(full_ctx), context_number(dts_tree))
  totals <- apply(full_ctx[3:5], 1, sum)
  expect_equal(full_ctx$freq, totals)
})